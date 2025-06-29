-module(money).
-export([start/1]).

start(Args) ->
    CustomerFile = lists:nth(1, Args),
    BankFile = lists:nth(2, Args),
    {ok, CustomerInfo} = file:consult(CustomerFile), 
    {ok, BankInfo} = file:consult(BankFile),

    io:format("~n------------------------- Given Info -------------------------~n"),
    io:format("Customer Info: ~p~n", [CustomerInfo]),
    io:format("Bank Info: ~p~n", [BankInfo]),
    io:format("------------------------ End of Info -------------------------~n"),

    LogPid = spawn(fun() -> log() end),
    
    MainPid = self(),
    
    BankNames = [BankName || {BankName, _} <- BankInfo],

    
    io:format("~n------------------------- Customers -------------------------~n"),
    lists:foreach(
        fun({CustName, Objective}) ->
            io:format("Starting customer ~p with objective ~p~n", [CustName, Objective]),
            spawn(customer, start, [CustName, Objective, LogPid, MainPid, BankNames])
        end,
        CustomerInfo
    ),
    
    io:format("~n------------------------- Banks -------------------------~n"),
    lists:foreach(
        fun({BankName, Amount}) ->
            io:format("Starting bank ~p with initial amount ~p~n", [BankName, Amount]),
            Pid = spawn(bank, start, [BankName, Amount, LogPid]),
            register(BankName, Pid)
        end,
        BankInfo
    ),

    io:format("~n ** The financial market is opening for the day ** ~n"),
    io:format("~n Starting transaction log... ~n~n"),
    NumCustomers = length(CustomerInfo),
    summary_loop(NumCustomers, [], BankInfo, LogPid).

log() ->
    receive
        {log, startup, BankName, Amount} ->
            io:format("Bank ~p started with initial amount: ~p~n", [BankName, Amount]),
            log();
        {log, request, Customer, Bank, Amount} ->
            io:format("? ~p requests a loan of ~p dollar(s) from the ~p bank~n",
                      [Customer, Amount, Bank]),
            log();
        {log, response, Bank, Customer, approve, Amount} ->
            io:format("$ The ~p bank approves a loan of ~p dollar(s) to ~p~n",
                      [Bank, Amount, Customer]),
            log();
        {log, response, Bank, Customer, deny, Amount} ->
            io:format("$ The ~p bank denies a loan of ~p dollar(s) to ~p~n",
                      [Bank, Amount, Customer]),
            log();
        stop ->
            ok
    end.

summary_loop(0, CustomerResults, BankInfo, LogPid) ->
    LogPid ! stop,
    timer:sleep(100),
    
    io:format("~n** Banking Report **~n~n"),
    io:format("Customers:~n"),
    lists:foreach(
        fun({Name, Objective, Received}) ->
            io:format(" ~p: objective ~p, received ~p~n", [Name, Objective, Received])
        end,
        CustomerResults
    ),
    TotalObjective = lists:sum([Obj || {_, Obj, _} <- CustomerResults]),
    TotalReceived = lists:sum([Rec || {_, _, Rec} <- CustomerResults]),
    io:format(" -----~n Total: objective ~p, received ~p~n~n", [TotalObjective, TotalReceived]),

    io:format("Banks:~n"),
    lists:foreach(
        fun({BankName, Original}) ->
            BankPid = whereis(BankName),
            BankPid ! {get_balance, self()},
            receive
                {balance, BankName, Balance} ->
                    io:format(" ~p: original ~p, balance ~p~n", [BankName, Original, Balance])
            end
        end,
        BankInfo
    ),
    TotalOriginal = lists:sum([Amt || {_, Amt} <- BankInfo]),
    io:format(" -----~n Total: original ~p, loaned ~p~n", [TotalOriginal, TotalReceived]),

    io:format("~nThe financial market is closing for the day...~n");

summary_loop(N, Acc, BankInfo, LogPid) ->
    receive
        {done, customer, Name, Objective, Received} ->
            summary_loop(N - 1, [{Name, Objective, Received} | Acc], BankInfo, LogPid)
    end.
