-module(customer).
-export([start/5]).

start(Name, Objective, LogPid, MainPid, BankList) ->
    timer:sleep(200),
    rand:seed(exsplus, {erlang:monotonic_time(), erlang:unique_integer(), erlang:system_time()}),
    loop(Name, Objective, Objective, BankList, LogPid, MainPid).

loop(Name, Objective, Remaining, BankList, LogPid, MainPid) ->
    if
        Remaining =< 0 ->
            MainPid ! {done, customer, Name, Objective, Objective},
            ok;
        BankList =:= [] ->
            MainPid ! {done, customer, Name, Objective, Objective - Remaining},
            ok;
        true ->
            timer:sleep(rand:uniform(91) + 9),
            MaxRequest = min(Remaining, 50),
            LoanAmount = rand:uniform(MaxRequest),
            Index = rand:uniform(length(BankList)),
            Bank = lists:nth(Index, BankList),
            BankPid = whereis(Bank),
            
            case BankPid of
                undefined ->
                    NewBankList = lists:delete(Bank, BankList),
                    loop(Name, Objective, Remaining, NewBankList, LogPid, MainPid);
                _ ->
                    BankPid ! {loan_request, Name, LoanAmount, self()},
                    LogPid ! {log, request, Name, Bank, LoanAmount},

                    receive
                        {loan_reply, approve, LoanAmount} ->
                            loop(Name, Objective, Remaining - LoanAmount, BankList, LogPid, MainPid);
                        {loan_reply, deny, LoanAmount} ->
                            NewBankList = lists:delete(Bank, BankList),
                            loop(Name, Objective, Remaining, NewBankList, LogPid, MainPid)
                    after 5000 ->
                        NewBankList = lists:delete(Bank, BankList),
                        loop(Name, Objective, Remaining, NewBankList, LogPid, MainPid)
                    end
            end
    end.
