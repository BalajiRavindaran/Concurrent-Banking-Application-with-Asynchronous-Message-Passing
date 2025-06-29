-module(bank).
-export([start/3]).

start(Name, Amount, Master) ->
    loop(Name, Amount, Master).

loop(Name, Balance, Master) ->
    receive
        {loan_request, CustomerName, RequestAmount, CustomerPid} ->
            if
                Balance >= RequestAmount ->
                    CustomerPid ! {loan_reply, approve, RequestAmount},
                    Master ! {log, response, Name, CustomerName, approve, RequestAmount},
                    loop(Name, Balance - RequestAmount, Master);
                true ->
                    CustomerPid ! {loan_reply, deny, RequestAmount},
                    Master ! {log, response, Name, CustomerName, deny, RequestAmount},
                    loop(Name, Balance, Master)
            end;
        {get_balance, From} ->
            From ! {balance, Name, Balance},
            loop(Name, Balance, Master)
    end.
