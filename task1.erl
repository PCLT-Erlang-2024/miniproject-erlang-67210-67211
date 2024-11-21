-module(task1).

-export([main/0, conveyor_belt/1]).

-record(product, {id, size}).

-define(NUM_PRODUCTS, 20).

generate_products(Pids) ->
    F = fun() -> generate_products(Pids, 1) end,
    spawn(F).

generate_products(Pids, ?NUM_PRODUCTS) ->
    Product = #product{id = ?NUM_PRODUCTS, size = rand:uniform(5)},
    io:format("Generated product: ~p~n", [Product]),
    Pid = lists:nth(?NUM_PRODUCTS rem length(Pids) + 1, Pids),
    Pid ! {product, Product},
    [EndPid ! {stop, stop} || EndPid <- Pids];
generate_products(Pids, Id) ->
    Product = #product{id = Id, size = rand:uniform(5)},
    io:format("Generated product: ~p~n", [Product]),
    Pid = lists:nth(Id rem length(Pids) + 1, Pids),
    Pid ! {product, Product},
    timer:sleep(500),
    generate_products(Pids, Id + 1).

start_conveyor_belts() ->
    Pids = [spawn(?MODULE, conveyor_belt, [Id]) || Id <- lists:seq(1, 6)],
    Pids.

conveyor_belt(Id) ->
    receive
        {product, Product} ->
            io:format("~p :: Received product ~p~n", [Id, Product]),
            conveyor_belt(Id);
        {stop, stop} ->
            io:format("~p :: Stopping~n", [Id])
    end.

main() ->
    Pids = start_conveyor_belts(),
    generate_products(Pids).
