-module(task1).

-export([main/0, conveyor_belt/3]).

-record(product, {id, size}).
-record(truck, {id, capacity, load}).

-define(NUM_PRODUCTS, 20).
-define(NUM_CONVEYOR_BELTS, 2).

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
    Pids = [spawn(?MODULE, conveyor_belt, [Id]) || Id <- lists:seq(1, ?NUM_CONVEYOR_BELTS)],
    Pids.

conveyor_belt(Id, Truck, TPPid) ->
    receive
        {product, Product} ->
            io:format("~p :: Received product ~p~n", [Id, Product]),
            timer:sleep(2000),
            case Truck of
                null ->
                    io:format("~p :: No trucks available~n", [Id]),
                    TPPid ! {truck, please},
                    receive
                        {truck, Truck} ->
                            io:format("~p :: Received truck ~p~n", [Id, Truck]),
                            process_product(Product, Truck)
                    end;
                Truck ->
                    process_product(Product, Truck)
            end,
            conveyor_belt(Id, Truck, TPPid);
        {stop, stop} ->
            io:format("~p :: Stopping~n", [Id])
    end.

provide_truck(CBPid, Id) -> 
    receive 
        {truck, please} -> 
            io:format("Received truck request~n"),
            CBPid ! {truck, #truck{id = Id, capacity = 10, load = 0}}
    end.

process_product(Product, Truck) ->
    
    ok.


main() ->
    Pids = start_conveyor_belts(),
    generate_products(Pids).
