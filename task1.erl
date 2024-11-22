-module(task1).
-export([main/0, start_conveyor_belts/1, generate_products/2, conveyor_belt/4, start_truck_provider/0, truck_provider/1, process_product/2]).

-define(NUM_PRODUCTS, 20).
-define(NUM_CONVEYOR_BELTS, 2).

-record(truck, {id, capacity, load}).
-record(product, {id, size}).

generate_products(Pids, MainPid) ->
    F = fun() -> generate_products(Pids, 1, MainPid) end,
    spawn(F).

generate_products(Pids, ?NUM_PRODUCTS, MainPid) ->
    Product = #product{id = ?NUM_PRODUCTS, size = rand:uniform(5)},
    io:format("Generated product: ~p~n", [Product]),
    Pid = lists:nth(?NUM_PRODUCTS rem length(Pids) + 1, Pids),
    Pid ! {product, Product},
    [EndPid ! {stop, stop} || EndPid <- Pids],
    MainPid ! {all_products_generated, ?NUM_PRODUCTS};
generate_products(Pids, Id, MainPid) ->
    Product = #product{id = Id, size = rand:uniform(5)},
    io:format("Generated product: ~p~n", [Product]),
    Pid = lists:nth(Id rem length(Pids) + 1, Pids),
    Pid ! {product, Product},
    timer:sleep(500),
    generate_products(Pids, Id + 1, MainPid).

start_conveyor_belts(TPId) ->
    Pids = [spawn(?MODULE, conveyor_belt, [Id, {waiting_for_product, null}, TPId, self()]) || Id <- lists:seq(1, ?NUM_CONVEYOR_BELTS)],
    Pids.

conveyor_belt(Id, State, TPPid, MainPid) ->
    receive
        {product, Product} ->
            handle_product(Id, Product, State, TPPid, MainPid);
        {truck, Truck} ->
            handle_truck(Id, Truck, State, TPPid, MainPid);
        {stop, stop} ->
            io:format("~p :: Stopping~n", [Id])
    end.

handle_product(Id, Product, {waiting_for_product, Truck}, TPPid, MainPid) when Truck =/= null ->
    io:format("~p :: Processing product ~p with truck ~p~n", [Id, Product, Truck]),
    case process_product(Product, Truck) of
        {ok, UpdatedTruck} ->
            MainPid ! {product_processed, Id},
            conveyor_belt(Id, {waiting_for_product, UpdatedTruck}, TPPid, MainPid);
        {error, RemainingProduct} ->
            io:format("~p :: Truck capacity exceeded, requesting new truck~n", [Id]),
            request_and_receive_truck(Id, RemainingProduct, TPPid, MainPid)
    end;
handle_product(Id, Product, {waiting_for_product, _}, TPPid, MainPid) ->
    io:format("~p :: Received product ~p, waiting for truck~n", [Id, Product]),
    request_and_receive_truck(Id, Product, TPPid, MainPid).

handle_truck(Id, Truck, {waiting_for_truck, Product}, TPPid, MainPid) ->
    io:format("~p :: Processing product ~p with truck ~p~n", [Id, Product, Truck]),
    case process_product(Product, Truck) of
        {ok, UpdatedTruck} ->
            MainPid ! {product_processed, Id},
            conveyor_belt(Id, {waiting_for_product, UpdatedTruck}, TPPid, MainPid);
        {error, RemainingProduct} ->
            io:format("~p :: Truck capacity exceeded, requesting new truck~n", [Id]),
            request_and_receive_truck(Id, RemainingProduct, TPPid, MainPid)
    end;
handle_truck(Id, Truck, {waiting_for_product, _}, TPPid, MainPid) ->
    io:format("~p :: Received truck ~p, waiting for product~n", [Id, Truck]),
    conveyor_belt(Id, {waiting_for_truck, Truck}, TPPid, MainPid).

request_and_receive_truck(Id, Product, TPPid, MainPid) ->
    TPPid ! {self(), truck},
    io:format("~p :: Requested truck from ~p~n", [Id, TPPid]),
    receive
        {truck, Truck} ->
            io:format("~p :: Received truck ~p~n", [Id, Truck]),
            handle_product(Id, Product, {waiting_for_product, Truck}, TPPid, MainPid)
    end.

start_truck_provider() ->
    TPId = spawn(?MODULE, truck_provider, [1]),
    TPId.

truck_provider(Id) -> 
    receive 
        {CBPid, truck} -> 
            io:format("Received truck request from ~p~n", [CBPid]),
            CBPid ! {truck, #truck{id = Id, capacity = 10, load = 0}},
            truck_provider(Id + 1)
    end.

process_product(Product, Truck) ->
    if Truck#truck.load + Product#product.size =< Truck#truck.capacity ->
        UpdatedTruck = Truck#truck{load = Truck#truck.load + Product#product.size},
        {ok, UpdatedTruck};
    true ->
        RemainingProduct = Product#product{size = Product#product.size - (Truck#truck.capacity - Truck#truck.load)},
        {error, RemainingProduct}
    end.

main() ->
    TPId = start_truck_provider(),
    Pids = start_conveyor_belts(TPId),
    generate_products(Pids, self()),
    wait_for_completion(?NUM_PRODUCTS).

wait_for_completion(0) ->
    io:format("All products processed.~n");
wait_for_completion(Count) ->
    receive
        {all_products_generated, Total} ->
            wait_for_completion(Total);
        {product_processed, _Id} ->
            wait_for_completion(Count - 1)
    end.