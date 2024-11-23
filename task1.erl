-module(task1).

-export([main/0, generate_products/0, conveyor_belt/1, truck_provider/0]).

-define(NUM_PRODUCTS, 20).
-define(NUM_CONVEYOR_BELTS, 2).

-record(truck, {id, capacity, load}).
-record(product, {id, size}).

log_with_time(Format, Args) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    DateTimeString =
        io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                      [Year, Month, Day, Hour, Minute, Second]),
    LogMessage = io_lib:format(Format, Args),
    io:format("[~s] ~s~n", [DateTimeString, LogMessage]).

log_with_time(Format) ->
    log_with_time(Format, []).

get_cb_process_alias(Id) ->
    list_to_atom("cb" ++ integer_to_list(Id)).

start_generate_products() ->
    spawn(?MODULE, generate_products, []).

generate_products() ->
    generate_products(1).

generate_products(Id) when Id =< ?NUM_PRODUCTS ->
    Product = create_product(Id),
    log_with_time("Generate Products :: Product ~p with size ~p",
                  [Product#product.id, Product#product.size]),
    CbId = Id rem ?NUM_CONVEYOR_BELTS + 1,
    get_cb_process_alias(CbId) ! {product, Product},
    timer:sleep(500),
    generate_products(Id + 1);
generate_products(_) ->
    [get_cb_process_alias(CbId) ! stop || CbId <- lists:seq(1, ?NUM_CONVEYOR_BELTS)],
    tp ! stop.

create_product(Id) ->
    #product{id = Id, size = 3}.

start_conveyor_belts() ->
    [register(get_cb_process_alias(Id), spawn(?MODULE, conveyor_belt, [Id]))
     || Id <- lists:seq(1, ?NUM_CONVEYOR_BELTS)].

conveyor_belt(Id) ->
    Truck = get_truck(Id),
    conveyor_belt(Id, Truck).

conveyor_belt(Id, Truck) ->
    receive
        {product, Product} ->
            UpdatedTruck =
                if Truck#truck.load + Product#product.size > Truck#truck.capacity ->
                       log_with_time("Conveyor Belt ~p :: Truck ~p can't carry Product ~p",
                                     [Id, Truck#truck.id, Product#product.id]),
                       NewTruck = get_truck(Id),
                       NewTruck#truck{load = Product#product.size};
                   true ->
                       Truck#truck{load = Truck#truck.load + Product#product.size}
                end,
            log_with_time("Conveyor Belt ~p :: Truck ~p loaded Product ~p (Load: ~p/~p)",
                          [Id,
                           UpdatedTruck#truck.id,
                           Product#product.id,
                           UpdatedTruck#truck.load,
                           UpdatedTruck#truck.capacity]),
            main ! {processed, Product#product.id},
            conveyor_belt(Id, UpdatedTruck);
        stop ->
            log_with_time("Conveyor Belt ~p :: Stopping", [Id])
    end.

get_truck(Id) ->
    tp ! {Id, truck},
    log_with_time("Conveyor Belt ~p :: Requested truck", [Id]),
    receive
        {truck, Truck} ->
            log_with_time("Conveyor Belt ~p :: Received Truck ~p", [Id, Truck#truck.id]),
            Truck
    end.

start_truck_provider() ->
    register(tp, spawn(?MODULE, truck_provider, [])).

truck_provider() ->
    truck_provider(1).

truck_provider(Id) ->
    receive
        {CBId, truck} ->
            log_with_time("Truck Provider :: Truck request from Conveyor Belt ~p", [CBId]),
            get_cb_process_alias(CBId)
            ! {truck,
               #truck{id = Id,
                      capacity = 10,
                      load = 0}},
            truck_provider(Id + 1);
        stop ->
            log_with_time("Truck Provider :: Stopping")
    end.

wait_for_completion(0) ->
    log_with_time("Main :: All products processed");
wait_for_completion(Count) ->
    receive
        {processed, ProductId} ->
            log_with_time("Main :: Product ~p processed", [ProductId]),
            wait_for_completion(Count - 1)
    end.

main() ->
    register(main, self()),
    start_truck_provider(),
    start_conveyor_belts(),
    start_generate_products(),
    wait_for_completion(?NUM_PRODUCTS),
    unregister(main).
