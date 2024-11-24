-module(task3).

-export([main/0, generate_products/0, conveyor_belt/1, truck_provider/0]).

-define(NUM_PRODUCTS, 100).
-define(PRODUCT_GENERATION_INTERVAL, 500).
-define(NUM_CONVEYOR_BELTS, 4).
-define(PRODUCT_MIN_SIZE, 1).
-define(PRODUCT_MAX_SIZE, 5).
-define(TRUCK_CAPACITY, 10).
-define(TRUCK_ARRIVAL_INTERVAL, 2000).

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
    timer:sleep(?PRODUCT_GENERATION_INTERVAL),
    Product = create_product(Id),
    log_with_time("Generate Products :: Product ~p with size ~p",
                  [Product#product.id, Product#product.size]),
    CbId = Id rem ?NUM_CONVEYOR_BELTS + 1,
    get_cb_process_alias(CbId) ! {product, Product},
    generate_products(Id + 1);
generate_products(_) ->
    log_with_time("Generate Products :: Stopping").

create_product(Id) ->
    #product{id = Id,
             size =
                 rand:uniform(?PRODUCT_MAX_SIZE - ?PRODUCT_MIN_SIZE + 1) + ?PRODUCT_MIN_SIZE - 1}.

start_conveyor_belts() ->
    [register(get_cb_process_alias(Id), spawn(?MODULE, conveyor_belt, [Id]))
     || Id <- lists:seq(1, ?NUM_CONVEYOR_BELTS)].

conveyor_belt(Id) ->
    Truck = get_truck(Id),
    conveyor_belt(Id, Truck).

conveyor_belt(Id, Truck) ->
    receive
        {product, Product} ->
            log_with_time("Conveyor Belt ~p :: Received Product ~p with size ~p",
                          [Id, Product#product.id, Product#product.size]),
            UpdatedTruck =
                if Truck#truck.load + Product#product.size > Truck#truck.capacity ->
                       log_with_time("Conveyor Belt ~p :: Truck ~p can't carry Product ~p with size "
                                     "~p (Load: ~p/~p)",
                                     [Id,
                                      Truck#truck.id,
                                      Product#product.id,
                                      Product#product.size,
                                      Truck#truck.load,
                                      Truck#truck.capacity]),
                       NewTruck = get_truck(Id),
                       NewTruck#truck{load = Product#product.size};
                   true ->
                       Truck#truck{load = Truck#truck.load + Product#product.size}
                end,
            log_with_time("Conveyor Belt ~p :: Truck ~p loaded Product ~p with size ~p "
                          "(Load: ~p/~p)",
                          [Id,
                           UpdatedTruck#truck.id,
                           Product#product.id,
                           Product#product.size,
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
            log_with_time("Conveyor Belt ~p :: Receiving Truck ~p...", [Id, Truck#truck.id]),
            timer:sleep(?TRUCK_ARRIVAL_INTERVAL),
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
            get_cb_process_alias(CBId) ! {truck, create_truck(Id)},
            truck_provider(Id + 1);
        stop ->
            log_with_time("Truck Provider :: Stopping")
    end.

create_truck(Id) ->
    #truck{id = Id,
           capacity = ?TRUCK_CAPACITY,
           load = 0}.

wait_for_completion(0) ->
    log_with_time("Main :: All products processed"),
    [get_cb_process_alias(CbId) ! stop || CbId <- lists:seq(1, ?NUM_CONVEYOR_BELTS)],
    tp ! stop;
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
