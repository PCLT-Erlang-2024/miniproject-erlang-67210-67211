# Report

## Task 3

### Changes to Task 2

-   Added a new macro for the truck replacement time, `TRUCK_ARRIVAL_INTERVAL`.
-   In the process of requesting a new truck to the truck provider (function `get_truck/1`), after the truck is received, the process sleeps for the time defined by the `TRUCK_ARRIVAL_INTERVAL` macro and then returns the new truck to the conveyor belt loop.
-   During truck replacement, the conveyor belt loop is paused and is not processing any new products until the new truck is ready to receive them.
