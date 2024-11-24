# Report

## Task 1

### Main

We implemented a main/0 function, which starts the whole system when it is called. This function registers the main process with the 'main' alias, starts the truck provider process with the 'tp' alias, starts the conveyor belts processes with the 'cb' alias ('cb1', 'cb2', ...), starts the product generation process and then waits for all the products to be delivered to the trucks. Once all the products are delivered, the main process stops the truck provider process and the conveyor belts processes and unregisters its alias.

### Product Generation Process

This process is a recursive function that keeps generating products with an defined interval. And sends the generated products to the correct conveyor belt, following a round-robin strategy. Once it generates all the products, the process automatically stops.

### Truck Provider Process

This process is a recursive function that keeps generating trucks when it receives requests from the conveyor belts. It can also receive the stop message from the main process, which makes the process finish its execution. After receiving a truck request and a conveyor belt ID, it creates a new truck with a unique ID and sends it to that conveyor belt's process.

### Conveyor Belt Process

After starting, the conveyor belt process requests a new truck from the truck provider process and then starts a recursive function which will be the conveyor belt loop. This function waits for a product to arrive, then checks if the product fits in the current truck. If it fits, the product is loaded into the truck. If not, it replaces the truck with a new one with a request to the truck provider process. This process continues until a stop message is received from the main process.

### Important Notes

-   There are configurable constants defined as macros in the code, such as the number of conveyor belts, the number of generated products, the product size, the truck capacity, etc.
-   We make sure that the system is deadlock-free by using message passing and ensuring that all packages are eventually loaded onto trucks and confirmed by the main process. The processes are only stopped when main receives all the products, making sure that no package is left behind.
-   We built a logging function that prints messages with a timestamp to the console, which helps to understand the system's behavior and the order of events.
-   We built a function that gives us the process alias of a conveyor belt given its ID, which helps to send messages to the correct conveyor belt process.
-   We used multiple aliases to be free from the need to pass PIDs between processes, making the code cleaner and easier to understand. We considered this to be safe, since all of these processes run for basically the entire execution of the system and are correctly stopped.
