# Programming Languages Projects

A collection of projects developed for the Programming Languages course. These assignments were undertaken during the third semester of the Computer Science program at Gdańsk University of Technology. The course focused on introducing fundamental programming paradigms, including object-oriented programming in Smalltalk and functional programming in Haskell.

## Tasks by Language

### Ada95

- Create a program for simulating the operations of producers, consumers, and buffers (magazines) and add your own theme.
- Use rendezvous: selective wait and selective with conditional entry calls.
- Cover edge cases:
  - The consumer shouldn't receive assembly number `0`.
  - What happens if the producer cannot deliver an assembly to the buffer?
  - Other cases arising from your implementation.
- Solve concurrency problems:
  - Prevent deadlock.
  - Propose an idea for balancing the buffer or deliveries to ensure the program runs as smoothly as possible.
- Create a task called **"Cleaning"**, including an entry **"Start"** (similar to Consumer and Producer):
  - In the body of the task, initialize the variable `day_number` and assign it the value `1`.
  - Simulate a countdown of days with a fixed duration, incrementing `day_number` by one. When `day_number` reaches `10`, invoke the buffer entry **Cleaning_day** and reset `day_number` to `1`.
- In the **Buffer** task, add the entry **Cleaning_day**, and in its body:
  - Implement a procedure **Today_Is_Cleaning_Day** that removes 3 products of each type from the storage on day `10`.
  - Add another `or` condition to the `select` statement in the buffer to accept **Cleaning_day** and invoke **Today_Is_Cleaning_Day**.

### Smalltalk

- Create a subclass of the **Polygon** class: **An isosceles triangle** (base, height). The first vertex of each new polygon should be located at the point `(0,0)`.
- The new class should, just like the **Square** class, allow adding figures in terms of area. The result of the addition should have an area equal to the sum of the areas of the added figures and should also be a figure that retains the proportions of the recipient.
- The class should implement messages:
  - `area`
  - `+`
- Create a message **"scale: number"**, which will scale the sides of the polygon according to the given scale.
- Define the message **"print"**, which will print the vertices and area of the polygon. This message should be defined for the **Polygon** class. When adding transformation messages and other messages resulting from the task description, also ensure that the results are printed in the **"print"** message.
- Create a new message for objects of the **Polygon** class - **"rotate"**.
  - This message should accept a parameter **angle** and check if `0 < angle <= 90`.
  - Then, the polygon should be rotated by the specified angle to the right.

### Haskell

1. For a given natural number `n`, find the smallest natural number `m ≥ n` such that a set **A** can be formed consisting of two types of elements **X** and **Y** with the size `|A| = |X| + |Y| = m`, where `|X| ≥ |Y|`.
   - The probability of selecting two elements **X** in a row from the set **A** should be equal to `1/2`.
   - Example: For `n = 20`, `m = 21`, `|X| = 15`, and `|Y| = 6`, the probability is `P(xx) = (15/21) * (14/20) = 1/2`.

2. For any set of numbers **S**, let `sum(S)` denote the sum of its elements.
   - For a certain set **T** and a certain `k < n`, where `n = |T|`, consider all the sums `sum(Ui)` of all subsets `Ui` (where `0 < i ≤ C(n,k)`) of size `k` from the set **T**.
   - The subsets `Ui` represent all `C(n,k)` k-element combinations of the `n`-element set **T**.
   - Some of these sums `sum(Ui)` appear more than once, while others are unique.
   - Provide the sum of the unique sums `sum(Ui)` of the set **T**.
   - Example: For `T = {1, 3, 6, 8, 10, 11}` and `k = 3`, there are `C(6,3) = 20` subsets `Ui`, of which only `8` will have unique sums `sum(Ui)`, and their total sum will be `156`.

3. For a given natural number `n`, find the largest palindrome that can be obtained by multiplying two `n`-digit numbers.
   - Example: For `n=2`, the largest palindrome is `9009 = 91 × 99`.

### Prolog

- Implement **counting sort** as a predicate `counting_sort(List, SortedList)` which sorts the list in reverse order.
- Implement a predicate called `is_graph(List)` deciding if a list forms a **graphic sequence**.
  - Use the sorting predicate from the previous task.
- Implement a predicate `is_connected(List, Answer)` deciding if a list forms a **graphic sequence** and whether the graph it forms is **connected**.

---

## License
This project is licensed under the MIT License. See [LICENSE](LICENSE) for details.
