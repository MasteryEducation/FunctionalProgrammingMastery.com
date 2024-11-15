---
linkTitle: "11.3. Writing Your First Functional Program"
title: "First Functional Program: Haskell, JavaScript, and Scala"
description: "Learn how to write your first functional program in Haskell, JavaScript, and Scala, understanding the syntax and semantics of each language."
categories:
- Functional Programming
- Haskell
- JavaScript
- Scala
tags:
- Functional Programming
- Haskell
- JavaScript
- Scala
- Hello World
date: 2024-10-25
type: docs
nav_weight: 1130000
---

## 11.3. Writing Your First Functional Program

Embarking on your journey into functional programming begins with writing a simple "Hello, World!" program. This exercise, though basic, introduces you to the syntax and semantics of functional languages. We'll explore how to write this program in Haskell, JavaScript, and Scala, providing you with a foundational understanding of each language's structure and style.

### Simple "Hello, World!" in a Functional Language

#### Haskell

Haskell is a purely functional language known for its strong static typing and lazy evaluation. Here's how you write a "Hello, World!" program in Haskell:

```haskell
main :: IO ()
main = putStrLn "Hello, World!"
```

**Understanding the Syntax and Semantics:**

- **`main :: IO ()`**: This line declares the type of `main`. In Haskell, `IO ()` indicates that `main` is an I/O action that returns no meaningful value (denoted by `()`).
- **`main = putStrLn "Hello, World!"`**: This line defines `main` as the action of printing "Hello, World!" to the console. `putStrLn` is a standard library function that outputs a string followed by a newline.

#### JavaScript

JavaScript, while not a purely functional language, supports functional programming paradigms. Here's a functional approach to "Hello, World!" in JavaScript:

```javascript
const greet = () => {
  console.log("Hello, World!");
};

greet();
```

**Understanding the Syntax and Semantics:**

- **`const greet = () => { ... }`**: This line defines a constant `greet` as an arrow function, a concise syntax for function expressions in JavaScript.
- **`console.log("Hello, World!");`**: Inside the function, `console.log` is used to print "Hello, World!" to the console.
- **`greet();`**: This line calls the `greet` function, executing the code within it.

#### Scala

Scala is a hybrid language that combines object-oriented and functional programming. Here's how you write "Hello, World!" in Scala:

```scala
object HelloWorld extends App {
  println("Hello, World!")
}
```

**Understanding the Syntax and Semantics:**

- **`object HelloWorld extends App`**: This line defines a singleton object `HelloWorld` that extends the `App` trait, which allows the program to be executed as a script.
- **`println("Hello, World!")`**: Within the object, `println` is used to output "Hello, World!" to the console.

### Visual Aids

To better understand the structure of a basic functional program in each language, let's visualize the components using Mermaid.js diagrams.

#### Haskell Program Structure

```mermaid
graph TD;
    A[main :: IO ()] --> B[putStrLn "Hello, World!"];
```

#### JavaScript Program Structure

```mermaid
graph TD;
    A[greet = () => {...}] --> B[console.log("Hello, World!")];
    B --> C[greet()];
```

#### Scala Program Structure

```mermaid
graph TD;
    A[object HelloWorld extends App] --> B[println("Hello, World!")];
```

### Key Takeaways

- **Haskell** emphasizes purity and type safety, with `IO` actions encapsulating side effects.
- **JavaScript** allows functional programming through first-class functions and closures, despite being multi-paradigm.
- **Scala** blends functional and object-oriented paradigms, offering flexibility and expressiveness.

### References

- "Programming in Haskell" by Graham Hutton.
- "Scala for the Impatient" by Cay S. Horstmann.

These resources provide deeper insights into the languages and their functional programming capabilities.

## Quiz Time!

{{< quizdown >}}

### What is the type of the `main` function in Haskell?

- [x] IO ()
- [ ] String
- [ ] Int
- [ ] Bool

> **Explanation:** In Haskell, `main` is an I/O action with the type `IO ()`, indicating it performs input/output operations and returns no meaningful value.

### Which JavaScript feature allows it to support functional programming?

- [x] First-class functions
- [ ] Classes
- [ ] Prototypes
- [ ] Promises

> **Explanation:** JavaScript supports functional programming through first-class functions, allowing functions to be treated as values.

### In Scala, what does extending the `App` trait allow?

- [x] Running the program as a script
- [ ] Defining a new class
- [ ] Creating a new package
- [ ] Importing libraries

> **Explanation:** Extending the `App` trait in Scala allows the program to be executed as a script without explicitly defining a `main` method.

### What function is used in Haskell to print to the console?

- [x] putStrLn
- [ ] print
- [ ] echo
- [ ] display

> **Explanation:** `putStrLn` is the function used in Haskell to print a string followed by a newline to the console.

### Which syntax is used to define a function in JavaScript?

- [x] Arrow function
- [ ] Lambda function
- [ ] Anonymous function
- [ ] Named function

> **Explanation:** JavaScript uses arrow functions, a concise syntax for function expressions, to define functions.

### What is the purpose of the `println` function in Scala?

- [x] To print output to the console
- [ ] To read input from the user
- [ ] To write to a file
- [ ] To execute a command

> **Explanation:** `println` is used in Scala to print output to the console.

### Which of the following languages is purely functional?

- [x] Haskell
- [ ] JavaScript
- [ ] Scala
- [ ] Python

> **Explanation:** Haskell is a purely functional language, emphasizing immutability and pure functions.

### What does the `const` keyword do in JavaScript?

- [x] Declares a block-scoped constant
- [ ] Declares a variable
- [ ] Declares a function
- [ ] Declares a class

> **Explanation:** The `const` keyword in JavaScript declares a block-scoped constant, meaning its value cannot be reassigned.

### In Haskell, what does `IO ()` signify?

- [x] An I/O action with no meaningful return value
- [ ] A function returning an integer
- [ ] A string operation
- [ ] A boolean expression

> **Explanation:** `IO ()` in Haskell signifies an I/O action that returns no meaningful value, encapsulating side effects.

### True or False: Scala is a purely functional language.

- [ ] True
- [x] False

> **Explanation:** Scala is not purely functional; it is a hybrid language that combines functional and object-oriented programming paradigms.

{{< /quizdown >}}
