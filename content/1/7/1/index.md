---
linkTitle: "7.1. Basics of Function Composition"
title: "Function Composition Basics: Combining Simple Functions for Complex Solutions"
description: "Explore the fundamentals of function composition in functional programming, including its advantages and implementation in Haskell, JavaScript, and Scala."
categories:
- Functional Programming
- Software Development
- Programming Concepts
tags:
- Function Composition
- Functional Programming
- Code Modularity
- Haskell
- JavaScript
- Scala
date: 2024-10-25
type: docs
nav_weight: 710000
---

## 7.1. Basics of Function Composition

Function composition is a fundamental concept in functional programming that allows developers to build complex operations by combining simpler, reusable functions. This approach not only enhances modularity and code reuse but also improves readability and maintainability of the codebase. In this section, we will delve into the mechanics of function composition, explore its advantages, and provide practical examples in Haskell, JavaScript, and Scala.

### Combining Simple Functions into Complex Ones

At its core, function composition is the process of applying one function to the result of another, thereby creating a new function. This is akin to mathematical function composition, where if you have two functions, `f` and `g`, the composition `f(g(x))` applies `g` to `x` and then `f` to the result of `g(x)`.

In functional programming, this concept is often represented using the composition operator, which varies by language but serves the same purpose: to chain functions together seamlessly.

#### Example in Haskell

In Haskell, function composition is achieved using the `(.)` operator. Here's a simple example:

```haskell
let addOne = (+1)
let multiplyByTwo = (*2)
let addOneThenMultiplyByTwo = multiplyByTwo . addOne
print (addOneThenMultiplyByTwo 3) -- 8
```

In this example, `addOneThenMultiplyByTwo` is a new function created by composing `addOne` and `multiplyByTwo`. When applied to the number `3`, it first adds one, resulting in `4`, and then multiplies by two, yielding `8`.

#### Example in JavaScript

JavaScript, with libraries like Ramda, provides a `compose` function to facilitate function composition:

```javascript
const R = require('ramda');

const addOne = x => x + 1;
const multiplyByTwo = x => x * 2;
const addOneThenMultiplyByTwo = R.compose(multiplyByTwo, addOne);

console.log(addOneThenMultiplyByTwo(3)); // 8
```

Here, `R.compose` takes `multiplyByTwo` and `addOne` as arguments, creating a new function that applies these operations in sequence.

#### Example in Scala

Scala uses the `compose` method to achieve function composition:

```scala
def addOne(x: Int): Int = x + 1
def multiplyByTwo(x: Int): Int = x * 2

val addOneThenMultiplyByTwo = multiplyByTwo _ compose addOne _

println(addOneThenMultiplyByTwo(3)) // 8
```

In Scala, the underscore (`_`) is used to convert methods into function values, allowing them to be composed.

### Advantages of Function Composition

Function composition offers several benefits that make it an attractive paradigm in functional programming:

1. **Enhanced Modularity:** By breaking down complex operations into smaller, reusable functions, developers can build modular code that is easier to understand and maintain.

2. **Code Reuse:** Composed functions can be reused across different parts of a program, reducing redundancy and promoting DRY (Don't Repeat Yourself) principles.

3. **Improved Readability:** Function composition allows for a declarative style of programming, where the focus is on what needs to be done rather than how. This can lead to more readable and expressive code.

4. **Ease of Testing:** Smaller, pure functions are easier to test individually, and their compositions can be tested by verifying the expected output for given inputs.

### Visual Aids

To better understand function composition, consider the following diagram illustrating the composition of two functions, `f` and `g`, to form a new function `h`.

```mermaid
graph LR
    A[x] --> B[g(x)]
    B --> C[f(g(x))]
    C --> D[h(x)]
    subgraph Function Composition
        A --> B --> C
    end
```

In this diagram, `h(x)` represents the composed function that applies `g` to `x` and then `f` to the result of `g(x)`.

### Practical Exercises

To reinforce your understanding of function composition, try the following exercises:

1. **Exercise 1:** Implement a composed function in Haskell that first squares a number and then subtracts two. Test it with the input `5`.

2. **Exercise 2:** Using JavaScript and Ramda, create a composed function that converts a string to uppercase and then appends an exclamation mark. Test it with the string `"hello"`.

3. **Exercise 3:** In Scala, compose two functions: one that doubles a number and another that converts it to a string. Test it with the input `7`.

### References

- "Functional Programming in Scala" by Paul Chiusano and Rúnar Bjarnason.
- "JavaScript Allongé" by Reginald Braithwaite.

### Summary of Key Points

- Function composition allows for building complex functions by combining simpler ones.
- It enhances modularity, code reuse, and readability.
- Different languages offer various syntax and libraries to facilitate function composition.
- Practical exercises can help solidify the understanding of function composition.

## Quiz Time!

{{< quizdown >}}

### What is the primary purpose of function composition in functional programming?

- [x] To build complex functions from simpler ones
- [ ] To improve execution speed
- [ ] To reduce memory usage
- [ ] To enforce type safety

> **Explanation:** Function composition is primarily used to build complex functions by combining simpler, reusable functions.

### Which operator is used for function composition in Haskell?

- [x] (.)
- [ ] (+)
- [ ] (*)
- [ ] (->)

> **Explanation:** In Haskell, the `(.)` operator is used for function composition.

### In JavaScript, which library provides a `compose` function for function composition?

- [x] Ramda
- [ ] Lodash
- [ ] Underscore
- [ ] jQuery

> **Explanation:** Ramda is a JavaScript library that provides a `compose` function for function composition.

### What is a key advantage of function composition?

- [x] Enhanced modularity
- [ ] Increased code complexity
- [ ] Reduced readability
- [ ] Slower execution

> **Explanation:** Function composition enhances modularity by allowing developers to build complex operations from smaller, reusable functions.

### How does Scala achieve function composition?

- [x] Using the `compose` method
- [ ] Using the `add` method
- [ ] Using the `subtract` method
- [ ] Using the `multiply` method

> **Explanation:** Scala uses the `compose` method to achieve function composition.

### What is the result of the composed function `addOneThenMultiplyByTwo` when applied to `3` in the provided examples?

- [x] 8
- [ ] 6
- [ ] 9
- [ ] 7

> **Explanation:** The composed function first adds one to `3`, resulting in `4`, and then multiplies by two, yielding `8`.

### Which of the following is NOT a benefit of function composition?

- [ ] Code reuse
- [ ] Improved readability
- [x] Increased memory usage
- [ ] Ease of testing

> **Explanation:** Function composition does not inherently increase memory usage; it focuses on code reuse, readability, and ease of testing.

### In the context of function composition, what does the underscore (`_`) signify in Scala?

- [x] It converts methods into function values
- [ ] It denotes a private variable
- [ ] It is used for comments
- [ ] It indicates a loop

> **Explanation:** In Scala, the underscore (`_`) is used to convert methods into function values, allowing them to be composed.

### What is the result of composing a function that squares a number and another that subtracts two, when applied to `5`?

- [x] 23
- [ ] 27
- [ ] 20
- [ ] 25

> **Explanation:** Squaring `5` gives `25`, and subtracting two results in `23`.

### True or False: Function composition can only be used in functional programming languages.

- [ ] True
- [x] False

> **Explanation:** Function composition is a concept that can be applied in any programming paradigm, although it is a fundamental aspect of functional programming.

{{< /quizdown >}}
