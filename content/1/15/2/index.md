---
linkTitle: "15.2. Developing a To-Do List Application"
title: "Functional To-Do List Application: Managing State with Immutable Data"
description: "Explore the development of a functional to-do list application using immutable data structures and pure functions. Learn to implement CRUD operations in Haskell, JavaScript, and Scala."
categories:
- Functional Programming
- Software Development
- Case Studies
tags:
- Functional Programming
- Immutable Data
- State Management
- Haskell
- JavaScript
- Scala
date: 2024-10-25
type: docs
nav_weight: 1520000
---

## 15.2. Developing a To-Do List Application

In this section, we will delve into the development of a to-do list application using functional programming principles. This project will help solidify your understanding of managing state functionally, implementing CRUD operations, and using immutable data structures. We will explore these concepts through practical examples in Haskell, JavaScript, and Scala.

### Managing State Functionally

Managing state in functional programming involves using immutable data structures and pure functions. This approach ensures that state changes are predictable and traceable, which enhances the reliability and maintainability of your code.

#### Immutable Data Structures

Immutable data structures are key in functional programming. They ensure that once a data structure is created, it cannot be altered. Instead, any modification results in a new data structure. This immutability is crucial for maintaining consistency and avoiding side effects.

#### Pure Functions for State Management

Pure functions are functions where the output is determined solely by the input values, without observable side effects. In our to-do list application, we will use pure functions to handle state transitions, such as adding, removing, and updating to-do items.

### Implementing Add, Remove, and Update Features

Let's explore how to implement the core features of a to-do list application—adding, removing, and updating items—using functional programming principles.

#### Adding a To-Do Item

To add a to-do item, we prepend the new item to the existing list, creating a new list.

#### Removing a To-Do Item

To remove a to-do item, we filter the list to exclude the specified item, resulting in a new list.

#### Updating a To-Do Item

Updating a to-do item involves mapping over the list and replacing the target item with the updated one.

### Code Snippets

Let's look at how these operations can be implemented in Haskell, JavaScript, and Scala.

#### Haskell

```haskell
type Todo = String
type TodoList = [Todo]

addTodo :: Todo -> TodoList -> TodoList
addTodo todo todos = todo : todos

removeTodo :: Todo -> TodoList -> TodoList
removeTodo todo = filter (/= todo)

main :: IO ()
main = do
  let todos = []
  let todos1 = addTodo "Buy milk" todos
  let todos2 = addTodo "Read book" todos1
  let todos3 = removeTodo "Buy milk" todos2
  print todos3 -- ["Read book"]
```

In Haskell, we define a `Todo` as a `String` and a `TodoList` as a list of `Todo`. The `addTodo` function prepends a new to-do to the list, while `removeTodo` filters out the specified to-do.

#### JavaScript

```javascript
const R = require('ramda');
const addTodo = (todo, todos) => R.prepend(todo, todos);
const removeTodo = (todo, todos) => R.filter(R.complement(R.equals(todo)), todos);

// Usage
let todos = [];
todos = addTodo("Buy milk", todos);
todos = addTodo("Read book", todos);
todos = removeTodo("Buy milk", todos);
console.log(todos); // ['Read book']
```

In JavaScript, using the Ramda library, we leverage functional utilities like `prepend` and `filter` to manage our to-do list immutably.

#### Scala

```scala
case class TodoItem(description: String)

type TodoList = List[TodoItem]

def addTodo(todo: TodoItem, todos: TodoList): TodoList = todo :: todos

def removeTodo(todo: TodoItem, todos: TodoList): TodoList = todos.filterNot(_ == todo)

def main(args: Array[String]): Unit = {
  var todos: TodoList = List()
  todos = addTodo(TodoItem("Buy milk"), todos)
  todos = addTodo(TodoItem("Read book"), todos)
  todos = removeTodo(TodoItem("Buy milk"), todos)
  println(todos) // List(TodoItem(Read book))
}
```

In Scala, we define a `TodoItem` as a case class and use a list to represent our `TodoList`. The `addTodo` and `removeTodo` functions operate similarly to their Haskell counterparts.

### Visual Aids

Below is a flowchart illustrating the state management and function interactions within our to-do list application.

```mermaid
graph TD;
    A[Start] --> B[Initialize Empty TodoList]
    B --> C[Add Todo: "Buy milk"]
    C --> D[Add Todo: "Read book"]
    D --> E[Remove Todo: "Buy milk"]
    E --> F[Print TodoList]
    F --> G[End]
```

This flowchart demonstrates the sequence of operations from initializing the list to printing the final state.

### Practical Exercises

1. **Exercise 1: Extend the Application**
   - Implement an `updateTodo` function in each language that updates an existing to-do item.
   - Test your implementation by updating a to-do item and printing the result.

2. **Exercise 2: Handle Edge Cases**
   - Modify the `removeTodo` function to handle cases where the to-do item does not exist in the list.
   - Ensure your solution does not throw errors and handles such cases gracefully.

### Conclusion

In this section, we explored how to develop a to-do list application using functional programming principles. By managing state with immutable data structures and pure functions, we ensured our application is robust and maintainable. The examples in Haskell, JavaScript, and Scala demonstrate the universality of these concepts across different functional languages.

### References

- "JavaScript Patterns" by Stoyan Stefanov.
- "Functional Programming in Scala" by Paul Chiusano and Rúnar Bjarnason.

## Quiz Time!

{{< quizdown >}}

### What is the primary benefit of using immutable data structures in functional programming?

- [x] They prevent side effects and ensure data consistency.
- [ ] They allow for faster data processing.
- [ ] They are easier to implement than mutable structures.
- [ ] They require less memory.

> **Explanation:** Immutable data structures prevent side effects by ensuring that data cannot be altered once created, which maintains consistency.

### Which function in Haskell is used to add a new to-do item to the list?

- [x] addTodo
- [ ] appendTodo
- [ ] insertTodo
- [ ] pushTodo

> **Explanation:** The `addTodo` function is used to prepend a new to-do item to the list in Haskell.

### In JavaScript, which library is used to facilitate functional programming in the provided example?

- [x] Ramda
- [ ] Lodash
- [ ] Underscore
- [ ] jQuery

> **Explanation:** The Ramda library is used in the JavaScript example to provide functional utilities like `prepend` and `filter`.

### What is the purpose of the `filter` function in the remove operation?

- [x] To exclude the specified item from the list.
- [ ] To add a new item to the list.
- [ ] To sort the list.
- [ ] To duplicate the list.

> **Explanation:** The `filter` function is used to create a new list that excludes the specified item, effectively removing it.

### How does Scala represent a to-do item in the provided example?

- [x] As a case class
- [ ] As a string
- [ ] As an integer
- [ ] As a map

> **Explanation:** In Scala, a to-do item is represented as a case class, which provides an immutable data structure with built-in methods.

### What is a pure function?

- [x] A function where the output is determined solely by its input values.
- [ ] A function that modifies global state.
- [ ] A function that relies on external data sources.
- [ ] A function that has side effects.

> **Explanation:** A pure function's output is determined solely by its input values, with no side effects or reliance on external data.

### Which operation is not part of CRUD?

- [ ] Create
- [ ] Read
- [ ] Update
- [x] Delete

> **Explanation:** All listed operations are part of CRUD except for "Delete," which is indeed part of CRUD, making this a trick question to ensure understanding.

### What does the `prepend` function do in the context of a to-do list?

- [x] Adds a new item to the beginning of the list.
- [ ] Adds a new item to the end of the list.
- [ ] Removes an item from the list.
- [ ] Sorts the list.

> **Explanation:** The `prepend` function adds a new item to the beginning of the list, which is how new to-dos are added in the examples.

### Why is immutability important in concurrent programming?

- [x] It prevents race conditions by ensuring data consistency.
- [ ] It allows for faster execution of code.
- [ ] It simplifies the code structure.
- [ ] It reduces memory usage.

> **Explanation:** Immutability prevents race conditions by ensuring that data cannot be changed by concurrent processes, maintaining consistency.

### True or False: Functional programming principles can only be applied in purely functional languages.

- [ ] True
- [x] False

> **Explanation:** Functional programming principles can be applied in multi-paradigm languages like JavaScript and Scala, not just in purely functional languages.

{{< /quizdown >}}
