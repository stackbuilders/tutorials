---
title: How to start using Python Type Annotations with Mypy
published: 2020-02-04
tags: Python tutorial, Mypy , Python types, TDD
language: python
author-name: Carlos Villavicencio
twitter-profile: 
github-profile: po5i
description: Learn how to improve software quality and readability in Python code by leveraging the built-in typing system, as it complements other software development processes like type-driven development and test-driven development.
---

_Type-Driven Development_ is a technique in which we write types for a program before writing the program and then write code that satisfies the types, similar to how in test-driven development we write tests before writing code that passes the tests. It brings many benefits such as increasing robustness, accuracy, testability, readability, and extensibility of your code by taking advantage of the language’s type system.

In a [previous article][types-vs-tests], we discussed the benefits of working with _Type-Driven Development_ and the importance of static type checking at compilation time for strongly typed languages such as Haskell. Type-checking strengthens our code and reduces the number of tests we need to write. As a result, we are increasing our efficiency, whether if the paradigm is object-oriented or functional programming.

## What about Python?

Is it possible to use a strong type system in a language like Python? The answer is yes. [PEP-3107][pep-3107] and [PEP-484][pep-484] introduced _Type Annotations to Python_ 3.5 back in 2014. It’s been a while, right? Why is this not more popular? It's because Python, being a dynamic language, doesn’t require us to write variable or return types explicitly. They’re completely optional and mostly used by editors and IDEs.

In this article, we’ll be walking through the Python type system - how to add types to our code and Docstrings and how to perform static type checking using [mypy][mypy]. There are plenty of options for static type-checking in Python. However, in this tutorial we will follow [Guido van Rossum's suggestion][guido-mypy].

### Mypy

Mypy is a third-party Python library that provides optional static type checking. Unlike other non-dynamic programming languages like Java, where the static type-checking takes place at compilation time, Mypy CLI does the type-check to a file (or a set of files) on-demand. Apart from type-checking at development time, it's helpful to also include this check automatically in the Continuous Integration pipeline.

To start using mypy, install it using any version of `pip` globally or in your virtual environment.

```bash
pip install mypy
```

To perform static type checking on a source file:

```bash
mypy my_file.py
```

## Primitive types

You might know that programming languages can be statically or dynamically typed. A statically typed language does type checking at compile-time while dynamically typed language does it at run-time.

Another concept that is important to understand before we continue is the difference between weakly and strongly typed languages. In short, a strongly typed language has stricter rules such as variable assignment, return values, and function calling, while weakly typed ones can produce unpredictable results.

That being said, Python is a multi-paradigm dynamic language, so the type of variable is determined based on its value. This might be confusing, but it doesn't mean that Python is _weakly typed_. Python is _strongly typed_. Surprised? Check this out:

```python
movie = "Die Hard"
movie = movie + 2
# TypeError: cannot concatenate 'str' and 'int' objects
```

Now that all of that is clear, let’s begin this tutorial with a basic example:

```python
meat = "Ground beef"
print(type(meat))
# <class 'str'>

weight_pounds = 0.5
print(type(weight_pounds))
# <class 'float'>
```

In the above example we are using the `type` function to inspect the type representation of any variable. What if we include types in our first code example? Let’s check out the following example:

```python
meat: str = "Ground beef"
weight_pounds: str = 0.5
```

If we execute this file we expect the same output as the previous one, but we actually get the following:

```
error: Incompatible types in assignment (expression has type "float", variable has type "str")
```

Did you catch the reason why the error was thrown? The variable `weight_pounds` was defined as `str`, but we were assigning a float number to it. We can start spotting bugs everytime we run the type checking process.

I’m pretty sure you’ll be able to understand this function:

```python
def make_hamburger(meat, number_of_meats):
   return ["bread"] + [meat] * number_of_meats + ["bread"]


print(make_hamburger("ground beef", 2))
# ['bread', 'ground beef', 'ground beef', 'bread']
```

Let’s see how it goes with types:

```python
from typing import List


def make_hamburger(meat: str, number_of_meats: int) -> List[str]:
   return ["bread"] + [meat] * number_of_meats + ["bread"]
```

What happened here? We defined the function’s arguments types: string for `meat`, integer for `number_of_meats` and the return type, which is a list of string values. Did you notice the import at the beginning? Complex types such as `List`, `Dict` or `Tuple` must be imported from the `typing` library.

## Type Alias

You can also define your custom type names for known structures, which is very useful for improving readability. If we wanted to specify that a hamburger is a list of strings, we can define a type `Hamburger` in the following way:

```python
Hamburger = List[str]

def make_hamburger(meat: str, number_of_meats: int) -> Hamburger:
   return ["bread"] + [meat] * number_of_meats + ["bread"]
```

## Callables

A callable is anything you can call, using parentheses, and possibly with passing arguments as well. Callables can be functions, classes, methods, or even instances of classes (if their class implements a `__call__` method).

Are you working with functional programming? No problem, the typing library includes a type for callables which accepts a two-dimensional list like `[[argument1_type, … argumentN_type], return_type]`.

```python
from typing import Callable, Optional


def sum_and_process(a: int, b: int, callback: Callable[[int, Optional[str]], bool]) -> bool:
   total = a + b
   return callback(total, None)


def is_positive(val: int, message: Optional[str]) -> bool:
   if message:
       print(message)
   return val > 0


output = sum_and_process(5, 2, is_positive)
print(output)
# True
```

First of all, we are creating a function that takes two integer values, sums them up, and returns the output of an incoming callback. The callback `is_positive` is defined as a function that takes one mandatory integer and one optional string. If the message exists it will print it, and it returns `true` if the incoming value is greater than `0` (or `false` otherwise).

## Generics and Union Types

Two of the most powerful features in a type system are generics and union types. They are also available in Python's Type Annotations. Let’s take a look at the following example:

```python
from typing import TypeVar, List


T = TypeVar("T", int, List[str])

def generic_add(x: T, y: T) -> T:
   return x + y


x1: int = 5
y1: int = 2
print(generic_add(x1, y1))
# 7

x2: List[str] = ["Hello"]
y2: List[str] = ["World"]
print(generic_add(x2, y2))
# ['Hello', 'World']

x3: str = "foo"
y3: str = "bar"
print(generic_add(x3, y3))
# error: Value of type variable "T" of "generic_add" cannot be "str"
```

Above we defined a type variable `T` that can be an integer or a list of strings and a `generic_add` function that performs the “addition” operation for the incoming arguments, which are limited by the `TypeVar` declaration.

The first two invocations will work because we're passing arguments that belong to the `TypeVar` set of types, and the function will behave accordingly since both types implement the `+` operation. It will actually run for the third invocation because Python has this same operation for strings. Still, mypy will raise an error as `str` is not supported by the `T` definition.

We can also make use of union types. Let’s suppose we want to support any number that can only be of the types integer and float:

```python
from typing import Union


Number = Union[float, int]

def union_add(x: Number, y: Number) -> Number:
   return x + y


x1: int = 5
y1: int = 2
print(union_add(x1, y1))
# 7

x2: float = 3.14
y2: float = 3.14
print(union_add(x2, y2))
# 6.28

x3: str = "2"
y3: str = "1"
print(union_add(x3, y3))
# error: Argument 1 to "union_add" has incompatible type "str"; expected "Union[float, int]"
# error: Argument 2 to "union_add" has incompatible type "str"; expected "Union[float, int]"
```

Static type checking will always be important in order to prevent bugs or implementation misuse. It allows us to take advantage of Type-Driven Development without relying on an excessive amount of unit tests that check the values. The difference with Test-Driven Development is that "unlike tests, which can usually only be used to show the _presence_ of errors, types (used appropriately) can show the _absence_ of errors. But although types _reduce_ the need for tests, they rarely eliminate it entirely" (Brady, 2017, p.3).

For a deeper dive into the Type-driven Development topic, we suggest [_Type-Driven Development with Idris_][tdd-idris]. While Idris allows for more expressive types than Python, many of the concepts in that book can be utilized in Python or other languages.

Are you worried that the performance of the program could be affected by using Type Annotations? You don't need to be. Python doesn't check the types in run-time, although loading the `typing` library does add a small overhead with a negligible effect.

Also, don't forget to check out the official Python [documentation for typings][python-docs]. There you can find more useful stuff such as `Tuples`, `IO`, `Generator`, `Iterable`, and `Any` (_spoiler alert:_ don’t use `Any`).

[types-vs-tests]: https://www.stackbuilders.com/news/types-versus-tests-two-approaches-for-writing-correct-software
[tdd-idris]: https://www.manning.com/books/type-driven-development-with-idris
[guido-mypy]: https://mail.python.org/pipermail/python-ideas/2014-August/028618.html
[pep-484]: https://www.python.org/dev/peps/pep-0484/
[pep-3107]: https://www.python.org/dev/peps/pep-3107/
[mypy]: http://mypy-lang.org/
[python-docs]: https://docs.python.org/3/library/typing.html
