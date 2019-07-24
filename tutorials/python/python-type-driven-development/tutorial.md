---
title: Type annotations in Python
published: 2019-08-01
tags: Python, typing, types, tdd
language: python
author-name: Carlos Villavicencio
twitter-profile: po5i
github-profile: po5i
description: Learn how to improve the software quality and readability in our Python code by using the built-in typing system as it complements other software development processes like type-driven development and test-driven development.
---

_Type-Driven Development_ is a technique in which we plan our program development by adding types first and then write the code that satisfies that type. It brings many benefits such as increasing the robustness, accuracy, testability, readability, and extensibility of your code by taking advantage of the language’s type system.

In a [previous article][types-vs-tests], we discussed the benefits of working with _Type-Driven Development_ and the importance of static type checking at compilation time for strongly typed languages such as Java or Haskell. Type-checking strengthens our code, reduces the number of tests we need to write which decreases our dependency on them no matter if the paradigm is object-oriented or functional.

## What about Python?

Is it possible to use a strong type system in a language like Python? The answer is yes. [PEP-484][pep-484] introduced _Type Annotations to Python_ 3.5 back in 2014. It’s been a while, right? Why is this not popular? It's because Python being a dynamic language doesn’t require to explicitly write variable or return types, they’re completely optional and mostly used by editors and IDEs.

In this article, we’ll be walking through the Python - how to add types to our code and Docstrings and perform static type checking using [mypy][mypy]. There are plenty options for static type-checking in Python, but in this tutorial we will follow [Guido van Rossum suggestion][guido-mypy].

### Mypy

Mypy is a third-party Python library that provides optional static type checking. Unlike other non-dynamic programming languages like Java, where the static type-checking takes place at compilation time, Mypy CLI does the type-check to a file (or a set of files) on demand. Apart of having type-checking at development time, it's helpful to also include this check automatically in the Continous Integration pipeline.

To start using mypy, install it using any version of `pip` globally or in your virtual environment.

```bash
pip install mypy
```

To perform static type checking on a source file:

```bash
mypy my_file.py
```

## Primitive types

You might know that programming languages can be statically or dynamically typed. A statically typed language does type checking at compile-time while dynamically type languages do it at run-time.

Another concept that is important to know before we continue is the difference between weak and strong typed languages. I short, a strong typed language has stricter rules such as variable assignment, return values and function calling while weak typed ones can produce unpredictable results.

That said, Python is a multi-paradigm dynamic language, so the type of variable is determined based on the value. This might be confusing, but it doesn't mean that Python has _weak typing_. Python is _strongly typed_. Surprised? Check this out:

```python
movie = "Die Hard"
movie = movie + 2
# TypeError: cannot concatenate 'str' and 'int' objects
```

That said, let’s begin this tutorial with a basic example:

```python
meat = "Ground beef"
print(type(meat))
# <class 'str'>

weight_pounds = 0.5
print(type(weight_pounds))
# <class 'float'>
```

In the above example, we are using the `type` function to inspect the type representation of any variable. What if we include types in our first example code? Let’s check the following example:

```python
meat: str = "Ground beef"
weight_pounds: str = 0.5
```

If we execute this file we expect the same output as the previous one, but we actually get the following:

```
error: Incompatible types in assignment (expression has type "float", variable has type "str")
```

Did you catch the reason? The variable `weight_pounds` was defined as `str` but we were assigning a float number to it. We can start spotting bugs in every type check.

I’m pretty sure you can understand this function:

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

What happened here? We defined the function’s arguments types: string for `meat`, integer for `number_of_meats` and the return type which is a list of string values. Did you see the import at the beginning? Complex types such as `List`, `Dict` or `Tuple` must be imported from the `typing` library.

## Type Alias

You can also define your custom type names for known structures, which is very useful for improving readability. Let’s say if we want to consider a hamburger is a list of strings, we can define a type `Hamburger` in the following way:

```python
Hamburger = List[str]

def make_hamburger(meat: str, number_of_meats: int) -> Hamburger:
   return ["bread"] + [meat] * number_of_meats + ["bread"]
```

## Callables

A callable is anything you can call, using parenthesis, and possibly passing arguments. Callables are functions, classes, methods and even instances of classes can be callables if their class implements a `__call__()` method.

Are you doing functional programming? No problem, the typing library includes a type for callables which accepts a two-dimensional list like `[[argument1_type, … argumentN_type], return_type]`.

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

First of all, we are creating a function that takes two integer values, sums them up and returns the output of an incoming callback. This callback `is_positive` is defined as a function that takes one mandatory integer and one optional string. If the message exists it will print it, and always returns if the incoming value is positive.

## Generics and Union Types

One of the most powerful features in a type system are the generics and union types. It’s also possible to use these in-Python annotations. Let’s take a look at the following example:

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

Above, we defined a type variable `T` that can be integer or List of strings and a `generic_add` function that performs the “addition” operation for the incoming arguments which are limited by the `TypeVar` declaration.

The first two invocations will work because we're passing arguments that belong to the `TypeVar` set of types and the function will behave accordingly since both types implements the `+` operation. It will actually run for the third invocation because Python implements the add operation for strings, but mypy will raise the error as `str` is not supported by the `T` definition.

Also, we can make use of union types. Let’s suppose we want to support any number that can only be of types integer and float:

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

To wrap up, static type checking will always be important to prevent bugs or implementation misuse. It allows us to take advantage of Type-Driven Development without relying on an excessive amount of unit tests that check the values. The difference with Test-Driven Development is that, "unlike tests, which can usually only be used to show the _presence_ of errors, types (used appropriately) can show the _absence_ of errors. But although types _reduce_ the need for tests, they rarely eliminate it entirely" (Brady, 2017, p.3). We strongly suggest as follow up reading [_Type-Driven Development with Idris_][tdd-idris] and extrapolate these practices to Python or other languages.

Also, don't forget to check out the official Python [documentation for typings][python-docs]. There you will find more useful stuff such as `Tuples`, `IO`, `Generator`, `Iterable` and `Any` (_spoiler alert:_ don’t use `Any`).

[types-vs-tests]: https://www.stackbuilders.com/news/types-versus-tests-two-approaches-for-writing-correct-software
[tdd-idris]: https://www.manning.com/books/type-driven-development-with-idris
[guido-mypy]: https://mail.python.org/pipermail/python-ideas/2014-August/028618.html
[pep-484]: https://www.python.org/dev/peps/pep-0484/
[mypy]: http://mypy-lang.org/
[python-docs]: https://docs.python.org/3/library/typing.html
