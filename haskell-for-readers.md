% Haskell for Readers
% [Joachim Breitner](http://www.joachim-breitner.de/), [DFINITY Foundation](https://dfinity.org/)

Preface {.unnumbered}
=======

Welcome to the lecture series “Haskell for Readers”. This workshop is uniquely tailored to those who need to *read*, rather than *write* Haskell code: auditors, scientists, managers, testers etc.

### Scope {.unnumbered}

This goal implies a higher focus on syntax (because you can *write* programs ignoring most syntactic gadgets available to you, but if you read code, you have to deal with them), types and type signatures (because they are the key to understanding Haskell code) and abstraction patterns (because it is key to understanding well-written code, and Haskell excels at abstraction).

On the other hand, less words will be spent on how to approach writing the program, e.g. how to set up your tooling, how to please Haskell’s layout rules, how to design your data type, which libraries to pick, how to read error messages. That said, we hope that even Haskell programmers will gain useful insight from tutorial.

Nevertheless it is hard to understand a programming paradigm without writing any code, so there will some amount of hands-on work to be done, especially early on, when we start with a introduction to basic functional programming.

### Form and audience {.unnumbered}

This document is not (necessarily) a self-contained tutorial; it is rather the base for an interactive lecture, given by a real instructor. In such a lecture, some holes will be filled as we go, and the questions from the audience form a crucial part of the learning experience.

That said we I that the notes are detailed enough that while they are not ideal for independent study, they are at least useful and helpful.

I expect the audience to be familiar with programming and computer science in general, but do not assume prior knowledge of functional program (or, in case you are worried, category theory).

The exercises are all very small, in the order of minutes, and are meant to be done along the way, especially as later material may refer to their results. If you are reading this on your own and you really do not feel like doing them, you can click on the blurred solutions to at least read them.

Some sections are marked with ★. These are optional in the sense that the following material does not rely heavily on them. If time is short, e.g. during a workshop, they can be skipped.

### Acknowledgments and license {.unnumbered}

The creation of this material was sponsored by the [DFINITY Foundation](http://dfinity.org/), and is shared with the public under the terms of the [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/). You can view [the source on GitHub](https://github.com/nomeata/haskell-for-readers/) of this document, and submit improvements there.

The basics of functional programming
====================================

Functional program is the the art of thinking about *data* and how the new data is calculated from old data, rather than thinking about how to *modify* data.

Numbers and arithmetic operators
--------------------------------

The simplest form of data are numbers, and basic arithmetic is one way of creating new numbers from old numbers.

To play around with this, start the Haskell REPL (“read-eval-print-loop”) by running `ghci` (or maybe on [tryhaskell.org](https://tryhaskell.org/)), and enter some numbers, and some of the usual arithmetic operations:

~~~~ {#ghci .haskell}
$ ghci
GHCi, version 8.4.4: http://www.haskell.org/ghc/  :? for help
Prelude> 1
1
Prelude> 1 + 1
2
Prelude> 2 + 3 * 4
14
Prelude> (2 + 3) * 4
20
~~~~~~~

At this point we can tell that the usual precedence rules apply (i.e. the [PEMDAS rule](https://en.wikipedia.org/wiki/Order_of_operations#Mnemonics)).

```
Prelude> 0 - 1
-1
```
Numbers can be negative…

~~~~ {#numbers-negative .haskell}
Prelude> 2^10
1024
Prelude> 2^2^10
17976931348623159077293051907890247336179769789423065727343008115773267580550096
31327084773224075360211201138798713933576587897688144166224928474306394741243777
67893424865485276302219601246094119453082952085005768838150682342462881473913110
540827237163350510684586298239947245938479716304835356329624224137216
Prelude> (2^2)^10
1048576
~~~~

…and also very large. By default, Haskell uses arbitrary precision integer arithmetic. Note that for this lecture, we will completely avoid and ignore floating point arithmetic.

In the last example we can see that Haskell interprets `a^b^c` as `a^(b^c)`, i.e. the power operator is *right associative*. It is worth noting that this information is not hard-coded in the compiler. Instead, when the operator is defined somewhere in a library, its associativity and precedence can be declared. We can ask the compiler about this information:

```
Prelude> :info (^)
(^) :: (Num a, Integral b) => a -> b -> a 	-- Defined in ‘GHC.Real’
infixr 8 ^
```
Let us ignore the first line (which is the type signature): The `r` in `infixr` tells us that the `(^)` operator is right-associative. And the number is the precedence; a higher number means that this operator binds more tightly.

::: Exercise
What associativity do you expect for `(+)` and `(-)`? Verify your expectation.
:::

::: Solution
Both operator are left-associative, so that `10 - 2 + 3 - 4` means `((10 - 2) + 3) - 4` as expected.
:::

::: Exercise
Look up the precedences of the other arithmetic operations, and see how that corresponds to the PEMDAS rule.
:::

::: Solution
The precedences of `(+)` and `(-)` are the same, and smaller than the precedence of `(*)`, which is again shorter than the precedence of `(^)`.
:::

Applying Functions
------------------

So far we have a calculator (which is not useless, I sometimes use `ghci` as a calculator). But to get closer to functional programming, let us look at some functions that are already available to use.

To stay within the realm of arithmetic (if only to have something to talk about), let us play with the `div` and `mod` functions. These do what you would expect from them:

```
Prelude> div 123 100
1
Prelude> mod 123 100
23
```

We observe that to apply a function, we just write the function, followed by the arguments; no parenthesis or commas needed. This not only makes for more elegant and less noise code; but there is also a very deep and beautiful reason for this, which we will come to later.

At this point, surely someone wants to know what happens when we divide by 0:
```
Prelude> div 123 0
*** Exception: divide by zero
```
Haskell has exceptions, they can even be caught etc., but let us talk about that later.

Of course, if the argument is not just a single number, we somehow have to make clear where the argument begins and ends:
```
Prelude> div (120 + 3) (10 ^ 2)
1
```
(If you leave out the parenthesis, you get a horrible error messages). In technical terms, we can say that function application behaves like an right-associative operator of highest precedence. But it is easier to just remember **function application binds most tightly**. (Exception: Record construction and update binds even more tightly, although some consider that a design flaw.)

Just to have more examples, here are two other functions that we can play around with:

```
Prelude> id 42
42
Prelude> const 23 42
23
```

::: Exercise
Can you predict the result of the following?
```
Prelude> 1 + const 2 3 + 4
```
:::

::: Solution
```
6
```
:::

**A note on syntactic sugar:** Haskell is a high-calorie language: There is lots of syntactic sugar. Syntactic sugar refers to when there are alternative ways of writing something that *look* different, but *behave* the same. The goal is to allow the programmer to write the code in a way that best suits the reader, which is good, but it also means that a reader needs to know about the sugar.


**Infix operator application (Syntactic Sugar)**:
Functions that take two arguments can be written infix, as if they were an operator, by putting backticks around the name:
```
Prelude> 123 `div` 10
12
Prelude> 123 `mod` 10
3
Prelude> (120 + 3) `div` (10 ^ 2)
1
Prelude> (120 + 3) `div` 10 ^ 2
1
Prelude> ((120 + 3) `div` 10) ^ 2
144
```
We see that written as operators, even functions have an associativity and precedence:
```
Prelude> :info div
class (Real a, Enum a) => Integral a where
  ...
  div :: a -> a -> a
  ...
  	-- Defined in ‘GHC.Real’
infixl 7 `div`
```

**Prefix operator application (Syntactic Sugar) ★**:
We can also go the other way, and use any operator as if it were a function, by wrapping it in parentheses:
```
Prelude> 1 + 1
2
Prelude> (+) 1 1
2
```

**The dollar operator (Non-Syntactic Sugar) ★**:
Consider an expression that takes a number, and applies a number of functions , maybe with arguments, to it, such as:
```
f5 (f4 (f3 (f2 (f1 42))))
```
Passing a piece of data through a number of functions is very common, and some (including me) greatly dislike the accumulation of parentheses there. Therefore, it is idiomatic to use the `($)` operator:
```
f5 $ f4 $ f3 $ f2 $ f1 42
```
This operator takes a function as the first argument, an argument as the second argument, and applies the function to the argument. In that way, it is exactly the same as function application. But it is *right-associative* (instead of left-associative) and has the *lowest precedence* (instead of the highest precedence). An easier way of reading such code is to read `($)` as “the same as parenthesis around the rest of the line”.

I call this non-syntactic sugar, because the dollar operator it is not part of the built-in language, but can be define by anyone.


::: Exercise
What is the result of
```
Prelude> (-) 5 $ div 16 $ (-) 10 $ 4 `div` 2
```
:::

::: Solution
```
3
```
:::

Booleans and branching
----------------------

Some cryptographers might be happy to only write code that always does the same thing (yay, no side effects), but most of us pretty quickly want to write branching code.

As you would expect, Haskell has the usual operators to compare numbers:
```
Prelude> 1 + 1 == 2
True
Prelude> 4 < 5
True
Prelude> 4 >= 5
False
Prelude> 23 /= 42
True
```
We see that there are values `True` and `False`. We can combine them using the usual Boolean operators:
```
Prelude> 1 + 1 == 2 && 5 < 4
False
Prelude> 1 + 1 == 2 || 5 < 4
True
```

And finally, we can use `if … then … else …` to branch based on such a Boolean expression:
```
Prelude> if 5 < 0 then 0 else 1
1
Prelude> if -5 < 0 then 0 else 1
0
```

The use of `if … then … else …` is actually not the most idiomatic way to code decisions in Haskell, and we will come back to that point later, but for now it is good enough.

Function abstraction
--------------------

Assume you want to check a bunch of numbers as to whether they are multiples of 10 (so called “round numbers” in German). You can do that using `mod` and `(==)`:
```
Prelude> 5 `mod` 10 == 0
False
Prelude> 10 `mod` 10 == 0
True
Prelude> 11 `mod` 10 == 0
False
Prelude> 20 `mod` 10 == 0
True
Prelude> -20 `mod` 10 == 0
True
Prelude> 123 `mod` 10 == 0
False
```

But this gets repetitive quickly. And whenever we program something in a repetitive way, we try to recognize the *pattern* and abstract over the changing *parameter*, leaving only the common parts

Here, the common pattern is ``x `mod` 10 == 0``, with a parameter named `x`. We can give this pattern a name, and use it instead:
```
Prelude> isRound x = x `mod` 10 == 0
Prelude> isRound 5
False
Prelude> isRound 10
True
Prelude> isRound 11
False
Prelude> isRound 20
True
Prelude> isRound (-20)
True
Prelude> isRound 123
False
```

And now we are squarely in the realm of functional programming, as we have just defined out first function, `isRound`!

Note that we defined the `isRound` by way of an equation. And it really is an equation: Wherever we see `isRound something`, we can obtain its meaning by replacing it with ``something `mod` 10 == 0``. This *equational reasoning*, where you replace equals by equals is one key technique to make sense of Haskell programs.

::: Exercise
Discuss: Think of other programming language that have concepts called functions. Can you always replace a function call with the function definition? Does it change the meaning of the program?
:::

::: Exercise
Write a function `absoluteValue` with one parameter. If the parameter is negative, returns its opposite number, otherwise the number itself.
:::

::: Solution
```haskell
absoluteValue x = if x < 0 then - x else x
```
:::

::: Exercise
Write a function `isHalfRound` that checks if a number is divisible by 5, by checking whether the last digit is 0 or 5.
:::

::: Solution
```haskell
isHalfRound x = x `div` 10 == 0 || x `div` 10 == 5
```
:::

::: Exercise
Write a function `isEven` that checks if a number is divisible by 2, by checking whether the last digit is 0, 2, 4, 6, 8.
:::

::: Solution
```haskell
isEven x = x `div` 10 == 0 || x `div` 10 == 2 || x `div` 10 == 4 || x `div` 10 == 6 || x `div` 10 == 8
```
:::

Of course, you can abstract over more than one parameter. In the last exercise, you had to write something like ``x `div` 10 == y`` a lot. So it makes sense to abstract over that:

```
Prelude> hasLastDigit x y = x `div` 10 == y
```

This allows us to define `isHalfRound` as follows:
```
Prelude> isHalfRound x = x `hasLastDigit` 0 || x `hasLastDigit` 5
```
which, if you read it out, is almost a transliteration of the specification! Here we see how abstraction, together with good naming and syntax, can produce very clear and readable code.

**Infix operator application again (Syntactic Sugar)**:
By the way, you can use infix operator syntax already when defining a function:
```
x `divides` y = x `div` y == 0
```

Recursion
---------

We already saw that one function that we defined could call another. But the real power of general computation comes when a function can call itself, i.e. when we employ recursion. Recursion is a very fundamental technique in functional programming, much more so than loops or iterators or such.

Let us come up with a function that determines the number of digits in a given number. We first check if the number is already just one digit:
```
Prelude> countDigits n = if n < 10 then 1 else █
```
At this point, we know that the number is larger than 10. So to count the digits, we would like to cut off one digit:
```
Prelude> countDigits n = if n < 10 then 1 else (n `div` 10)█
```
and count the number of digits of *that* number
```
Prelude> countDigits n = if n < 10 then 1 else countDigits (n `div` 10)█
```
and, of course, add one to that number:
```
Prelude> countDigits n = if n < 10 then 1 else countDigits (n `div` 10) + 1
Prelude> countDigits 0
1
Prelude> countDigits 5
1
Prelude> countDigits 10
2
Prelude> countDigits 11
2
Prelude> countDigits 99
2
Prelude> countDigits 100
3
Prelude> countDigits 1000
4
Prelude> countDigits (10^12345)
12346
```

The fact that we can replace equals with equals does not change just because we use recursion. For example, we can figure out what `countDigits 789` does by replacing equals with equals:
```
  countDigits 789
= if 789 < 10 then 1 else countDigits (789 `div` 10) + 1
= if False then 1 else countDigits (789 `div` 10) + 1
= countDigits (789 `div` 10) + 1
= countDigits 78 + 1
= (if 78 < 10 then 1 else countDigits (78 `div` 10) + 1) + 1
= (if False then 1 else countDigits (78 `div` 10) + 1) + 1
= (countDigits (78 `div` 10) + 1) + 1
= (countDigits 7 + 1) + 1
= ((if 7 < 10 then 1 else countDigits (7 `div` 10) + 1) + 1) + 1
= ((if True then 1 else countDigits (7 `div` 10) + 1) + 1) + 1
= (1 + 1) + 1
= 2 + 1
= 3
```

::: Exercise
Write the function `sumDigits` that sums up the digits of a natural number.
:::

::: Solution
```haskell
sumDigits n = if n < 10 then n else sumDigits (n `div` 10) + (n `mod` 10)
```
:::

Higher-order functions
----------------------

We created functions when we took expressions that followed a certain pattern, and abstracted over a number that occurred therein. But the thing we can abstract over does not have to be just a simple number. It could also be a function!

Consider the task of calculating the number of digits in the number of digits of a number:
```
Prelude> countDigits (countDigits 5)
1
Prelude> countDigits (countDigits 10)
1
Prelude> countDigits (countDigits (10^10))
2
Prelude> countDigits (countDigits (10^123))
3
Prelude> countDigits (countDigits (15^15))
2
```

Clearly, we can abstract over the argument here:
```
Prelude> countCountDigits n = countDigits (countDigits n)
Prelude> countCountDigits (10^123)
3
```

But now consider we also want `sumSumDigits`:
```
Prelude> sumSumDigits n = sumDigits (sumDigits n)
Prelude> sumSumDigits (9^9)
9
Prelude> sumSumDigits (7^7)
7
Prelude> sumSumDigits (13^13)
13
Prelude> sumSumDigits (15^15)
18
```

There is clearly a pattern that is shared by both `countCountDigits` and `sumSumDigits`: They both apply a function twice. And indeed, we can abstract over that pattern:
```
Prelude> twice f x = f (f x)
Prelude> twice countDigits (15^15)
2
Prelude> twice sumDigits (15^15)
18
```
This is our first *higher order function*, and it is called so because it is a function that take another function as an argument. More precisely, it is called a second-order function, because it takes a normal, i.e. first-order function, as an argument. Abstracting over a second order function yields a third order function, and so on. Up to [sixth-order functions](https://doi.org/10.1017/S0956796898003001) are seen in the wild.

If you look at the last two lines, we again see a common pattern. And abstracting over that, we recover very nice and declarative definitions for `countCountDigits` and `sumSumDigits`:
```
Prelude> countCountDigits x = twice countDigits x
Prelude> sumSumDigits x = twice sumDigits x
```

The ability to abstract very easily over functions is an important ingredient in making Haskell so excellent at abstraction: It allows to abstract over *behavior*, instead merely over *value*. To demonstrate that, let us recall the definitions of `countDigits` and `someDigits`:
```
Prelude> countDigits n = if n < 10 then 1 else countDigits (n `div` 10) + 1
Prelude> sumDigits n = if n < 10 then n else sumDigits (n `div` 10) + (n `mod` 10)
```

These two functions share something: They share a behavior! Both iterate over the digits of the function, do something at each digit, and sum something up. And this common functionality is not trivial! So it is very unsatisfying to copy’n’paste it, like we did. So how can we abstract over the parts that differ? It is not obvious on first glance, so go through it step by step, lets give the parts that differ names. For `countDigits`, we *ignore* the digit, and just sum up ones:
```
Prelude> always1 n = 1
Prelude> countDigits n = if n < 10 then always1 n else countDigits (n `div` 10) + always1 (n `mod` 10)
```
And for `sumDigits`, we use the digit as is. And we have already seen a function that just returns its argument, the identity function:
```
Prelude> sumDigits n = if n < 10 then id n else sumDigits (n `div` 10) + id (n `mod` 10)
```
Now the common pattern is clear, and we can abstract over the different parts:
```
Prelude> sumDigitsWith f n = if n < 10 then f n else sumDigitsWith f (n `div` 10) + f (n `mod` 10)
Prelude> countDigits n = sumDigitsWith always1 n
Prelude> sumDigits n = sumDigitsWith id n
```

To recapitulate: We took two functions that were doing somehow related things, and we rewrote them to clearly separate the common parts from the differing parts, and then we could extract the shared essence into its own, higher-order function.

This single mechanism -- abstracting over functions -- can [replace thick volumes full of design patterns](https://www.voxxed.com/2016/04/gang-four-patterns-functional-light-part-1/) in non-functional programming paradigms.

Note that if one would have to abstract `countDigits` and `sumDigits` to `sumDigitsWith` in practice, one would probably not rewrite them first with `id` etc., but just look at them and come up with `sumDigitsWith` directly.

::: Exercise
Write a (recursive) function `fixEq` so that `fixEq f x` repeatedly applies `f` to `x` until the result does not change.
:::

::: Solution
```haskell
fixEq f x = if f x == x then x else fixEq f (f x)
```
:::

::: Exercise
Use this function and `countDigits` to write a function `isMultipleOf3` so that `isMultipleOf3 x` is true if repeatedly applying `countDigits` to `x` results in 3 or 9.
:::

::: Solution
```haskell
isMultipleOf3 x = fixEq sumDigits x == 3 || fixEq sumDigits x == 6 || fixEq sumDigits x == 9
```
:::

Anonymous functions
-------------------

We defined a function `always1`, but it seems a bit silly to give a name to such a specialized and small concept. Therefore, Haskell allows us to define *anonymous functions* on the fly. The syntax is a backslash, followed by the parameter (or parameters), followed by the body of the function. So we can define `countDigits` and `sumDigits` without any helper functions like this:

```
Prelude> countDigits n = sumDigitsWith (\d -> 1) n
Prelude> sumDigits n = sumDigitsWith (\d -> d) n
```

These are also called *lambda abstractions*, because they are derived from the Lambda calculus, and the backslash is a poor imitation of the Greek letter lambda (λ).

Higher-order function definition
--------------------------------

Lets look at the previous two definitions, and remember that when we define a function this way, we define what to replace the left-hand side with. But notice that the argument `n` is not touched at all by this definition! So we should get the same result if we simply omit it from the equation, right? And indeed, we can just as well write

```
Prelude> countDigits = sumDigitsWith (\d -> 1)
Prelude> sumDigits = sumDigitsWith (\d -> d)
```

It looks as if we just saved two characters. But what really just happened is that we shifted our perspective, and raised the level of abstraction by one layer. Instead of defining a `countDigits` as a function that takes a number and produces another number, we have defined `countDigits` as the result of instantiating the pattern `sumDigitsWith` with the function `(\d -> 1)`. At this level of thought, we do not care about the argument to `countDigits`, i.e. what it is called or so.

::: Exercise
Which other recent definitions can be changed accordingly?
:::

::: Solution
The definitions for `countCountDigits` and `sumSumDigits`:
```
Prelude> countCountDigits = twice countCountDigits
Prelude> sumSumDigits = twice sumDigits
```
:::

Currying
--------

We have already seen functions that *receive* a function as an argument. The way we use `twice` or `sumDigitsWith` here, we can think of them as a function that *return* functions. And this brings us to the deep and beautiful explanation we write multiple arguments to functions the way we do: Because really, every function only ever has one argument, and returns another one.

We can *think* of `twice` has having two arguments (the function `f`, and the value `x`), but really, `twice` is a function that takes one argument (the function `f`), and returns another function, that then takes the value `x`. This “other” function is what we named in the above definition of `sumSumDigits`.

The composition operator ★
--------------------------

Because writing code that passes functions around and modifies them (like in `twice` or `sumDigitsWith`) is so important in this style of programming, I should at this point introduce the composition operator. It is already pre-defined, but we can define it ourselves:
```
Prelude> (f . g) x = f (g x)
```
The dot is a poor approximation of the mathematical symbol for function composition, “∘”, and can be read as “`f` after `g`”. Note `x` is passed to `g` first, and then the result to `f`.

It looks like a pretty vacuous definition, but it is very useful in writing high-level code. For example, it allows us the following, nicely abstract definition of `twice`:
```
Prelude> twice f = f . f
```

Do you remember the example we used when introducing the dollar operator? We started with
```
f5 (f4 (f3 (f2 (f1 42))))
```
and rewrote it to
```
f5 $ f4 $ f3 $ f2 $ f1 42
```

Now imagine we want to abstract over 42:
```
many_fs x = f5 (f4 (f3 (f2 (f1 x))))
```
This function really is just the composition of a bunch of function. So an idiomatic way of writing it would be
```
many_fs  = f5 . f4 . f3 . f2 . f1
```
where again, the actual value is no longer the emphasis, but rather the functions.

The value `x` is sometimes called the point (as in geometry), and this style of programming is called *points-free* (or sometimes *pointless*).


Laziness ★
----------

As a final bit in this section, let’s talk about laziness. Most often this can be ignored when reading Haskell code, and in general laziness is not as important (or as bad) as some people say it is. But it plays an important role in Haskell’s support for abstraction, so let’s briefly look at it.

Laziness means that an expression is evaluated as late as possible, i.e. when it is needed to make a branching decision, or when it is to be printed on the screen. We can only observe when things are being evaluated when we have side-effects, and the only side effects we can produce so far are non-termination and exceptions. So let us use division by zero to observe that the first argument to `const` is used, but the second one is not:
```
Prelude> 0 `div` 0
*** Exception: divide by zero
Prelude> const (0 `div` 0) 1
*** Exception: divide by zero
Prelude> const 1 (0 `div` 0)
1
```

To see why this is so crucial for abstraction, consider the following two functions that implement a safe version of `div` and `mod` that just returns 0 if the user tries to divide by 0:
```
Prelude> x `safeDiv` y = if y == 0 then 0 else x `div` y
Prelude> x `safeMod` y = if y == 0 then 0 else x `mod` y
Prelude> 10 `safeDiv` 5
2
Prelude> 10 `safeDiv` 0
0
```

Clearly, the definitions of `safeDiv` and `safeMod` share a pattern. So let us extract the pattern “if y is zero then return zero else do something else”:

```
Prelude> unlessZero y z = if y == 0 then 0 else z
Prelude> x `safeDiv` y = unlessZero y (x `div` y)
Prelude> x `safeMod` y = unlessZero y (x `mod` y)
Prelude> 10 `safeDiv` 5
2
Prelude> 10 `safeDiv` 0
0
```
So that works. But it only works because Haskell is lazy. Consider what would happen in a strict language, where expressions are evaluated before passed to a function:

```
  10 `safeDiv` 0
= unlessZero 0 (10 `div` 0)
= unlessZero 0 (*** Exception: divide by zero
```

Only with laziness can we easily abstract not only over computations, but even over control flow, and create our own control flow constructs -- simply as higher-order functions!

In fact, even `if … then … else` could just be a normal function with three parameters, defined somewhere in the standard library. The fact that there is special syntax for it is pure convenience -- which, again, is not the case in strict languages.

Types
=====

In the first section, we have seen how functional programming opens the way to abstraction, and to condense independent concerns into separate pieces of code. This is a very powerful tool for modularity, and to focus on the relevant part of a problem, while keeping the bookkeeping out of sight. But powerful is also dangerous -- using a higher order function correctly without any aid, can be mind-bending.

Whenever we write functions like in the previous section, we have an idea in our head about what their arguments are -- are they just numbers, or are they functions, and what kind of functions -- and what they return. It is obvious to us that writing `twice isEven` does not make sense, because `isEven` returns `True` or `False`, but expects a number, so it cannot be applied to itself.

This is all simple and obvious, but it is a lot to keep in your head as the code grows larger, and even more so once the code is changing and there are more people working on it.  So to keep this power and complexity manageable, Haskell has a strong static type system, which is essentially a way for you to communicate with the compiler about these ideas you have in your head. You can ask the compiler “what do you know about this function? what can it take, what kind of things does it return?”. And you can tell the compiler “this function ought to take this and return that (and please tell me if you disagree)”.

In fact, many Haskellers prefer to do type-driven development: First think about and write down the type of the function they need to create, and *then* think about implementing them.

Besides communicating with the compiler, types are also crucial in communicating with your fellow developers and/or users of your API. For many functions, the type alone, or the type and the name, is sufficient to tell you what it does.

Tooling interlude: Editing files ★
----------------------------------

At this point, we should switch from working exclusively in the REPL to writing an actual Haskell file. We can start by creating a file `Types.hs`, and put in the code from the previous section:

```haskell
isRound x = x `mod` 10 == 0
hasLastDigit x y = x `div` 10 == y
isHalfRound x = x `hasLastDigit` 0 || x `hasLastDigit` 5
x `divides` y = x `div` y == 0
twice f x = f (f x)
countCountDigits x = twice countDigits x
sumSumDigits x = twice sumDigits x
sumDigitsWith f n = if n < 10 then f n else sumDigitsWith f (n `div` 10) + f (n `mod` 10)
countDigits = sumDigitsWith (\d -> 1)
sumDigits = sumDigitsWith (\d -> d)
fixEq f x = if f x == x then x else fixEq f (f x)
isMultipleOf3 x = fixEq sumDigits x == 3 || fixEq sumDigits x == 6 || fixEq sumDigits x == 9
```

We can load this file into `ghci` by either starting it with `ghci Types.hs` or by typing `:load Types.hs`. After you change and save the file, you can reload with `:reload` (or simply `:r`)

Basic types
-----------

As I mentioned before, you can chat with the compiler about the types of things, and ask what it thinks they are. We can do that with the `:type` (or `:t`) command:

```
*Main> :t sumDigits
sumDigits :: Integer -> Integer
```

Here, GHC tells us the type of `sumDigits`, in the form of a *type annotation*, i.e. a term (`sumDigits`) followed by two colons, followed by its type. The type itself tells us that `sumDigits` is a function (as indicated by the arrow) that takes an `Integer` as an argument and returns an `Integer` as a result. Which greatly matches our expectation!

Instead of asking GHC for the type, we can also specify it, simply by adding the line
```haskell
sumDigits :: Integer -> Integer
```
to the file (commonly directly above the function definition, rarely all bundled up in the beginning).

If we insert a type annotation that does not match what we wrote in the code, for example, if we added
```haskell
isRound :: Integer -> Integer
```
we would get an error message like
```
types.hs:2:13: error:
    • Couldn't match expected type ‘Integer’ with actual type ‘Bool’
    • In the expression: x `mod` 10 == 0
      In an equation for ‘isRound’: isRound x = x `mod` 10 == 0
  |
2 | isRound x = x `mod` 10 == 0
  |             ^^^^^^^^^^^^^^^
```

Note that the compiler believes the type signature, and complains about the code, not the other way around.

The error message mentions a type `Bool` which, as you can guess, is the type of Boolean expressions, e.g. `True` and `False`. With this knowledge, we can write the correct type signature for `isRound`:

```haskell
isRound :: Integer -> Bool
```

Polymorphism
------------

Let us consider `twice` for a moment, and think about what expect from its type. It is a function that takes two arguments, the and the first ought to be a function itself... and here is how GHC writes this:
```
*Main> :t twice
twice :: (t -> t) -> t -> t
```

From this example, we learn that
 * the type of functions with multiple arguments is written using multiple arrows,
 * the function arrow can just as occur inside an argument, namely when an argument itself is a function.

But what is this type `t`? There is not, actually, a type called `t`. Instead, this is a *type variable*, meaning that the function `twice` can be used with any type. Any lower-case identifier in a type is a type variable (not just `t`), and concrete types are always upper-case.

Here we can see that we can use `twice` with numbers, Booleans, and even with functions:
```
*Main> twice countDigits (99^99)
3
*Main> twice not True
True
*Main> twice twice countDigits (99^99)
1
```

What does not work is passing a function to `twice` that works on numbers, but then pass a `Bool`:
```
*Main> twice countDigits True

<interactive>:12:19: error:
    • Couldn't match expected type ‘Integer’ with actual type ‘Bool’
    • In the second argument of ‘twice’, namely ‘True’
      In the expression: twice countDigits True
      In an equation for ‘it’: it = twice countDigits True
```

In other words: The `t`s in the type of `twice` can become *any* type, but it has the be the same type everywhere.

::: Exercise
What do you think is the type of `id`?
:::

::: Solution
```haskell
id :: a -> a
```
:::


If we ask for the type of the function `const`, we see two different type variables:
```
*Main> :t const
const :: a -> b -> a
```
And here we can indeed instantiate them at different types:
```
*Main> :t const True 1
const True 1 :: Bool
```

Constrained types (a first glimpse)
-----------------------------------

There are more polymorphic functions in our initial set, for example `fixEq`:
```
*Main> :t fixEq
fixEq :: Eq t => (t -> t) -> t -> t
```
The part after the `=>` is what we expect: two arguments, the first a function, all the same types, just like with `twice`. The part before the `=>` is new: It is a *constraint*, and it limits which types `t` can be instantiated with. Remember that `fixEq` uses `(==)` to check if the value has stabilized. But not all values can be compared for equality! (In particular, functions cannot). So `fixEq` does not work with any type, but only those that support equality. This is what `Eq t` indicates, and indeed we get an error message when we try to do it wrongly:
```
*Main> fixEq twice not True

<interactive>:27:1: error:
    • No instance for (Eq (Bool -> Bool)) arising from a use of ‘fixEq’
        (maybe you haven't applied a function to enough arguments?)
    • In the expression: fixEq twice not True
      In an equation for ‘it’: it = fixEq twice not True
```
This `Eq` thing is not some built-in magic, but rather a *type class*, another very powerful and important feature of Haskell, which we will dive into separately later.

Other functions in our list are polymorphic where we may not have expected it:
```
*Main> :t sumDigitsWith
sumDigitsWith :: (Integral a1, Num a2) => (a1 -> a2) -> a1 -> a2
```
This is not the type we might have expected! This is because Haskell supports different numeric types, and uses type classes to overload the numeric operations. But remember that typing is a conversation: We can simply tell GHC that we want a different (more specific) type for `sumDigitsWith`, by adding
```haskell
sumDigitsWith :: (Integer -> Integer) -> Integer -> Integer
```
to our file. In fact, in practice one *always* writes full type signatures for all top-level definitions, so this should be less of a problem for Haskell readers.

Parametricity
-------------

One obvious use for such polymorphism is to write code once, and use it at different types. But there is another great advantage of polymorphic functions, even if we only ever intend to instantiate the type variables with the same type, and that is reasoning by parametricity.

In a function with a polymorphic type like `twice` there is not a lot we can do with the parameters. Sure, we can apply `f` to `x`, and maybe apply `f` more than once. But that is just about all we can do: Because `x` can have an arbitrary type, we cannot do arithmetic with it, we cannot print it, we cannot even compare it to other values of type `x`.

This severely restricts what `twice` can do at all … but on the other hand means that just from looking at the type signature of `twice` we already know a lot about what it does.

A very simple example for that is the function `id`, with type `a -> a`. *Any* function of this type will either

* return its argument (i.e. be the identity function),
* return never (i.e. go into an infinite loop), or
* raise an exception.

::: Exercise
A great example for the power of polymorphism is the following type signature:
```
(a -> b -> c) -> b -> a -> c
```
There is a function of that type in the standard library. Can you tell what it does? Can you guess its name? You can use a type-based search engine like [Hoogle](https://www.haskell.org/hoogle/?hoogle=%28a+-%3E+b+-%3E+c%29+-%3E+b+-%3E+a+-%3E+c) to find the function.
:::

::: Solution
It is the function `flip` that takes a function and swaps its first two arguments.
:::

Algebraic data types
--------------------

The function type is very expressive, and one can model many data structures purely with functions. But of course it is more convenient to use dedicated data structures. There are a number of data structure types that come with the standard library, in particular tuples, lists, the `Maybe` type. But it is more instructive to start defining our own.

We can declare new data types using the `data` keyword, the name of the type, the `=` sign, and then a list of *constructors*.

### Enumerations

In the simplest case, we can use this to declare an enumeration type:

```haskell
data Suit = Diamonds | Clubs | Hearts | Spades
```

From now on, we can use the constructors, e.g. `Diamonds` as values of type `Suit`. This is how we *create* values of type `Suit` -- and it is the only way, so we know that every value of type `Suit` is, indeed, one of these four constructors.

When we have a value of type `Suit` then the only thing we can really do with it is to find out which of these four constructors it actually is. The way to do that is using *pattern matching*, for example using the `case … of …` syntax:

```haskell
isRed :: Suit -> Bool
isRed s = case s of
  Diamonds -> True
  Hearts -> True
  _ -> False
```

The expression `case e of …` evaluates the *scrutinee* `e`, and then linearly goes through the list of cases. If the value of the scrutinee matches the *pattern* left of the arrow, the whole expression evaluates to the right-hand side. We see two kinds of patterns here: Constructor patterns like `Diamonds`, which match simply when the value is the constructor, and the *wildcard pattern*, written as an underscore, which matches any value.

It is common to immediately pattern match on the parameter of a function, so Haskell supports pattern-matching directly in the function definition:
```haskell
isRed :: Suit -> Bool
isRed Diamonds = True
isRed Hearts = True
isRed _ = False
```

The type `Bool` that we have already used before is nothing else but one of these enumeration types, defined as
```haskell
data Bool = False | True
```
and we could do without `if … then … else …` by pattern-matching on `Bool`:
```haskell
ifThenElse :: Bool -> a -> a -> a
ifThenElse True x y = x
ifThenElse False x y = y
```
the only reason to have `if … then … else …`  is that it is a bit more readable.

### Constructor with parameters

So far, the constructors were just plain values. But we can also turn them into “containers” of sort, where we can store other values. As an basic example, maybe we want to introduce complex numbers:

```
data Complex = C Integer Integer
```
(Mathematically educated readers please excuse the use of `Integers` here).

This creates a new type `Complex`, with a constructor `C`. But `C` itself is not a value of type `Complex`, but rather it is a function that creates values of type `Complex` and, crucially, it is the only way of creating values of type `Complex`. We can ask for the type of `C` and see that it is indeed just a function:
```
Prelude> :t C
C :: Integer -> Integer -> Complex
```
so it should be clear how to use it:
```haskell
origin :: Complex
origin = C 0 0
```

Again, and as always, the way to use a complex number is by pattern matching. This time we use pattern matching not to distinguish different cases -- *every* `Complex` is a `C` -- but to extract the parameters of the constructor:

```haskell
addC :: Complex -> Complex -> Complex
addC (C x1 y1) (C x2 y2) = C (x1 + x2) (y1 + y2)
```

The parameter in a pattern -- the `x1` here -- can itself be a pattern, for example `0` (which matches only the number 0), or underscore:
```haskell
isReal :: Complex -> Bool
isReal (C 0 _) = True
isReal _ = False
```

A type like `Complex`, with exactly one constructor, is called a *product type*. But we can of course have types with more than one constructor and constructor arguments:
```haskell
data Riemann = Complex Complex | Infinity
```
This declares a new type `Riemann` that can be built using one of these two constructors:

 1. The constructor `Complex`, which takes one argument, of type `Complex`. Types and terms (including constructors) have different namespaces, so we can have a type called `Complex`, and a constructor called `Complex`, and they can be completely independent. This can be confusing, but is rather idiomatic.

    The type of `Complex` shows that we can use it as a function, to create a point of the `Riemann` sphere from a complex number:
    ```
    Prelude> :t Complex
    Complex :: Complex -> Riemann
    ```
 2. The constructor `Infinity` takes no arguments, and simply is a value of type `Riemann` itself.

When we pattern match on a value of type `Riemann`, we learn whether it was created using `Complex` or `Infinity`, and in the former case, we also get the complex number passed to it:
```haskell
addR :: Riemann -> Riemann -> Riemann
addR (Complex c1) (Complex c2) = Complex (c1 `add` c2)
addR Infinity _  = Infinity
addR _ Infinity  = Infinity
```

A data type that has more than one constructor is commonly called a *sum type*. So these data types are built from sums and products, and hence are called algebraic data types (ADTs).

### Recursive data types

It it worth pointing out that it is completely fine to haven a constructor argument of the type that we are currently defining. This way, we obtain a *recursive data type*, and this is the foundation for many important data structures, in particular lists and trees of various sorts. Here is a simple example, a binary tree with numbers on all internal nodes:
```haskell
data Tree = Leaf | Node Integer Tree Tree
```
Again, this can be read as “a value of type `Tree` is either a `Leaf`, or it is a `Node` that contains a value of type `Integer` and references to two subtrees.”

There is nothing particularly interesting about constructing such a tree (use `Leaf` and `Node`) and traversing it (use pattern matching).  Here is a piece of idiomatic code that inserts a new number into a tree:
```haskell
insert :: Integer -> Tree -> Tree
insert x Leaf = Node x Leaf Leaf
insert x (Node y t1 t2)
    | y < x     = Node y t1 (insert x t2)
    | otherwise = Node y (insert x t1) t2
```
This code shows a new, syntactic feature: Pattern guards! These are Boolean expressions that you can use to further restrict when a case is taken. The third case in this function definition is only used if `y < x`, otherwise the following cases are tried. Of course this could be written using `if … then … else …`, but the readability and aesthetics are better with pattern guards. The value `otherwise` is simply `True`, but this reads better.


### Polymorphic data types

The tree data type declared in the previous section ought to be useful not just for integers, but maybe for any type. But it would be seriously annoying to have to create a new tree data type for each type we want to store in the tree. Therefore, we haven have *polymorphic data types*. In the example of the tree, we can write:
```haskell
data Tree a = Leaf | Node a (Tree a) (Tree a)
```

You may remember that the `a` here ought to be a type variable, because it occurs in the place of a type, but is lower-case. When we want to use this tree data type at a concrete type, say `Integer`, we simply write `Tree Integer`:
```haskell
insert :: Integer -> Tree Integer -> Tree Integer
```
The actual code of the function does not change.

We can also write functions that work on polymorphic trees, i.e. which we can use on *any* `Tree`, no matter what type the values in the nodes are. A good example is:
```haskell
size :: Tree a -> Integer
size Leaf = 0
size (Node _ t1 t2) = 1 + size t1 + size t2
```
Again, parametricity makes the type signature of such a function more useful than it seems at first: Just from looking at the type signature of `size` we *know* that this function does not look at the values stored in the nodes. Together with the name, that is really all the documentation we might need. Compare this to `size :: Tree Integer -> Integer` -- now it could just as well be that this function includes the number stored in the node in the result somehow.

### Functions in data types ★

Maybe this is obvious to you, after the emphasis on functions in the first chapter, but it is still worth pointing out that data type can also store functions. This blurs the distinction between data and code some more, as this nice example shows:
```haskell
data Stream a b
    = NeedInput (a -> Stream a b)
    | HasOutput b (Stream a b)
    | Done
```
The type `Stream a b` models a state machine that consumes values of type `a`, produces values of type `b`, and maybe eventually stops. Such a machine is in one of three states:

 1. Waiting for input. This uses the `NeedInput` constructor, which carries a *function* that consumes a type of value `a` and returns the new state of the machine.
 2. Producing output. This uses the `HasOutput` constructor, which stores the output value of type `b`, and the subsequent state of the machine.
 3. `Done`.

We can create a state machine that does run-length encoding this way. This one does not ever stop, but that’s fine:

```haskell
rle :: Eq a => Stream a (Integer,a)
rle = NeedInput rle_start

rle_start :: Eq a => a -> Stream a (Integer, a)
rle_start x = NeedInput (rle_count x 1)

rle_count :: Eq a => a -> Integer -> a -> Stream a (Integer, a)
rle_count x n x' | x == x' = NeedInput (rle_count x (n + 1))
                 | otherwise = HasOutput (n, x) (rle_start x')
```

Predefined data types
--------------------

We intentionally discussed the mechanisms of algebraic data types first, so that we can explain the most common data types in the standard library easily.

### Booleans ★

As mentioned before, the values `True` and `False` are simply the constructors of a data type defined as
```haskell
data Bool = False | True
```

There is nothing magic about the definition of `Bool`. But this type plays a special role because of the `if … then … else …` construct, and because of the pattern guards seen before.

### `Maybe`

A very common use case for algebraic data types is to capture the idea of a type whose values “maybe contain nothing, or just a value of type `a`”. Because this is so common, such a data type is predefined:
```haskell
data Maybe a = Nothing | Just a
```

You might see `Maybe`, for example, in the return type of a function that deserializes a binary or textual representation of a type, for example:
```haskell
parseFoo :: String -> Maybe Foo
```
Such an operation can fail, and if the input is invalid, it would return `Nothing`. As a user of such a function, the only way to get to the value of type `a` therein is to pattern-match on the result, which forces me to think about and handle the case where the result is `Nothing`.

This is much more robust than the common idiom in C, where you have to remember to check for particular error values (-1, or `NULL`), or Go, where you get both a result and a separate error code, but you can still be lazy and use the result without checking the error code.

A big part of Haskell’s reputation as a language that makes it easier to write correct code relies on the use of data types to precisely describe the values you are dealing with.

::: Exercise
How many values are there of type `Maybe (Maybe Bool)`. When can it be useful to nest `Maybe` in that way?
:::

::: Solution
There are four: `Nothing`, `Just Nothing`, `Just False` and `Just True`. It can be useful if, for example, the outer `Maybe` indicates whether some input was *valid*, whereas `Just Nothing` could indicate that the input was valid, but empty. But arguably this is not best practice, and dedicated data types with more speaking names could be preferred here.
:::

### `Either` ★

With maybe we can express “one or none”. Sometimes we want “one or another” type. For this, the standard library provides
```haskell
data Either a b = Left a | Right b
```

Commonly, this type is used for computations that can fail, but that provide some useful error messages when they fail:
```haskell
parseFoo :: String -> Either ParseError Foo
```
This gives us the same robustness benefits of `Maybe`, but also a more helpful error messages. If used in this way, then the `Left` value is always used for the error or failure case, and the `Right` value for when everything went all right.

### Lists

In the previous section we defined trees using a recursive data type. It should be obvious that we can define lists in a very analogous way:
```haskell
data List a = Empty | Link a (List a)
```

This data structure is so ubiquitous in functional programming that it not only comes with the standard library, it also has very special, magic syntax:
```haskell
data [a] = [] | a : [a]
```
In words: The type `[a]` is the type of lists with values of type `a`. Such a list is either the empty list, written as `[]`, or it is a non-empty list containing of a head `x` of type `a`, and a tail `xs`, and is written as `x:xs`. Note that the constructor `(:)`, called “cons”, is using operator syntax.

There is more special syntax for lists:

1. Finite lists can be written as `[1, 2, 3]` instead of `1 : 2 : 3 : []`.
2. Lists of numbers can be enumerated, e.g. `[1..10]`, or `[0,2..10]`, or even (due to laziness) `[1..]`.
3. List comprehensions look like `[ (x,y) | x <- xs, y <- ys, x < y ]`, reminiscent of the set comprehension syntax from mathematics. We will not discuss them now, I just wanted to show them and tell you what to search for.

Common operations on lists worth knowing are `(++)` to concatenate two lists.

Lists are very useful for many applications, but they are not a particularly high-performance data structure -- random access and concatenation is expensive, and they use quite a bit of memory. Depending on the application, other types like arrays/vectors, finger trees, difference lists might be more suitable.

### Characters and strings ★

Unexpectedly, Haskell has built-in support for characters and text. A single character has type `Char`, and is written in single quotes, e.g. `'a'`, `'☃'`, `'\''`, `'\0'`, `'\xcafe'`. These character are Unicode code points, and not just 7 or 8 bit characters.

The built-in type `String` is just an alias for `[Char]`, i.e. a list of characters. Haskell supports special built-in syntax for strings, using double quotes, but this is just syntactic sugar to the list syntax:
```
Prelude> "hello"
"hello"
Prelude> ['h','e','l','l','o']
"hello"
Prelude> 'h':'e':'l':'l':'o':[]
"hello"
```

Because `String` is built on the list type, it has the same problems with performance: While it is fine to be used in non-critical parts of the code (diagnostic and error messages, command line and configuration file parsing, filenames), it is usually the wrong choice if large amounts of strings need to be processed, e.g. in a templating library. Additionally libraries provide more suitable data structures, in particular `ByteString` for binary data and `Text` for human-readable text.


Records
-------

Newtypes
--------

A data type with one constructor and one field…


Type synonyms
-------------

Haddock-documentation.

Outlook: More type safety (refinement types, e.g. for natural numbers)


Code structure small and large
==============================

Let, where, modules, imports, qualified names
