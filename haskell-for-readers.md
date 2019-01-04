Haskell for Readers
==================

Welcome to the lecture series “Haskell for Readers”. This workshop is uniquely tailored to those who need to *read*, rather than *write* Haskell code: auditors, scientists, managers, testers etc.

This implies a higher focus on syntax (because you can *write* programs ignoring most syntactic gadgets available to you, but if you read code, you have to deal with them), types and type signatures (because they are the key to understanding Haskell code) and abstraction patterns (because it is key to understanding well-written code, and Haskell excels at abstraction).

On the other hand, less words will be spent on how to approach writing the program, e.g. how to set up your tooling, how to please Haskell’s layout rules, how to design your data type, which libraries to pick, how to read error messages. That said, we hope that even Haskell programmers will gain useful insight from tutorial.

Nevertheless it is hard to understand a programming paradigm without writing any code, so there will some amount of hands-on work to be done, especially early on, when we start with a introduction to basic functional programming.

I expect the audience to be familiar with programming and computer science in general.

The basics of functional programming
------------------------------------

Functional program is the the art of thinking about *data* and how the new data is calculated from old data, rather than thinking about how to *modify* data.

### Expressions

The simplest form of data are numbers, and basic arithmetic is one way of creating new numbers from old numbers.

#### Numbers and arithmetic operators

To play around with this, start the Haskell REPL (“read-eval-print-loop”) by running `ghci` (or maybe on [tryhaskell.org](https://tryhaskell.org/)), and enter some numbers, and some of the usual arithmetic operations:

```
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
```

At this point we can tell that the usual precedence rules apply (i.e. the [PEMDAS rule](https://en.wikipedia.org/wiki/Order_of_operations#Mnemonics)).

```
Prelude> 0 - 1
-1
```
Numbers can be negative…

```
Prelude> 2^10
1024
Prelude> 2^2^10
17976931348623159077293051907890247336179769789423065727343008115773267580550096
31327084773224075360211201138798713933576587897688144166224928474306394741243777
67893424865485276302219601246094119453082952085005768838150682342462881473913110
540827237163350510684586298239947245938479716304835356329624224137216
Prelude> (2^2)^10
1048576
```

…and also very large. By default, Haskell uses arbitrary precision integer arithmetic. Note that for this lecture, we will completely avoid and ignore floating point arithmetic.

In the last example we can see that Haskell interprets `a^b^c` as `a^(b^c)`, i.e. the power operator is *right associative*. It is worth noting that this information is not hard-coded in the compiler. Instead, when the operator is defined somewhere in a library, its associativity and precedence can be declared. We can ask the compiler about this information:

```
Prelude> :info (^)
(^) :: (Num a, Integral b) => a -> b -> a 	-- Defined in ‘GHC.Real’
infixr 8 ^
```
Let us ignore the first line (which is the type signature): The `r` in `infixr` tells us that the `(^)` operator is right-associative. And the number is the precedence; a higher number means that this operator binds more tightly.

**Exercise:** What associativity do you expect for `(+)` and `(-)`? Verify your expectation.

**Exercise:** Look up the precedences of the other arithmetic operations, and see how that corresponds to the PEMDAS rule.

#### Applying Functions

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

**Exercise**: Can you predict the result of the following?
```
Prelude> 1 + const 2 3 + 4
```

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

**Prefix operator application (Syntactic Sugar)**:
We can also go the other way, and use any operator as if it were a function, by wrapping it in parentheses:
```
Prelude> 1 + 1
2
Prelude> (+) 1 1
2
```

**The dollar operator (Non-Syntactic Sugar)**:
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


**Exercise:**
What is the result of
```
Prelude> (-) 5 $ div 16 $ (-) 10 $ 4 `div` 2
```

#### Booleans and branching

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

### Function abstraction

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

**Exercise (discussion):** Think of other programming language that have concepts called functions. Can you always replace a function call with the function definition? Does it change the meaning of the program?

**Exercise:** Write a function `absoluteValue` with one parameter. If the parameter is negative, returns its opposite number, otherwise the number itself.

**Exercise:** Write a function `isHalfRound` that checks if a number is divisible by 5, by checking whether the last digit is 0 or 5.

**Exercise:** Write a function `isEven` that checks if a number is divisible by 2, by checking whether the last digit is 0, 2, 4, 6, 8.

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
By the way, you can use infix operator syntax in function definitions as well:
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

**Exercise:** Write the function `digitSum` that sums up the digit of a natural number.

### Higher order functions

We created functions when we took expressions that followed a certain pattern, and abstracted over a number that occurred therein. But the thing we can abstract over does not have to be a simple number. It could also be a function! 

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

Clearly, we can abstract over over the argument here:
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
This is our first *higher order function*, and it is called as such because it is a function that take another function as an argument. More precisely, it is called a second-order function, because it takes a normal, i.e. first-order function, as an argument. Abstracting over a second order function yields a third order function, and so on. Up to [sixth-order functions](https://doi.org/10.1017/S0956796898003001) are seen in the wild.

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

### Anonymous functions

We defined a function `always1`, but it seems a bit silly to give a name to such a specialized and small concept. Therefore, Haskell allows us to define *anonymous functions* on the fly. The syntax is a backslash, followed by the parameter (or parameters), followed by the body of the function. So we can define `countDigits` and `sumDigits` without any helper functions like this:

```
Prelude> countDigits n = sumDigitsWith (\d -> 1) n
Prelude> sumDigits n = sumDigitsWith (\d -> d) n
```

These are also called *lambda abstractions*, because they are derived from the Lambda calculus, and the backslash is a poor imitation of the Greek letter lambda (λ).

### Higher-Order function definition

Lets look at the previous two definitions, and remember that when we define a function this way, we define what to replace the left-hand side with. But notice that the argument `n` is not touched at all by this definition! So we should get the same result if we simply omit it from the equation, right? And indeed, we can just as well write

```
Prelude> countDigits = sumDigitsWith (\d -> 1)
Prelude> sumDigits = sumDigitsWith (\d -> d)
```

It looks as if we just saved two characters. But what really just happened is that we shifted our perspective, and raised the level of abstraction by one layer. Instead of defining a `countDigits` as a function that takes a number and produces another number, we have defined `countDigits` as the result of instantiating the pattern `sumDigitsWith` with the function `(\d -> 1)`. At this level of thought, we do not care about the argument to `countDigits`, i.e. what it is called or so.

**Exercise:**
Which other recent definitions can be changed accordingly?

**Answer:** The definitions for `countCountDigits` and `sumSumDigits`:
```
Prelude> countCountDigits = twice countCountDigits
Prelude> sumSumDigits = twice sumDigits
```

### Currying

We have already seen functions that *receive* a function as an argument. The way we use `twice` or `sumDigitsWith` here, we can think of them as a function that *return* functions. And this brings us to the deep and beautiful explanation we write multiple arguments to functions the way we do: Because really, every function only ever has one argument, and returns another one.

We can *think* of `twice` has having two arguments (the function `f`, and the value `x`), but really, `twice` is a function that takes one argument (the function `f`), and returns another function, that then takes the value `x`. This “other” function is what we named in the above definition of `sumSumDigits`.

### The composition operator

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


### Laziness

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
-----

What is the deal about types?

Basic types, function type, tuples.

Algebraic data types.


