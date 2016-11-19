# Diet Coke #
A small functional language with effect inference based on [Koka][koka].

### Quick Setup (macOS) ###

`brew install sbt rlwrap`

## Requirements ##

Diet Coke is built using Scala 2.11.7 and depends only on Scala Parser Combinators. In
order to make the most use of it, you should have `sbt` installed, and the `repl.sh` also depends on `rlwrap`.

## Using Diet Coke ##

Diet Coke consists of both a single file interpreter and a REPL. The interpreter simply takes a path to a single file and interprets it (n.b. this requires an entrypoint named main). The REPL is a fairly straightforward console, featuring only three commands (`:multiline`, `:type`, and `:quit`). You can toggle multiline input mode on and off using `:m on` and `:m off` (or the long-form `:multiline`). You can query the type of an arbitrary expression using `:type expr` or `:t expr` for short. Finally, you can quit the console using `:quit` or `:exit`. The REPL will also bind the result of every computation to a fresh result variable. To run a single file, use `sbt "run file.coke"` and to run the REPL, use `repl.sh`.

## Syntax ##

```
expr ::= id
       | n
       | true | false
       | string
       | op1 expr | expr op2 expr
       | builtIn(expr*) | expr(expr*)
       | error string
       | fix id => expr | id => expr
       | if expr then expr else expr
       | { expr [; expr]* }

builtIn ::= show
          | print
          | println
          | catch
          | inject
          | random

op1 ::= ! | -

op2 ::= + | - | * | / | %
      | == | /= | > | < | >= | <= |
      | && | || | ^

typ ::= () | num | bool | string | id | typ -> typ

stmt ::= let id = expr
       | fn id(id*) = expr
       | expr
       | stmt; stmt
       | type id :: typ
```

## Example Programs with REPL Sessions ##

#### Program ####

```
let x = 10
x * x
```

#### Session ####

```
λ let x = 10
λ x * x
let res4: num = 100
```

#### Program ####

```
fn randRange(x) = random() % x
```

#### Session ####

```
λ fn randRange(x) = random() % x
λ :t randRange
randRange :: (num -> <ndet | a> num)
λ randRange(5)
let res5: num = -4
λ randRange(10)
let res6: num = 8
```

#### Program ####

```
fn randRangeErr(x) =
  if x < 0 then 
    error "invalid input"
  else
    random() % x
```

#### Session ####

```
λ fn randRangeErr(x) = if x < 0 then error "invalid input" else random() % x
λ :t randRangeErr
randRangeErr :: (num -> <ndet, exn | a> num)
λ randRange(10)
let res7: num = 8
λ randRange(32)
let res8: num = -27
```

#### Program ####

```
fn fact(x) =
  if x < 0 then
    error "undefined"
  else if x == 0 then
    1
  else
    x * fact(x - 1)
```

#### Session ####

```
λ :m on
λ fn fact(x) =
|   if x < 0 then
|     error "undefined"
|   else if x == 0 then
|     1
|   else
|     x * fact(x - 1)
| 
λ :m off
λ :t fact
fact :: (num -> <exn | a> num)
λ fact(5)
let res15: num = 120
λ fact(10)
let res16: num = 3628800
```

#### Program ####

```
fn trace(x) = ({ println(show(x)); x })()

fn wild(x) =
  if x == 0 then
    error "bad"
  else if x > 0 then
    trace(14)
  else
    random()
```

#### Session ####

```
λ fn trace(x) = ({ println(show(x)); x })()
λ :t trace
trace :: (a -> <io | b> a)
λ trace(30)
30
let res9: num = 30
λ :m on
λ fn wild(x) =
|   if x == 0 then
|     error "bad"
|   else if x > 0 then
|     trace(14)
|   else
|     random()
| 
λ :m off
λ :t wild
wild :: (num -> <ndet, io, exn | a> num)
λ wild(10)
14
let res10: num = 14
λ wild(10)
14
let res11: num = 14
λ wild(0)
(0): bad
λ wild(-32)
let res12: num = 2027454890
λ wild(-32)
let res13: num = -536856153
```

## Miscellaneous ##

### To-Do List ###

- [ ] Fix bug with typing inject and catch builtins.
- [ ] Fix bug causing type errors to be order-dependent in multi-statement files.
- [ ] Implement explicit effect annotations.

### Full Citation ###
```
Daan Leijen. Koka: Programming with row polymorphic effect types. 
In Paul Levy and Neel Krishnaswami, editors, Proceedings 5th Workshop on 
Mathematically Structured Functional Programming, MSFP 2014, Grenoble, 
France, 12 April 2014, pages 100–126, 2014.
```

[koka]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/paper-20.pdf "Koka: Programming with Row-polymorphic Effect Types"
