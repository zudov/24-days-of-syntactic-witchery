
## `[ a | cond ]`

This trick allows to conditionally construct an empty or single-element list,
via [list comprehension] syntactic sugar.

[list comprehension]: https://wiki.haskell.org/List_comprehension

```haskell
λ> [ 42 | True ]
[42]
λ> [ 42 | False ]
[]
```

Is there any use for it? Bundled with `concat` it provides a nice way of constructing
lists where particular elements should be included only if a set of conditions is satisfied:

```haskell
λ> concat [ [ "foo" | True ], [ "bar" | True, False ], [ "baz" | True ] ]
["foo","baz"]
```

But what's that good for?

Let's say you want to conditionally add several css classes to an html element (uses [lucid]):

[lucid]: https://github.com/chrisdone/lucid

```haskell
import Lucid

todoHtml :: Todo -> Html ()
todoHtml todo =
  div_
    [ classes_ $ concat
        [ [ "todo" ]
        , [ "completed" | isCompleted todo ]
        , [ "active"    | isActive todo ]
        , [ "urgent"    | isActive todo, deadlineSoon   todo ]
        , [ "wasted"    | isActive todo, deadlineMissed todo ]
        ]
    ]
    (todoTitle todo)
```

Or you want to list yourself out of boring interview questions:

```haskell
main :: IO ()
main = mapM_ (putStrLn . fizzbuzz) [1..100]
  where
    fizzbuzz n = concat (fizz ++ buzz ++ num)
      where
        fizz = [ "Fizz" | n `mod` 3 == 0 ]
        buzz = [ "Buzz" | n `mod` 5 == 0 ]
        num  = [ show n | null fizz, null buzz ]
```

As any proper spell this one might be confusing and non-obvious for someone who haven't
encountered it before. However in cases above it gives a good structure
and easy to grasp code pattern, which I believe justifies it.

Next we'll look at generalization of this pattern that doesn't require list comprehensions,
but simply uses a funky operator.
