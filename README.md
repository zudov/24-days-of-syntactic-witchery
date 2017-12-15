# 24 days of syntactic witchery
*aligning characters for fun and profit, with Haskell and PureScript*

## Day 1 — Prelude <a name="day-1"></a>
In this "24 days of…" I want to focus on tricks and patterns that are (at least to some extent):

- expressed using a relatively simple machinery
- capture some idea in a concise, but not necessarily intuitive way
- non-obvious, but can be discovered by accident or when striked with a bright idea
- neat in their own way
- mostly ungooglable

Some of these are widely used in every other codebase, other pop up occasionally in the dark corners. Some are charming, but when used inappropriately can seed confusion and havoc.

Even though the title says "syntactic", I'll steer away from talking about plain usage of syntactic sugar and GHC's syntactic extensions since they are already very well documented in [GHC User's Guide][ghc-syntactic-extensions] and excellent [24 Days of GHC Extensions][24-days-of-ghc-extensions].

Real witches do not rely on syntactic sugar, they cast thunderstorms of funny operators instead.

[ghc-syntactic-extensions]: http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#syntactic-extensions

[24-days-of-ghc-extensions]: https://ocharles.org.uk/blog/pages/2014-12-01-24-days-of-ghc-extensions.html

Obviously it's a work in progress. I don't yet have a full list of things to write about, which means that the topics won't be ordered very meaningfully. I might start running out of good ideas soon, and would have to [pick up `lens`](https://ro-che.info/ccc/23).

Guest posts are very welcome, and don't have to be in Haskell/PureScript as long as they fit the idea.

## Day 2 — `[ a | cond ]` <a name="day-2"></a>

This simple spell allows to conditionally construct an empty or single-element list:

```haskell
λ> [ 42 | True ]
[42]
λ> [ 42 | False ]
[]
```

It uses a small subset of the [list comprehension] syntactic sugar.

[list comprehension]: https://wiki.haskell.org/List_comprehension

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

As any proper spell this one might be confusing and non-obvious for someone who haven't encountered it before. But when used wisely it provides a good structure and easy to grasp code-pattern which justifies it.

There are many more crazy things that one can do with Haskell's list comprehensions, you can read more about them [here](https://ocharles.org.uk/blog/guest-posts/2014-12-07-list-comprehensions.html). PureScript, on the other hand, choses to not have list comprehension sugar.

In the next section I'll show another pattern that relies on a funky operator and allows to express the same idea in a more generic way.

## Day 3 — `a <$ fb` <a name="day-3"></a>

The `<$` operator is provided by both [Haskell's][haskell <$] and [PureScript's][purescript <$] Preludes:

[haskell <$]: https://hackage.haskell.org/package/base-4.10.0.0/docs/Prelude.html#v:-60--36-
[purescript <$]: https://pursuit.purescript.org/packages/purescript-prelude/3.1.0/docs/Data.Functor#v:(%3C$)

```haskell
(<$) :: Functor f => a -> f b -> f a
```

The idea is that it takes an `f b` and replaces all the `b` values in it with the `a` that was given to it, resulting in `f a`.

It can be easily defined in terms of `<$>` (aka `fmap`):

```haskell
a <$ fb = const a <$> fb
```

And used for all of your favourite functors:

```haskell
λ> 42 <$ [1, 2, 3]
[42,42,42]
λ> 42 <$ Just 0
Just 42
λ> 42 <$ Nothing
Nothing
λ> 42 <$ Right 0
Right 42
λ> 42 <$ Left "come 40 million years later"
Left "come 40 million years later"
λ> 42 <$ putStrLn "Computing the ultimate answer"
Computing the ultimate answer
42
```

Until today (when I finally read the docstring) my misintuition about `<$` has been something like "evaluates the stuff on the right side, throws away the result and returns the value on the left side".

It's handy for returning the value while logging the act of returning it:

```haskell
gift <$ putStrLn "HERE'S YOUR GIFT, BOY. HO! HO! HO!"
```

You can also combine it with `do-notation` thus avoiding the `pure`/`return` in the ond of it:

```haskell
-- | Gets reindeer ready for action.
initReindeer :: IO ReindeerStatus
initReindeer = ReindeerReady <$ do
  putStrLn "Attach the sledges"
  putStrLn "Turn the bells on"
  putStrLn "Open the stable doors"
```

I find this particularly handy when implementing ["eval" functions for `purescript-halogen`][halogen-eval]:

[halogen-eval]: https://github.com/slamdata/purescript-halogen/blob/master/docs/2%20-%20Defining%20a%20component.md#evaluating-actions

```haskell
Toggle next -> next <$ do
  state <- H.get
  let nextState = not state
  H.put nextState
  H.raise $ Toggled nextState
```

Combined with `guard`, we can replicate the success of [`[ a | cond ]`](#day-2) with no sugar:

```haskell
-- | Returns a gift for a given kid (if they deserve it).
giftFor :: Kid -> Maybe Gift
giftFor kid =
  requestedGift kid <$ guard (isGoodKid kid)
```

Note that the above expression in the body of this function can return a list, `Maybe`, `IO` (throws an exception for bad kids), or any other type that implements the `Alternative`.

One could achieve the same generality using [`[ a | cond ]`](#day-2) and enabling `MonadComprehensions` extension, but I'll leave that to muggles.

## Day 4 — `f <$> fa` <a name="day-4"></a>

`<$>` operator is a handy alias for our favourite `map`/`fmap` whose definition makes up a `Functor` typeclass.

It's used a lot in Haskell and PureScript alike. It's an important stepping stone to more elaborate tricks, and a building block for many combinators.

```haskell
(<$>) :: Functor f => (a -> b) -> f a -> f b
```

Taking a function (`a -> b`) and an `f a` functor, `fmap` applies that function to every `b` inside it.

"Container types" are most intuitive `Functor`s, quite literally their `fmap` applies the given function to every element that's contained in there.

```haskell
λ> (+1) <$> Just 2017
Just 2018
λ> fmap (gregorianMonthLength 2017) [1..12]
[31,28,31,30,31,30,31,31,30,31,30,31]
```

It's also more-or-less intuitive to think about `IO` and other effectful actions as something that holds a value:

```haskell
λ> map Char.toUpper <$> getLine
ho ho ho
"HO HO HO"
```

However don't be surpised if that intuition doesn't work for some types:

```haskell
λ> ((+2000) <$> (+18)) 0
2018
```

To learn more about functors, I recommend reading [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia#Functor).

As shown above, I prefer using `fmap`/`map` function when mapping over multi-element data-structures. `<$>` shines when applying functions to values in effectful contexts or things like `Maybe` or `Either`. The second case is more synonymous to function application, whereas the first actually relates to the logic of the program and I prefer to explicitly point out that I map over every element of a structure.

`<$>` is quite descriptive and helpful when it's needed to wrap result of some action in a newtype before binding it to a name:

```haskell
username <- Username <$> getEnv "USERNAME"
password <- Password <$> getEnv "PASSWORD"
```

Or extracting a part of the value:

```haskell
today <- utctDay <$> getCurrentTime
```

`<$>` will be popping up quite often in the upcoming chapters. Tomorrow we'll look at it's friend `flap`/`<@>` and then at some point we'll get to their ally `<*>` and see how they all can work together.

## Day 5 — `ff <@> a` <a name="day-5"></a>

While writing the previous posts I noticed that Purescript's `Data.Functor` module defines an interesting function called `flap`, which is also availabe via operator named `<@>`.

```haskell
(<@>) :: Functor f => f (a -> b) -> a -> f b
```

Takes a function in a context of `f` and applies it to a normal value `a`.

Notice that it's a lot like [`<$>` (`fmap`)](#day-4) only now first argument, the `a -> b` function is wrapped in an `f` functor, while the second argument is just a normal `a` value.

Here is how it could be defined in haskell:

```haskell
flap :: Functor f => f (a -> b) -> a -> f b
flap ff x = (\f -> f x) <$> ff

(<@>) :: Functor f => f (a -> b) -> a -> f b
(<@>) = flap

infixl 4 <@>
```


Note that `flap` generalizes a more well-known [`flip`](https://pursuit.purescript.org/packages/purescript-prelude/3.1.0/docs/Data.Function#v:flip) function.

See, if we start with the type of `flap`:

```haskell
f (b -> c) -> b -> f c
```

and substitute `a ->` for the `f`, we'll get:

```haskell
(a -> b -> c) -> b -> a -> c
```

Which is exactly the type of `flap`. We can indeed use it as if it was `flip`:

```haskell
λ> flap (++) "foo" "bar"
"barfoo"
```

Another simple thing we can do is applying several functions to the same value:

```purescript
λ> [maximum, minimum] <@> [5,7,1,9,3,2,6]
[9,1]
λ> [add 1, add 2, add 3] <@> 0
[1,2,3]
```

But combining it with `<$>` makes it more intersting:

```haskell
λ> add <$> [1,2,3] <@> 0
[1,2,3]
```

A neat thing is going on here. We first do `add <$> [1,2,3]` which is equivalent to `[add 1, add 2, add 3]`.
Then we apply each of those functions to `0` using `<@>` operator.

Here's a slightly more useful application of lists:

```haskell
-- | Every christmas since year 2000th.
christmases :: [Day]
christmases =
  fromGregorian <$> [2000..] <@> 12 <@> 25
```

Works equally well for IO:

```haskell
thisChristmas :: IO Day
thisChristmas =
  fromGregorian <$> getCurrentYear <@> 12 <@> 25  
```

Using `f <$> fa <@> b <@> c` pattern we can give function `f` a value which is wrapped in a functor along with a couple more normal values. `f` can be of any arity, we'll just chain the `<@>`.

However we still lack an ability to apply a function to several non-pure values. Next we'll see how it can be done with a help of `<*>`.


## Day 6 — `fa <*> fb` <a name="day-6"></a>

"Apply over" operator — `<*>`, brings the best of `<$>` and `<@>` together:

```haskell
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
```

We stepped up from `Functor` to `Applicative`, and now both the `a -> b` function and `a` value are wrapped in the context of `f`:

```haskell
λ> Just not <*> Just False
Just True
λ> Nothing <*> Just False
Nothing
```

Combining it with `<$>` and `<@>` allows us to lift functions with arbitrary amount of arguments while still passing some pure values in there:

```haskell
*Main> (\wish name -> wish ++ ", " ++ name) <$> ["Merry Christmas", "Happy new year"] <*> ["Foo", "Bar"]
["Merry Christmas, Foo","Merry Christmas, Bar","Happy new year, Foo","Happy new year, Bar"]
```

```haskell
-- | Every Friday 13th since year 2000th.
evilFridays :: [Day]
evilFridays =
  filter isFriday $ fromGregorian <$> [2000..] <*> [1..12] <@> 13
```

There are many useful applications for `Applicative`. Context-free parsers and validators are among my favourite.

Chaining `<$>`, `<@>`, `<*>` is cute, but it gets a bit hard to follow with complex expressions. Fortunately, both Haskell and PureScript now support `ApplicativeDo` notation.

The above example could actually be rewritten as:

```haskell
evilFridays :: [Day]
evilFridays = filter isFriday $ do
  year   <- [2000..]
  month  <- [1..12]
  pure $ fromGregorian year month 13
```

## Day 7 — `fa *> fb` / `fa <* fb`

Both operators `<*` and `*>` allow to combine two applicative actions in such a way that both are executed but only one is returned. `<*` returns the result of the first, `*>` the result of the second.

Both can be defined in terms of `<*>`.

For lists, the behaviour might seem a bit strange:

```haskell
> ["foo", "bar"] <* [1..3]
["foo","foo","foo","bar","bar","bar"]
```

Every element of the right list is replaced with the left list and the resulting list of lists is concatenated.

For `Maybe`, both branches gets evaluated and if both are `Just` the result of the one to which the arrow points is returned:

```haskell
> Just 'a' *> Just 'b'
Just 'b'
> Nothing *> Just "foo"
Nothing
```

I find these operators most pleasant when it's applied in "stateful" applicatives, for example IO:

```haskell
λ> reply <- putStr "Were you a good kid?" *> getLine <* putStrLn "HO! HO! HO!"
Were you a good kid?
yes
HO! HO! HO!
λ> reply
"yes"
```

or applicative regexps (via [`regex-applicative` library][regexp-applicative]:

[regexp-applicative]: https://hackage.haskell.org/package/regex-applicative

```haskell
> "@username" =~ "@" *> many (psym isAlphaNum)
Just "username"
```

```haskell
> let hashtag = "#" *> many (psym isAlphaNum)
> "#foo" =~ hashtag
Just "foo"
```

```haskell
> let hashtags = many (few anySym *> hashtag <* few anySym)
> "Yo, #foo bar #baz, #quux" =~ hashtags
Just ["foo","baz,","quux"]
```

```
parsePrice :: String -> Maybe Double
parsePrice =
  match $
    optional ("$" <* optional " ")  -- can start with dollar and optional space
      *> (read <$> value) <*        -- has a value in the middle
    optional (optional " " *> "$")  -- can end with dollar and optional space
  where
    value = do
      dollars <- integral
      cents   <- optional fractional
      pure (dollars ++ maybe "" ('.' :) cents)

    integral   = many digit
    fractional = sym '.' *> replicateM 2 digit

    digit = psym Char.isDigit
```

# Day 8 — `<>`

The "append" operator, `<>` originates from `Monoid` typeclass, but a more precise place for it is `Semigroup`.
These days you can get it from `Data.Semigroup` both in Haskell and PureScript.

```purescript
class Semigroup a where
  (<>) :: a -> a -> a
```

`<>` combines two values of the same type. This "combine" operation can be pretty much anything as long as it's associative:

```haskell
(x <> y) <> z = x <> (y <> z)
```

Haskell's [`Data.Semigroup`](https://hackage.haskell.org/package/base-4.10.1.0/docs/Data-Semigroup.html) and [`purescript-monoid`](https://pursuit.purescript.org/packages/purescript-monoid/) defines a number of useful `Semigroup`s.

##### Appending normal things

The most natural usage is appending things in the common sense of most programming languages:

```haskell
> "abc" <> "def"
"abcdef"
> [1..3] <> [4..6]
[1,2,3,4,5,6]
> Set.fromList [1..4] <> Set.fromList [3..6]
fromList [1,2,3,4,5,6]
> Map.fromList (zip [1..4] ["a".."d"]) <> Map.fromList (zip [3..6] ["C".."G"])
fromList [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'E'),(6,'F')]
```

Note that in the last case values from the first `Map` win over the values of the second `Map`.

One might need to append two `Map`s while merging the values under colliding keys. This can be done via `Map.unionWith (<>)`.

Our favourite fizzbuzz could be written like that:

```haskell
fizzbuzz :: Int -> String
fizzbuzz n = concat (fizz <> buzz <> num)
  where
    fizz, buzz, num :: [String]
    fizz = "Fizz" <$ guard (n `mod` 3 == 0)
    buzz = "Buzz" <$ guard (n `mod` 5 == 0)
    num  = show n <$ guard (null fizz && null buzz)
```

`fizz`, `buzz` and `num` are simply lists of strings that are being appended together via `<>`.

##### Appending maybe things

Given a `Semigroup` for `a`, we can get a free `Semigroup` for `Maybe a`:

```
class Semigroup a => Semigroup (Maybe a) where
  Nothing <> Nothing = Nothing
  Just a  <> Nothing = a
  Nothing <> Just b  = b
  Just a  <> Just b  = Just (a <> b)
```

This allows us to explicitly describe the "default" case in fizzbuzz:

```haskell
fizzbuzz :: Int -> String
fizzbuzz n = fromMaybe (show n) (fizz <> buzz)
  where
    fizz, buzz :: Maybe String
    fizz = "Fizz" <$ guard (n `mod` 3 == 0)
    buzz = "Buzz" <$ guard (n `mod` 5 == 0)	
```

Now `fizz` and `buzz` are `Maybe String`, we append them to each other and if that is `Nothing` we just return the default `show n`.

##### Appending functions

For every `Semigroup` `b`, we can also get a free `Semigroup` for `a -> b`:

```haskell
class Semigroup b => Semigroup (a -> b) where
  f <> g = \a -> f a <> g a
```

The result of appending two functions is a function that passes a value to both and appends the results.

```
-- | Combines the gifts that should be sent to a given kid.
kidGift :: Kid -> Maybe Gift
kidGift = goodKidGift <> candyGift

-- | Good kids get requested gifts. Bad ones get nothing.
goodKidGift :: Kid -> Maybe Gift
goodKidGift kid =
  requestedGift kid <$ isGoodKid kid

-- | We attach some fresh candies for every kid.
--   Bad ones get old uneatable candies and salmiaki (unless they are from Finland).
candyGift :: Kid -> Maybe Gift
candyGift kid =
  (freshCandies <$ isGoodKid kid)
    <>
  (salmiakki <$ guard (not (isGoodKid kid) && not (fromFinland kid)))
    <>
  (oldCandies <$ guard (not (isGoodKid kid)))
```


##### Higher-order appends

As show above with `Map.unionWith (<>)` it's common to pass `<>` to other higher-order functions.

Some of those patterns are so common that there are shorthands for them.

`fold` folds a structure and combines all elements:

```haskell
> let fold = foldl (<>) mempty
> fold ["foo", "bar", "baz"]
"foobarbaz"
```

`foldMap` allows to first apply a function to all of them:

```haskell
> let foldMap f = fold . map f
> foldMap Product [1..10]
Product {getProduct = 3628800}
```

# Day 12 — `<|>` <a name="day-12"></a>

The "alt" operator (<|>) comes from `Alternative` class. In PureScript, more precise `Alt` class is available.

```
class Alt f where
  (<|>) :: f a -> f a -> f a
```

It's very similar to the `<>` operator, it also combines two values, only its defined for `* -> *` "container" types.
It also has to be associative, but in addition to that it also must be distributive:

```
f <$> (x <|> y) == (f <$> x) <|> (f <$> y)
```

Applying a function to a combination of two values is the same as combining those values after applying the function to each of them.

`<|>` works as append for lists/arrays:

```haskell
> ['a'..'c'] <|> ['D'..'F']
"abcDEF"
```

That's an interesting fact, but `<|>` is seldomly used like that.
For appending it's more common to use `<>`.

`<|>` is very handy for picking one of the `Maybe` values:

```haskell
> Nothing <|> Just 'a' <|> Just 'b'
Just 'a'
```

If you have many alternatives you might prefer to use `asum` (Haskell) or `oneOf` (PureScript):

```purescript
> asum [ Nothing, Just 'a', Just 'b' ]
```

Alt comes handy for matching alternatives of [regexps][regexp-applicative]:

```haskell
data Currency = EUR | GBP | USD
  deriving (Show)

currency :: RE Char Currency
currency =
  USD <$ ("$" <|> "USD")
    <|>
  EUR <$ ("€" <|> "EUR")
    <|>
  GBP <$ ("£" <|> "GBP")
```

Or even swallowing any `IOException` in a hideous way (may be good for scripts):

```haskell
> readFile "/etc/shadow" <|> pure "Ooops. Operation failed."
"Ooops. Operation failed."
```

Or race two [async](https://hackage.haskell.org/package/async) processes:

```haskell
> runConcurrently $
    Concurrently (threadDelay 5000 *> print 5000)
      <|>
    Concurrently (threadDelay 1000 *> print 1000)
1000
```

[`purescript-aff`](https://github.com/slamdata/purescript-aff) steps in the game with examples from its README:

```purescript
example = do
  result <- Ajax.get "http://foo.com" <|> Ajax.get "http://bar.com"
  pure result
```

```purescript
-- Make a request with a 3 second timeout
example =
  sequential $ oneOf
    [ parallel (Just <$> Ajax.get "https://foo.com")
    , parallel (Nothing <$ delay (Milliseconds 3000.0))
    ]
```

## Day 14 — `>>=` / `=<<`

The `bind` operation has two directional operators, `>>=` and `=<<`. This operators are so common that one of them became a part of Haskell logo.

As usually, I give a couple of examples that use some of our favourite monads:

Lists are concatenated like via `concatMap`:

```haskell
λ> show =<< [1..10]
"12345678910"
```

For `Maybe` the `Just` values are threaded through, short-circuting on `Nothing`:

```haskell
λ> Just "foobarbaz" >>= Text.stripPrefix "foo" >>= Text.stripSuffix "baz"
Just "bar"
```

Nested property access in json data:

```purescript
(.?) :: forall a. DecodeJson a => JObject -> String -> Either String a
```

```purescript
object .? "foo" >>= \foo -> foo .? "bar" >>= \bar -> bar .? "baz"
```

Though in this case it's much more readable to use do-sugar:

```purescript
do foo <- object .? "foo"
   bar <- foo .? "bar"
   bar .? "baz"
```


You can use destructuring functions like `maybe` or `either` to conviniently match
in the binding function.

```haskell
requestLaunchPermit >>= maybe abortLaunch (\permit -> igniteEngine (enginePermit permit))
```

Though using `case` syntax (perhaps via `LambdaCase`) is not bad and sometimes better as well:

```haskell
requestLaunchPermit >>= \case
  Nothing -> abortLaunch
  Just permit -> igniteEngine (enginePermit permit)
```
