## `a <$ fb`

The `<$` operator is provided by both [Haskell's][haskell <$] and [PureScript's][purescript <$] Prelude.

[haskell <$]: https://hackage.haskell.org/package/base-4.10.0.0/docs/Prelude.html#v:-60--36-
[purescript <$]: https://pursuit.purescript.org/packages/purescript-prelude/3.1.0/docs/Data.Functor#v:(%3C$)

This 'void right' or 'less dollars' operator, has the following signature:

```haskell
(<$) :: Functor f => a -> f b -> f a
```

The idea is that it takes an `f b` and replaces all the `b` values in it with the `a` that was given to it, resulting in `f a`.

It can be easily defined in terms of `<$>` (aka `fmap`):

```haskell
a <$ fb = const a <$> fb
```

And used for all of your favourite functors.

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

My misintuition about `<$` has been something like "evaluates the stuff on the right side, throws away the result and returns the value on the left side", because it's common to use it for "return the value while logging the act of returning it":

```haskell
gift <$ putStrLn "HERE'S YOUR GIFT, BOY. HO! HO! HO!"
```

You can also combine it with `do-notation` thus avoiding the `pure`/`return` in the ond of it:

```haskell
prepareReindeer :: IO ReindeerStatus
prepareReindeer = RendeerReady <$ do
  putStrLn "Attach the sledges"
  putStrLn "Turn the bells on"
  putStrLn "Open the stable doors"
```

I find this particularly handy when implementing ["eval" functions with `purescript-halogen`][halogen-eval]:

[halogen-eval]: https://github.com/slamdata/purescript-halogen/blob/master/docs/2%20-%20Defining%20a%20component.md#evaluating-actions

```haskell
Toggle next -> next <$ do
  state <- H.get
  let nextState = not state
  H.put nextState
  H.raise $ Toggled nextState
```


Combined with `guard`, we can replicate the success of `[ a | cond ]` without zero sugar:

```haskell
requestedGift kid <$ guard (isGoodKid kid)
```

Note that the above expression can return a list, `Maybe`, `IO` (throws an exception for bad kids), or any other type that implements the `Alternative`.

One could achieve the same with `[ a | cond ]` by enabling `MonadComprehensions` extension, but syntax sugar and extensions are for muggles.
