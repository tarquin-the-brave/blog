+++
title = "Monad Fail in Rust"
date = 2020-04-09T12:40:13+01:00
images = []
tags = []
categories = []
draft = true
+++

Boring Way:

```haskell
maybePos :: Int -> Maybe Int
maybePos x
  | x < 0 = Nothing
  | otherwise = Just x

posIntoList :: Int -> [Int]
posIntoList x = case maybePos x of
  Nothing -> []
  Just posX -> [posX]
```

cool way:
```haskell
maybePosM ::  MonadFail m => Int -> m Int
maybePosM x
  | x < 0 = fail "woops"
  | otherwise = return x

posIntoList :: Int -> [Int]
posIntoList = maybePosM

posIntoMaybe :: Int -> Maybe Int
posIntoMaybe = maybePosM

posIntoIO :: Int -> IO Int
posIntoIO = maybePosM
```

in rust:
- define a trait that the output type must adhere to
- method could take generic that implements that trait as a type
  parameter and return that.
