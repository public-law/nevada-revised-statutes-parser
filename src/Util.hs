module Util where

-- Elixir-style function chaining
(|>) :: a -> (a -> b) -> b
(|>) a f = f a


