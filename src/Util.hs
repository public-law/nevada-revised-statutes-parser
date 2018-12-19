module Util where

(|>) :: a -> (a -> b) -> b
(|>) a f = f a


