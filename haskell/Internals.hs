module Jbobaf.Internals where
 import Monad (liftM2)

 infixr 5 ~:, ~~, ~:~
 infixr 0 ?:, :?

 (~:) :: Monad m => a -> m [a] -> m [a]
 a ~: xs = xs >>= return . (a :)

 (~~) :: Monad m => m [a] -> m [a] -> m [a]
 (~~) = liftM2 (++)

 (~:~) :: Monad m => m a -> m [a] -> m [a]
 (~:~) = liftM2 (:)

 data TernaryBranch a = a :? a deriving (Eq, Ord, Read, Show, Bounded)

 instance Functor TernaryBranch where fmap f (x :? y) = f x :? f y

 (?:) :: Bool -> TernaryBranch a -> a
 True ?: (y :? _) = y
 False ?: (_ :? z) = z
