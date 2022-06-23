module Helpers(
	splitLast, interleave, takeLast, dropLast, 
	intToBitString, paddedIntToBitString, bitStringToInt,
	rotate
	) where

	import Numeric (showIntAtBase)
	import Data.Char (intToDigit, digitToInt)

	splitLast :: Eq a => a -> [a] -> Either [a] ([a],[a])
	splitLast c' = foldr go (Left [])
			where
					go c (Right (f,b)) = Right (c:f,b)
					go c (Left s) | c' == c = Right ([],s)
											| otherwise = Left (c:s)

	interleave :: [a] -> [a] -> [a]
	interleave xs ys = concat (zipWith (\x y -> x : [y]) xs ys)

	takeLast :: Int -> [a] -> [a]
	takeLast n = reverse . take n . reverse

	dropLast :: Int -> [a] -> [a]
	dropLast n = reverse . drop n . reverse

	intToBitString :: (Integral a, Show a) => a -> String
	intToBitString n = showIntAtBase 2 intToDigit n ""

	paddedIntToBitString :: (Integral a, Show a) => Int -> a -> String
	paddedIntToBitString pad n = replicate (pad - length s) '0' ++ s
		where s = intToBitString n

	bitStringToInt :: String -> Int
	bitStringToInt bits = go (map (\c -> read [c] :: Int) (reverse bits)) 0
		where 
			go []     _ = 0
			go (x:xs) n = (x*(2^n)) + go xs (n+1)

	rotate :: Int -> [a] -> [a]
	rotate = drop <> take