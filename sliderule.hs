{-- Copyright © Gil Moskowitz 2014
--}

import Data.Ratio
import qualified Data.List  as L
import qualified Numeric    as N

srPrec   =  3

data MarkType = Major | Minor | Tick | Offset deriving (Eq, Enum, Ord, Show)

data Location = Top | Slide | Bottom   deriving (Eq, Enum, Ord, Show)

data SRMark = SRMark {
  value    :: Float,
  distance :: Float,
  mark     :: MarkType,
  label    :: String
} deriving (Eq)

data Scale = Scale {
  name  :: String,
  loc   :: Location,
  marks :: [SRMark]
}

instance Ord SRMark where
  SRMark _ d _ _ <= SRMark _ e _ _ = d <= e

instance Show Scale where
  show x = (name x) ++ " (" ++ (show $ loc x) ++ "):\n" ++ (show $ marks x)

instance Show SRMark where
  show x = "\n" ++ indent (mark x) (label x) ++ "\t" ++ (show $ value x) ++ " (" ++ (show $ distance x) ++ ")"
                        where indent m l | m == Major  = "--- " ++ l
                                         | m == Minor  = "--  " ++ l
                                         | m == Tick   = "-   " ++ l
                                         | m == Offset = "\t"   ++ l
                                         | otherwise   = "    " ++ l

srOffset :: Float -> Float
srOffset val = logBase 10 val

arrayToFrac :: Fractional a => [a] -> a
arrayToFrac [] = 0
arrayToFrac [x] = x / 10
arrayToFrac (x:xs) = x / 10 + arrayToFrac (map (/ 10) xs)

arrayToInt :: Fractional a => [Int] -> a
arrayToInt [] = 0
arrayToInt [x] = fromIntegral x
arrayToInt (x:xs) = fromIntegral x + arrayToInt (map (*10) xs)

normalizeDigitsArray :: ([Int], Int) -> ([Int], Int)
normalizeDigitsArray (digits, intLength)
  | intLength < 0             = (0:digits, intLength + 1)
  | intLength > length digits = (digits ++ (take (intLength - length digits) $ repeat 0), intLength)
  | otherwise                 = (digits, intLength)

digitsToFloat :: Fractional a => ([Int], Int) -> a
digitsToFloat digitArray = (arrayToInt $ reverse $ fst parts) + (arrayToFrac $ map (fromIntegral) (snd parts))
  where normalized = normalizeDigitsArray digitArray
        parts = splitAt (snd normalized) $ fst normalized

roundTo :: (RealFrac a, Floating a) => a -> a -> a
roundTo prec number = fromIntegral (round (number * 10 ** prec)) * 10 ** (-prec)

truncateTo :: (RealFrac a, Floating a) => a -> a -> a
truncateTo prec number = fromIntegral (truncate (number * 10 ** prec)) * 10 ** (-prec)

e = exp 1

decimalDigit :: RealFloat b => Int -> b -> Int
decimalDigit i x = if i < length fracPart then fracPart !! i
                   else 0
                   where parts = N.floatToDigits 10 x
                         fracPart = 0 : (drop (snd parts) (fst parts))
{-- this doesn't work because of internal rounding
decimalDigit i x = truncate ((x - truncateTo (i - 1) x) * 10 ** i)
--}

tickC v | decimalDigit 1 v == 0 && decimalDigit 2 v == 0 = Major
        | v < 4 && decimalDigit 1 v /= 0 && decimalDigit 2 v == 0 = Major
        | v < 4                 && decimalDigit 2 v == 5 = Minor
        | decimalDigit 1 v /= 0 && decimalDigit 2 v == 0 = Minor
        | v `elem` [ pi, e ]                             = Offset
        | otherwise                                      = Tick

labelC v | decimalDigit 1 v == 0 && decimalDigit 2 v == 0 = show (truncate v)
         | v < 2 && decimalDigit 1 v /= 0 && decimalDigit 2 v == 0 = show $ decimalDigit 1 v
         | v == pi         = "π"
         | v == e          = "e"
         | otherwise       = "\t"

scaleC = Scale "C" Slide
         [ SRMark v (srOffset v) (tickC v) (labelC v) |
           v <- map (roundTo srPrec) $ L.sort (pi:e:[1.0, 1.01 .. 3.99] ++ [4.0, 4.05 .. 10])
         ]

scaleCI = Scale "CI" Slide
          [ SRMark v (1 + (srOffset $ 1/v)) (tickC v) (labelC v) |
            v <- reverse $ map (roundTo srPrec) $ L.sort (pi:e:[1.0, 1.01 .. 3.99] ++ [4.0, 4.05 .. 10])
          ]

scaleD  = Scale "D"  Bottom (marks scaleC)
scaleDI = Scale "DI" Bottom (marks scaleCI)

scaleR1 = Scale "√ (odd # digits)" Bottom
            [ SRMark v (srOffset (v ** 2)) (markT v) (label v) |
              v <- map (roundTo srPrec) $ L.sort ([1.0, 1.005 .. 1.995] ++ [2.0, 2.01 .. sqrt 10])
            ] where
              markT v | decimalDigit 1 v == 0 && decimalDigit 2 v == 0
                     && decimalDigit 3 v == 0                          = Major
                      | v < 2                 && decimalDigit 2 v `elem` [0,5]
                     && decimalDigit 3 v == 0                          = Major
                      | v < 2                 && decimalDigit 3 v == 0 = Minor
                      | decimalDigit 2 v `elem` [0, 5]
                     && decimalDigit 3 v == 0                          = Minor
                      | otherwise                                      = Tick
              label v | decimalDigit 1 v == 0 && decimalDigit 2 v == 0
                     && decimalDigit 3 v == 0                          = (show $ truncate v)
                      | decimalDigit 2 v == 0 && decimalDigit 3 v == 0 = show $ decimalDigit 1 v
                      | otherwise = ""

scaleR2 = Scale "√ (even # digits)" Bottom
            [ SRMark v (srOffset (v ** 2) - 1) (markT v) (label v) |
              v <- map (roundTo srPrec) $ L.sort (sqrt10 : [ truncateTo 2 sqrt10, truncateTo 2 sqrt10 + 0.01 .. 4.99] ++ [5.0, 5.02 .. 10])
            ] where
              sqrt10 = sqrt 10
              markT v | decimalDigit 1 v == 0 && decimalDigit 2 v == 0 = Major
                      | v < 5 && decimalDigit 2 v == 0                 = Major
                      | v < 5 && decimalDigit 2 v == 5                 = Minor
                      | decimalDigit 1 v == 5 && decimalDigit 2 v == 0 = Major
                      | decimalDigit 2 v == 0                          = Minor
                      | v == roundTo srPrec sqrt10                     = Offset
                      | otherwise                                      = Tick
              label v | decimalDigit 1 v == 0 && decimalDigit 2 v == 0 = (show $ truncate v)
                      | v < 5 && decimalDigit 2 v == 0                 = show $ decimalDigit 1 v
                      | v == roundTo srPrec sqrt10                     = "√10"
                      | otherwise                                      = ""

{--

scaleS = Scale "S" Slide
         [ SRMark v (srOffset $ asin v) (markT v) (label v) |
             v <- L.sort ([ degrees asin 0.1 .. 9.999 ] ++
--}

{--
scaleA :: Float -> Float
scaleA x = x ** 2

scaleB :: Float -> Float
scaleB x = x ** 2

scaleK :: Float -> Float
scaleK x = x ** 3

scaleL :: Float -> Float
scaleL x = log x

scaleLL3 :: Float -> Float
scaleLL3 x = exp x

scaleT :: Float -> Float
scaleT x = tan x
--}
