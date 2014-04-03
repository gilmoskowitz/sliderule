{-- Copyright © Gil Moskowitz 2014
--}

import Data.Ratio
import qualified Data.List  as L
import qualified Numeric    as N

srPrec   =  3
srEps    = 10 ** (-srPrec)
e        = exp 1

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
  marks :: [SRMark],
  desc  :: String
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

isWhole :: Float -> Bool
isWhole v = v - fromIntegral (truncate v) < srEps

degrees :: Floating a => a -> a
degrees r = r * 180 / pi

radians :: Floating a => a -> a
radians d = d * pi / 180

decimalDigit :: RealFloat b => Int -> b -> Int
decimalDigit i x | (-i) >= length intPart = 0
                 | i <= 0                = intPart  !! (length intPart + i - 1)
                 | i <= length fracPart  = fracPart !! (i - 1)
                 | otherwise             = 0
                 where parts = N.floatToDigits 10 x
                       (intPart, fracPart) = splitAt (snd parts) (fst parts)
{-- this doesn't work because of internal rounding
decimalDigit i x = truncate ((x - truncateTo (i - 1) x) * 10 ** i)
--}

tickC v | isWhole v                               = Major
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
         "Baseline scale on the slide, running from 1 to 10."

scaleCI = Scale "CI" Slide
          [ SRMark v (1 + (srOffset $ 1/v)) (tickC v) (labelC v) |
            v <- reverse $ map (roundTo srPrec) $ L.sort (pi:e:[1.0, 1.01 .. 3.99] ++ [4.0, 4.05 .. 10])
          ]
          "Inversion scale. To find 1/x, look for x on the C scale and read the inverse from the CI scale."

scaleD  = Scale "D"  Bottom (marks scaleC)
          "Baseline scale on the stator. To multiply, place 1 on the C scale above the multiplicand on the D scale, look for the multiplier on the C scale, and read the result from the D scale."

scaleDI = Scale "DI" Bottom (marks scaleCI)
          "Inversion scale. To find 1/x, look for x on the D scale and read the inverse from the DI scale."

scaleR1 = Scale "√ (odd # digits)" Bottom
          [ SRMark v (srOffset (v ** 2)) (markT v) (label v) |
            v <- map (roundTo srPrec) $ L.sort ([1.0, 1.005 .. 1.995] ++ [2.0, 2.01 .. sqrt 10])
          ]
          "Square roots for values with an odd number of digits. To find √x, look for x on the D scale (or the C scale if R1 is on the slide) and read √x from the R1 scale."
          where
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
          ]
          "Square roots for values with an even number of digits. To find √x, look for x/10 on the D scale (or the C scale if R1 is on the slide) and read √x from the R1 scale."
          where
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

scaleS = Scale "S" Slide
         [ SRMark v (srOffset (sin (radians v))) (markT v) (label v) |
           v <- map (roundTo srPrec) $ L.sort (
                       [degrees (asin 0.1), 5.74 ]         ++ [5.75, 5.80 ..  9.95]
                       ++ [10,  10.1 .. 19.9 ] ++ [20,  20.2  .. 29.8 ]
                       ++ [30,  30.5 .. 59.49] ++ [60 .. 79] ++ [80, 85, 90]
                       )
         ]
         "Sine and Cosine for angles between ~6 and 90˚. To find sin x, look for Sx on the S scale and read sin x from the C scale (or D if S is on the stator). To find cos x, look for Cx on the S scale and read cos x from the C (D) scale."
         where
           markT v | v <=10 = if isWhole v then Major
                              else if decimalDigit 1 v == 5 && decimalDigit 2 v == 0 then Major
                              else if decimalDigit 1 v /= 0 && decimalDigit 2 v == 0 then Minor
                              else Tick
                   | v <=20 = if isWhole v then Major
                              else if decimalDigit 1 v == 5 && decimalDigit 2 v == 0 then Minor
                              else Tick
                   | v < 60 = if isWhole v then Minor else Tick
                   | v <= 90.1 = if isWhole (v / 10) then Major
                              else if decimalDigit 0 v == 5 then Minor
                              else Tick
                   | otherwise = Tick
           label v | v < 10.01 && isWhole v = "C"  ++ show complement
                                           ++ " S" ++ show (round v)
                   | isWhole v && truncate v `elem` [ 15,20,25] ++ [30,40..80] = "C" ++ show complement
                                           ++ " S" ++ show (round v)
                   | truncate v == 90 = "S90"
                   | otherwise     = ""
                   where complement = 90 - round v

scaleST = Scale "ST" Slide
          [ SRMark v (srOffset . sin . radians $ v) (markT v) (label v) |
            v <- map (roundTo srPrec) $ L.sort (
                [degrees (asin 0.01), 0.58, 0.59] ++ [0.6, 0.62 .. 0.88] ++ [ 1, 1.02 .. 10 * (degrees (asin 0.01)) ]
                )
          ]
          "Sine for angles less than ~6˚ and cosine for angles close to 90˚. To find sin x, look for x on the ST scale and divide the C scale value by 100 (or D if ST is on the stator). Thus sin 1˚ = 1.745 / 100 = 0.01745."
          where
            markT v | v < 1 && decimalDigit 2 v == 0                 = Major
                    | isWhole v                                      = Major
                    | decimalDigit 1 v == 5 && decimalDigit 2 v == 0 = Major
                    | decimalDigit 1 v /= 0 && decimalDigit 2 v == 0 = Minor
                    | otherwise                                      = Tick
            label v | v < 1 && decimalDigit 2 v == 0                 = "0." ++ show (truncate (v * 10))
                    | v < 3 && decimalDigit 1 v == 5 && decimalDigit 2 v == 0 = show (truncateTo 1 v)
                    | isWhole v = show (truncate v)
                    | otherwise = ""

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
