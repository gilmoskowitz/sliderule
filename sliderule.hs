{- Copyright © Gil Moskowitz 2014
-}

import Data.Ratio
import qualified Data.List  as L
import qualified Numeric    as N

e        = exp 1
srPrec   =  3

srEps :: Double
srEps    = 10 ^^ (-srPrec)

data MarkType = Major | Minor | Tick | Offset deriving (Eq, Enum, Ord, Show)

data Location = Top | Slide | Bottom   deriving (Eq, Enum, Ord, Show)

data SRMark = SRMark {
  value    :: Double,
  distance :: Double,
  mark     :: MarkType,
  label    :: String
} deriving (Eq)

data Scale = Scale {
  name  :: String,
  loc   :: Location,
  marks :: [SRMark],
  desc  :: String
} deriving (Show)

instance Ord SRMark where
  SRMark _ d _ _ <= SRMark _ e _ _ = d <= e

instance Show SRMark where
  show x = "\n" ++ indent (mark x) (label x) ++ "\t" ++ (show $ value x) ++ " (" ++ (show $ distance x) ++ ")"
                        where indent m l | m == Major  = "--- " ++ l
                                         | m == Minor  = "--  " ++ l
                                         | m == Tick   = "-   " ++ l
                                         | m == Offset = "   <" ++ l
                                         | otherwise   = "    " ++ l

srOffset :: Double -> Double
srOffset val = logBase 10 val

roundTo :: (Integral b, RealFrac a, Floating a) => b -> a -> a
roundTo prec number = fromIntegral (round (number * 10 ^^ prec)) * 10 ^^ (-prec)

truncateTo :: (Integral b, RealFrac a, Floating a) => b -> a -> a
truncateTo prec number = fromIntegral (truncate (number * 10 ^^ prec)) * 10 ^^ (-prec)

isWhole :: Double -> Bool
isWhole v = abs (v - fromIntegral (round v)) < srEps

isTenth :: Double -> Bool
isTenth x = isWhole (x * 10)

isHalf :: Double -> Bool
isHalf x = isWhole (x * 2)

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
decimalDigit i x = truncate ((x - truncateTo (i - 1) x) * 10 ^^ i)
--}

tickC v | isWhole v                     = Major
        | v < 4 && isTenth v            = Major
        | v < 4 && isHalf (v * 10)      = Minor
        | v >= 4 && isTenth v           = Minor
        | v `elem` [ pi, e ]            = Offset
        | otherwise                     = Tick

labelC v | isWhole v          = show (round v)
         | v < 2 && isTenth v = show $ decimalDigit 1 (roundTo srPrec v)
         | v == pi            = "π"
         | v == e             = "e"
         | otherwise          = "\t"

scaleC = Scale "C" Slide
         [ SRMark v (srOffset v) (tickC v) (labelC v) |
           v <- L.sort (pi:e:[1.0, 1.01 .. 3.99] ++ [4.0, 4.05 .. 10])
         ]
         "Baseline scale on the slide, running from 1 to 10."

scaleCI = Scale "CI" Slide
          [ SRMark v (1 + (srOffset $ 1/v)) (tickC v) (labelC v) |
            v <- reverse $ L.sort (pi:e:[1.0, 1.01 .. 3.99] ++ [4.0, 4.05 .. 10])
          ]
          "Inversion scale. To find 1/x, look for x on the C scale and read the inverse from the CI scale."

scaleD  = Scale "D"  Bottom (marks scaleC)
          "Baseline scale on the stator. To multiply, place 1 on the C scale above the multiplicand on the D scale, look for the multiplier on the C scale, and read the result from the D scale."

scaleDI = Scale "DI" Bottom (marks scaleCI)
          "Inversion scale. To find 1/x, look for x on the D scale and read the inverse from the DI scale."

scaleR1 = Scale "√ (odd # digits)" Bottom
          [ SRMark v (srOffset (v ^^ 2)) (markT v) (label v) |
            v <- L.sort ([1.0, 1.005 .. 1.995] ++ [2.0, 2.01 .. sqrt 10])
          ]
          "Square roots for values with an odd number of digits. To find √x, look for x on the D scale (or the C scale if R1 is on the slide) and read √x from the R1 scale."
          where
          markT v | isWhole v                   = Major
                  | v < 2 && isHalf (10 * v)    = Major
                  | v < 2 && isWhole (100 * v)  = Minor
                  | isHalf (10 * v)             = Minor
                  | otherwise                   = Tick
          label v | isWhole v           = show $ (round v)
                  | isWhole (10 * v)    = show $ decimalDigit 1 (roundTo srPrec v)
                  | otherwise           = ""

scaleR2 = Scale "√ (even # digits)" Bottom
          [ SRMark v (srOffset (v ^^ 2) - 1) (markT v) (label v) |
            v <- L.sort (sqrt10 : [ 3.17, 3.18 .. 4.99] ++ [5.0, 5.02 .. 10])
          ]
          "Square roots for values with an even number of digits. To find √x, look for x/10 on the D scale (or the C scale if R1 is on the slide) and read √x from the R1 scale."
          where
            sqrt10 = sqrt 10
            markT v | isWhole v                 = Major
                    | v < 5 && isTenth v        = Major
                    | v < 5 && isHalf  (v * 10) = Minor
                    | isHalf v                  = Major
                    | isTenth v                 = Minor
                    | v == sqrt10               = Offset
                    | otherwise                 = Tick
            label v | isWhole v                 = (show $ round v)
                    | v < 5 && isWhole (v * 10) = show $ decimalDigit 1 (roundTo srPrec v)
                    | v == sqrt10               = "√10"
                    | otherwise                 = ""

scaleS = Scale "S" Slide
         [ SRMark v (1 + (srOffset . sin . radians $ v)) (markT v) (label v) |
           v <- L.sort (
                       degrees (asin 0.1) : [5.75, 5.80 ..  9.95]
                       ++ [10,  10.1 .. 19.9 ] ++ [20,  20.2  .. 29.8 ]
                       ++ [30,  30.5 .. 59.49] ++ [60 .. 79] ++ [80, 85, 90]
                       )
         ]
         "Sine and Cosine for angles between ~6 and 90˚. To find sin x, look for Sx on the S scale and read sin x from the C scale (or D if S is on the stator). To find cos x, look for Cx on the S scale and read cos x from the C (D) scale."
         where
           markT v | v == degrees (asin 0.1) = Offset
                   | v <=10 = if isHalf v then Major
                              else if isWhole (v * 10) then Minor
                              else Tick
                   | v <=20 = if isWhole v     then Major
                              else if isHalf v then Minor
                              else Tick
                   | v < 60 = if isWhole v then Minor else Tick
                   | v <= 90.1 = if isWhole (v / 10) then Major
                              else if isHalf (v / 10) then Minor
                              else Tick
                   | otherwise = Tick
           label v | v < 10.01 && isWhole v = "C"  ++ show complement
                                           ++ " S" ++ show (round v)
                   | isWhole v && truncate v `elem` [ 15,20,25] ++ [30,40..80] = "C" ++ show complement
                                           ++ " S" ++ show (round v)
                   | truncate v == 90 = "S90"
                   | otherwise        = ""
                   where complement = 90 - round v

{-- TODO: Pickett marks scaleST with " at ~1.18˚ and ' at ~1.965˚. why?
          are they really for 11.9˚ and 20.1˚ on scaleS???
--}
scaleST = Scale "ST" Slide
          [ SRMark v ((srOffset . sin . radians) v - srOffset asin_01) (markT v) (label v) |
            v <- L.sort (
                [degrees asin_01, 0.58, 0.59] ++ [0.6, 0.62 .. 0.98] ++ [ 1, 1.02 .. 10 * (degrees asin_01) ]
                )
          ]
          "Sine for angles less than ~6˚ and cosine for angles close to 90˚. To find sin x, look for x on the ST scale and divide the C scale value by 100 (or D if ST is on the stator). Thus sin 1˚ = 1.745 / 100 = 0.01745."
          where
            asin_01 = asin 0.01
            markT v | v < 1 && isWhole (v * 10) = Major
                    | isWhole v                 = Major
                    | isHalf v                  = Major
                    | isWhole (v * 10)          = Minor
                    | v == degrees asin_01      = Offset
                    | otherwise                 = Tick
            label v | v < 1 && isWhole (v * 10) = "0." ++ show (round (v * 10))
                    | v == 1                    = "1˚"
                    | v < 3 && isHalf v         = show (roundTo 1 v)
                    | isWhole v                 = show (round v)
                    | otherwise                 = ""

scaleCF = Scale "CF" Slide
          [ SRMark v (srOffset (v / pi)) (markT v) (label v) |
            v <- L.sort (
                   [ pi, 10 * pi ]
                   ++ [  3.16, 3.18 .. 3.98 ] ++ [ 4,  4.05 ..  9.95 ]
                   ++ [ 10,   10.1 .. 19.9  ] ++ [20, 20.2  .. 31.2  ]
                 )
          ]
          "π to 10π. To find πx, find x on the C (or D) scale and read πx from the CF (or DF) scale."
          where
            markT v | v < 10 && isHalf  v       = Major
                    | v < 10 && isTenth v       = Minor
                    | isWhole (v / pi)          = Offset
                    | isWhole v                 = Major
                    | isHalf  v                 = Minor
                    | otherwise                 = Tick
            label v | v < 11 && isWhole v       = show $ round v
                    | isWhole (v / pi)          = show (round $ v / pi) ++ "π"
                    | v < 20 && isWhole v       = show $ decimalDigit 0 (roundTo 0 v)
                    | isWhole (v / 10)          = show $ round (v / 10)
                    | otherwise                 = ""

scaleDF = Scale "DF" Slide (marks scaleCF)
          "π to 10π. Multiply or divide using the C and D scales ignoring factors of π, then read the result off the corresponding CF scale. Thus to calculate 3/4 π, calculate 3 / 4 by aligning C4 over D3 and read 7.5 (/ 10) from the D scale. Then read ~2.36 off DF."

scaleT1 = Scale "T1" Slide
         [ SRMark v (srOffset . tan . radians $ v) (markT v) (label v) |
           v <- L.sort ( (degrees . atan $ 0.1) : [5.75, 5.80 .. 9.95 ]
                       ++ [10, 10.1  .. 29.9] ++ [30, 30.2 .. 45 ]
                       )
         ]
         "Tangent for angles between ~5.7˚ and 45˚. Find the angle x on T1, then tan x = C / 10."
         where
           markT v | isWhole v && v < 30 = Major
                   | isHalf  v && v < 10 = Major
                   | isTenth v && v < 10 = Minor
                   | isHalf  v && v < 30 = Minor
                   | isHalf (v / 10)     = Major
                   | isWhole v           = Minor
                   | otherwise           = Tick
           label v | isHalf (v / 10)     = show $ round v
                   | isWhole v && v < 10 = show $ round v
                   | otherwise           = ""

scaleT2 = Scale "T2" Slide
         [ SRMark v (srOffset . tan . radians $ v) (markT v) (label v) |
           v <- L.sort ( [45, 45.2  .. 58.8] ++ [60, 60.1 .. 79.9]
                       ++ [80, 80.05 .. 84.3]
                       )
         ]
         "Tangent for angles between 45˚ and 84.3˚. Find the angle x on T2 and read tan x from C."
         where
           markT v | isHalf (v / 10)  = Major
                   | isWhole v && v > 60 = Major
                   | isHalf  v && v > 60 = Minor
                   | isTenth v && v > 80 = Minor
                   | isWhole v        = Minor
                   | otherwise        = Tick
           label v | isHalf (v / 10)     = show $ round v
                   | isWhole v && v > 80 = show $ round v
                   | otherwise           = ""

{--
scaleA x = x ^^ 2
scaleB x = x ^^ 2
scaleK x = x ^^ 3
scaleL x = log x
scaleLL3 x = exp x
--}
