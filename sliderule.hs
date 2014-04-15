{- Copyright © Gil Moskowitz 2014
-}

import Data.Ratio
import qualified Data.List  as L
import qualified Numeric    as N

main = do
  let actualLength = 36
  let scales = [ scaleA
               , scaleB
               , scaleC
               , scaleCF
               , scaleCF_M
               , scaleCI
               , scaleD
               , scaleDF
               , scaleDF_M
               , scaleDI
               , scaleK1
               , scaleK2
               , scaleK3
               , scaleL
               , scaleLL0
               , scaleLL0I
               , scaleLL1
               , scaleLL1I
               , scaleLL2
               , scaleLL2I
               , scaleLL3
               , scaleLL3I
               , scaleR1
               , scaleR2
               , scaleS
               , scaleSH1
               , scaleSH2
               , scaleST
               , scaleT1
               , scaleT2
               , scaleTH
               ]
  let marks = (concat (map (\s -> distanceScaleToStr (scaleUp s actualLength 8))
           scales))
  putStr (L.unlines marks)

e        = exp 1
srPrec   =  3

infix 4 ~=
(~=) :: (Fractional a, Ord a) => a -> a -> Bool
0 ~= 0 = True
0 ~= x = abs x < 1e-10
x ~= 0 = abs x < 1e-10
x ~= y = abs (x - y) < x * 10 ^^ negate 5

data MarkType = Major | Minor | Tick | Offset deriving (Eq, Enum, Ord, Show)

data Location = Unspecified | Body | Slide   deriving (Eq, Enum, Ord, Show)

data Distance = Normalized Double
              | Distance  {
                  wholePart       :: Int,
                  numeratorPart   :: Double,
                  denominatorPart :: Int
                }
              deriving (Show)

instance Eq Distance where
  (Normalized x)   == (Normalized y)      = (x == y)
  (Distance w n d) == (Distance w' n' d') = (w == w') && (n / fromIntegral d == n' / fromIntegral d')
  _ == _ = False

instance Ord Distance where
  Normalized x <= Normalized y            = x <= y
  Distance w n d <= Distance w' n' d'     = w <  w'
                                         || (w == w' && (n / fromIntegral d <= n' / fromIntegral d'))

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

data DistanceMark = DistanceMark {
  dvalue    :: Double,
  ddistance :: Distance,
  dmark     :: MarkType,
  dlabel    :: String
} deriving (Eq)

data DistanceScale = DistanceScale {
  dname  :: String,
  dloc   :: Location,
  dmarks :: [DistanceMark],
  ddesc  :: String
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

instance Show DistanceMark where
  show x = "\n" ++ indent (dmark x) (dlabel x) ++ "\t" ++ (show $ dvalue x) ++ " (" ++ (show $ ddistance x) ++ ")"
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

inRange :: Ord a => a -> String -> a -> a -> Bool
inRange x str l u | l > u       = inRange x str u l
                  | str == "[]" = l <= x && x <= u
                  | str == "[)" = l <= x && x <  u
                  | str == "(]" = l <  x && x <= u
                  | str == "()" = l <  x && x <  u

isWhole :: Double -> Bool
isWhole v = abs (v - fromIntegral (round v)) ~= 0

isTenth :: Double -> Bool
isTenth x = isWhole (x * 10)

isHalf :: Double -> Bool
isHalf x = isWhole (x * 2)

degrees :: Floating a => a -> a
degrees r = r * 180 / pi

radians :: Floating a => a -> a
radians d = d * pi / 180

decimalDigit :: RealFloat b => Int -> b -> Int
decimalDigit i x | offset < 0   = 0
                 | offset >= length digits = 0
                 | otherwise               = digits !! offset
                 where (digits,  pointPos) = N.floatToDigits 10 x
                       offset = pointPos + i - 1
{-- this doesn't work because of internal rounding
decimalDigit i x = truncate ((x - truncateTo (i - 1) x) * 10 ^^ i)
--}

{- convert real x to a proper fraction with denominator d, where the numerator
   is a floating point number, not an integer
   This is used to convert scale output to fractions of an inch
-}
normalizeDigits :: ([Int], Int) -> ([Int], Int)
normalizeDigits ([], _)   = ([], 0)
normalizeDigits (ds, 0)   = (ds, 0)
normalizeDigits (d:ds, l) = if l < 0 then normalizeDigits (0:d:ds, l+1)
                            else normalizeDigits (d:ds ++ [0], l-1)

toFraction :: (RealFrac a, RealFloat a) => Int -> a -> Distance
toFraction d x = Distance intPart n d
             where (intPart, fracPart) = properFraction x
                   d' = fromIntegral d
                   places = length (takeWhile (\x -> not (0 ~= x)) (map (\x -> 1 / d' ^^ x) [0..]))
                   fracDigits | fracPart ~= 0 = [0]
                              | otherwise     = fst . normalizeDigits $ (N.floatToDigits (toInteger d) (abs fracPart))
                   digitToFrac (x,y) = fromIntegral x / d' ^^ y
                   n' = sum (map digitToFrac $ zip fracDigits [1 .. places])
                   n  = n' * d'

--scaleUp :: Num a => Scale -> a -> Int -> DistanceScale
scaleUp scale len units = DistanceScale (name scale) (loc scale) newMarks (desc scale)
  where newMarks = map (\x -> DistanceMark (value x)
                                     (toFraction units $ distance x * len)
                                     (mark  x) (label x)) (marks scale)

distanceScaleToStr :: DistanceScale -> [String]
distanceScaleToStr d = [ l | m <- dmarks d,
                                 let l = L.intercalate "\t"
                                         [dname d,  markToStr (dmark m),
                                          dlabel m, distToStr (ddistance m)]
                           ]
                         where distToStr dist = (shows (wholePart dist)
                                                . showChar ' '
                                                . N.showGFloat (Just 3) (numeratorPart dist)
                                                . showChar '/'
                                                . N.showInt (denominatorPart dist)) ""
                               markToStr Major = "---"
                               markToStr Minor = "-- "
                               markToStr Tick  = "-  "
                               markToStr Offset= "  <"

tickC v | isWhole v                     = Major
        | v < 2 && isTenth v            = Major
        | v < 2 && isHalf (v * 10)      = Minor
        | v < 4 && isHalf  v            = Major
        | v < 4 && isTenth v            = Minor
        | v >= 4 && isTenth v           = Minor
        | v `elem` [ pi, e ]            = Offset
        | otherwise                     = Tick

labelC v | isWhole v          = show (round v)
         | v < 2 && isTenth v = show $ decimalDigit 1 (roundTo srPrec v)
         | v == pi            = "π"
         | v == e             = "e"
         | otherwise          = ""

scaleA = Scale "A" Body
         [ SRMark v (srOffset . sqrt $ v) (markT v) (label v) |
           v <- [1.0, 1.02 .. 4.98 ] ++ [ 5, 5.1 .. 9.9] ++ [10, 10.5 .. 100 ]
         ]
         "Square of x. Find x on scale D and read x^2 from Scale A."
         where
         markT v | v < 10  && isWhole  v        = Major
                 | v <  5  && isHalf   v        = Major
                 | v <  5  && isTenth  v        = Minor
                 | v < 10  && isHalf   v        = Minor
                 | isHalf  (v / 10)             = Major
                 | isWhole v                    = Minor
                 | otherwise                    = Tick
         label v | v < 10  && isWhole  v        = show $ round v
                 | v <  5  && isTenth  v        = show $ decimalDigit 1 (roundTo srPrec v)
                 | isHalf  (v / 10)             = show $ round v
                 | otherwise                    = ""

scaleB = Scale "B" Slide (marks scaleA)
         "Square of x. Find x on scale C and read x^2 from Scale B."

scaleC = Scale "C" Slide
         [ SRMark v (srOffset v) (tickC v) (labelC v) |
           v <- (L.sort . (L.nubBy (~=))) (pi:e:[1, 1.01 .. 2] ++ [2, 2.02 .. 4] ++ [4, 4.05 .. 10])
         ]
         "Baseline scale on the slide, running from 1 to 10."

scaleCF = Scale "CF" Slide
          [ SRMark v (srOffset (v / pi)) (markT v) (label v) |
            v <- (L.sort . (L.nubBy (~=))) (
                   [ pi, 10 * pi ]
                   ++ [  3.16, 3.18 ..  4 ] ++ [ 4,  4.05 .. 10 ]
                   ++ [ 10,   10.1  .. 20 ] ++ [20, 20.2  .. 31.2 ]
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

scaleCF_M = Scale "CF/M" Slide
            [ SRMark v (srOffset (v / log 10)) (markT v) (label v) |
              v <- (L.sort . (L.nubBy (~=))) (
                  pi : log 10 : 10 * log 10 : [  2.32, 2.34 .. 4 ]
                  ++ [ 4, 4.05 .. 9.95 ] ++   [ 10,   10.1 .. 20 ]
                  ++ [20, 20.2 .. 23.2 ]
              )
            ]
            "Convert between log base 10 and natural log: CF/M = C * ln 10 or 10^C = e^CF/M."
            where
              markT v | v ~= log 10               = Offset
                      | v ~= log 10 * 10          = Offset
                      | v ~= pi                   = Offset
                      | v < 20 && isHalf   v      = Major
                      | v < 10 && isTenth  v      = Minor
                      | v >=20 && isWhole (v / 10)= Major
                      | v >=20 && isWhole  v      = Minor
                      | otherwise                 = Tick
              label v | v ~= pi                     = "π"
                      | v < 10 && isWhole v         = show $ round v
                      | v ~= 10 || v ~= 20          = show $ decimalDigit (-1) v
                      | v > 10  && isWhole v        = show $ decimalDigit 0 v
                      | otherwise                   = ""

scaleCI = Scale "CI" Slide
          [ SRMark v (1 + (srOffset $ 1/v)) (tickC v) (labelC v) |
            v <- (reverse . L.sort . (L.nubBy (~=)))
                   (pi:e:[1.0, 1.01 .. 4 ] ++ [4.0, 4.05 .. 10])
          ]
          "Inversion scale. To find 1/x, look for x on the C scale and read the inverse from the CI scale."

scaleD  = Scale "D"  Body (marks scaleC)
          "Baseline scale on the stator. To multiply, place 1 on the C scale above the multiplicand on the D scale, look for the multiplier on the C scale, and read the result from the D scale."

scaleDF = Scale "DF" Body (marks scaleCF)
          "π to 10π. Multiply or divide using the C and D scales ignoring factors of π, then read the result off the corresponding CF scale. Thus to calculate 3/4 π, calculate 3 / 4 by aligning C4 over D3 and read 7.5 (/ 10) from the D scale. Then read ~2.36 off DF."

scaleDF_M = Scale "DF/M" Body (marks scaleCF_M)
          "Convert between log base 10 and natural log: DF/M = D * ln 10 or 10^D = e^DF/M."

scaleDI = Scale "DI" Body (marks scaleCI)
          "Inversion scale. To find 1/x, look for x on the D scale and read the inverse from the DI scale."

scaleK1 = Scale "3√ (# digits = [1,4,7,...])" Body
          [ SRMark v (srOffset . (^^ 3) $ v) (markT v) (label v) |
            v <- [ 1, 1.005 .. 1.995 ] ++ [ 2, 2.01 .. 10 ** recip 3 + 0.01 ]
          ]
          "Cube Root of numbers with 1, 4, 7, ... digits. Find x on D and read 3√x from this scale."
          where markT v | v < 2 && isTenth  v           = Major
                        | v < 2 && isHalf  (v * 10)     = Major
                        | v < 2 && isWhole (v * 100)    = Minor
                        | v >=2 && isTenth  v           = Major
                        | v >=2 && isHalf  (v * 10)     = Minor
                        | otherwise                     = Tick
                label v | isWhole v = show $ round v
                        | isTenth v = show $ decimalDigit 1 (roundTo 1 v)
                        | otherwise = ""

scaleK2 = Scale "3√ (# digits = [2,5,8,...])" Body
          [ SRMark v ((srOffset . (^^ 3) $ v) - 1) (markT v) (label v) |
            v <- 10 ** recip 3 : [2.16, 2.17 .. 100 ** recip 3 + 0.01]
          ]
          "Cube Root of numbers with 2, 5, 8, ... digits. Find x/10 on D and read 3√x from this scale."
          where markT v | isTenth v                     = Major
                        | isHalf  (v * 10)              = Minor
                        | v == 10 ** recip 3            = Offset
                        | otherwise                     = Tick
                label v = ""

scaleK3 = Scale "3√ (# digits = [3,6,9,...])" Body
          [ SRMark v ((srOffset . (^^ 3) $ v) - 2) (markT v) (label v) |
            v <- 100 ** recip 3 : [4.65, 4.66 .. 4.99] ++ [ 5, 5.02 .. 10 ]
          ]
          "Cube Root of numbers with 2, 5, 8, ... digits. Find x/10 on D and read 3√x from this scale."
          where markT v | isHalf  v                     = Major
                        | v < 5 && isTenth v            = Major
                        | v < 5 && isHalf (v * 10)      = Minor
                        | isTenth v                     = Minor
                        | v == 100 ** recip 3           = Offset
                        | otherwise                     = Tick
                label v | v < 5 && isTenth v    = show $ decimalDigit 1 (roundTo 1 v)
                        | isWhole v             = show $ round v
                        | otherwise             = ""

scaleL = Scale "L" Unspecified
         [ SRMark v v (markT v) (label v) | v <- [0, 0.002 .. 1] ]
         "Log base 10 of C (D) scale. Thus log 2 ~= 0.477."
         where markT v | isTenth v        = Major
                       | isHalf (v * 10)  = Major
                       | isTenth (v * 10) = Minor
                       | otherwise        = Tick
               label v | isTenth  v     = show (roundTo 1 v)
                       | otherwise      = ""

scaleLL0 = Scale "LL0" Body
           [ SRMark v (srOffset $ 1000 * logBase 10 v) (markT v) (label v) |
             v <- (L.sort . (L.nubBy (~=))) ([10 ** 0.001, 10 ** 0.01]
                 ++ [1.00232, 1.00234 .. 1.005] ++ [ 1.005, 1.00505 .. 1.01]
                 ++ [1.01,    1.0101  .. 1.02 ] ++ [ 1.02,  1.0202  .. 10**0.01]
                 )
           ]
           "log-log scale for x between ~1.00025 and ~1.023. Use with LL1 (2, 3) scales to find x^10 (100, 1000), and C scale to find x^n."
           where markT v | v ~= 10 ** 0.001                = Offset
                         | v ~= 10 ** 0.01                 = Offset
                         | v < 1.01 && isHalf  (v *  1000) = Major
                         | v < 1.01 && isWhole (v * 10000) = Minor
                         | v < 1.02 && isWhole (v *  1000) = Major
                         | v < 1.02 && isHalf  (v *  1000) = Minor
                         | isWhole (v *  100)              = Major
                         | isWhole (v * 1000)              = Minor
                         | otherwise                       = Tick
                 label v | or (map (v ~=) [1.0025, 1.015, 1.02]) = show $ roundTo 4 v
                         | v < 1.011 && isWhole (v * 1000) = show $ roundTo 3 v
                         | otherwise      = ""

scaleLL0I = Scale "LL0I" Body
           [ SRMark v (srOffset $ 1000 * logBase 10 (recip v)) (markT v) (label v) |
             v <- (reverse . L.sort . (L.nubBy (~=)))
                    ([recip 10 ** 0.001, recip 10 ** 0.01]
                     ++ [0.9780, 0.9785 .. 0.99  ]
                     ++ [0.9901, 0.9902 .. 0.9976]
                    )
           ]
           "Inverse log-log scale for x. Can be used to find reciprocals of numbers slightly larger than 1 (compare to LL0 scale) and some negative exponents."
           where markT v | recip v ~= 10 ** 0.001          = Offset
                         | recip v ~= 10 ** 0.01           = Offset
                         | v < 0.99 && isHalf  (v *  100)  = Major
                         | v < 0.99 && isWhole (v * 1000)  = Minor
                         | inRange v "()" 0.99 0.996 && isWhole (v * 1000) = Major
                         | inRange v "()" 0.99 0.996 && isHalf  (v * 1000) = Minor
                         | v >= 0.995                && isHalf  (v * 1000) = Major
                         | otherwise                       = Tick
                 label v | v >= 0.995 && isWhole (v * 1000) = "<" ++ (show $ roundTo 3 v)
                         | v ~= 0.98 || v ~= 0.99           = "<" ++ (show $ roundTo 3 v)
                         | otherwise      = ""

scaleLL1 = Scale "LL1" Body
           [ SRMark v (srOffset (100 * logBase 10 v)) (markT v) (label v) |
             v <- L.sort ([10 ** 0.01, 10 ** 0.1]
                 ++ [1.0232, 1.0234 .. 1.0498 ]
                 ++ [ 1.05, 1.0505 .. 1.0995]
                 ++ [ 1.1,   1.101 .. 1.199 ] ++ [ 1.2, 1.202 .. 10**0.1 ]
                 )
           ]
           "log-log scale for x between ~1.02 and ~1.26. Use with LL2 (0, 3) scales to find x^10 (0.1, 100), and C scale to find x^n."
           where markT v | v ~= 10 ** 0.01               = Offset
                         | v ~= 10 ** 0.1                = Offset
                         | v < 1.1 && isHalf  (v *  100) = Major
                         | v < 1.1 && isWhole (v * 1000) = Minor
                         | v < 1.2 && isWhole (v *  100) = Major
                         | v < 1.2 && isHalf  (v *  100) = Minor
                         | isWhole (v *  10)             = Major
                         | isWhole (v * 100)             = Minor
                         | otherwise                     = Tick
                 label v | or (map (v ~=) [1.025, 1.15, 1.2]) = show $ roundTo 4 v
                         | v < 1.101 && isWhole (v * 100) = show $ roundTo 3 v
                         | otherwise      = ""

scaleLL1I = Scale "LL1I" Body
           [ SRMark v (srOffset $ 100 * logBase 10 (recip v)) (markT v) (label v) |
             v <- (reverse . L.sort . (L.nubBy (~=)))
                    ([recip 10 ** 0.01, recip 10 ** 0.1]
                     ++ [0.796, 0.798 .. 0.90  ]
                     ++ [0.90,  0.901 .. 0.977 ]
                    )
           ]
           "Inverse log-log scale for x. Can be used to find reciprocals of numbers between 1.02 and 1.25 (compare to LL1 scale) and some negative exponents."
           where markT v | recip v ~= 10 ** 0.01           = Offset
                         | recip v ~= 10 ** 0.1            = Offset
                         | v < 0.90 && isHalf  (v *  10)   = Major
                         | v < 0.90 && isWhole (v * 100)   = Minor
                         | v >=0.90 && isWhole (v * 100)   = Major
                         | v >=0.90 && isHalf  (v * 100)   = Minor
                         | otherwise                       = Tick
                 label v | v <  0.90 && isHalf  (v * 10) = "<" ++ (show $ roundTo 2 v)
                         | v >= 0.90 && isWhole (v *100) = "<" ++ (show $ roundTo 2 v)
                         | otherwise                     = ""

scaleLL2 = Scale "LL2" Body
           [ SRMark v (srOffset (10 * logBase 10 v)) (markT v) (label v) |
             v <- L.sort (10 ** 0.1 : e : pi : [ 1.26, 1.262 .. 1.398]
                 ++ [ 1.4, 1.405 .. 1.795 ] ++ [ 1.8 , 1.81  .. 1.99 ]
                 ++ [ 2  , 2.01  .. 2.49  ] ++ [ 2.5 , 2.52  .. 3.98 ]
                 ++ [ 4  , 4.05  .. 5.95  ] ++ [ 6   , 6.1   ..10    ]
                 )
           ]
           "log-log scale for x between ~1.25 and 10. Use with LL3 (0, 1) scales to find x^10 (0.01, 0.1), and C scale to find x^n."
           where markT v | or (map (v ~=) [10**0.1,e,pi])             = Offset
                         | v < 1.8 && isHalf  (v * 10) = Major
                         | v < 1.8 && isTenth (v * 10) = Minor
                         | inRange v "[)" 1.8 2   && isTenth  v       = Major
                         | inRange v "[)" 1.8 2   && isHalf  (v * 10) = Minor
                         | inRange v "[]" 2   2.5 && isTenth  v       = Major
                         | inRange v "[]" 2   2.5 && isHalf  (v * 10) = Minor
                         | isHalf  v                                  = Major
                         | v < 6   && isTenth  v                      = Minor
                         | otherwise                                  = Tick
                 label v | v ~= e                 = "e"
                         | v ~= pi                = "π"
                         | v <= 2   && isTenth  v = show $ roundTo 1 v
                         | v ~= 2.5               = show $ roundTo 1 v
                         | isWhole v              = show $ round v
                         | otherwise              = ""

scaleLL2I = Scale "LL2I" Body
           [ SRMark v (srOffset $ 10 * logBase 10 (recip v)) (markT v) (label v) |
             v <- (reverse . L.sort . (L.nubBy (~=)))
                   (recip 10 ** 0.1 : [ 0.1 , 0.105 .. 0.79 ])
           ]
           "Inverse log-log scale for x between ~0.1 and 0.79. Use with LL2 to find reciprocals of numbers between ~1.25 and 10 and some negative exponents."
           where markT v | recip v ~= 10 ** 0.1 = Offset
                         | isHalf  (v * 10)     = Major
                         | isWhole (v * 100)    = Minor 
                         | otherwise            = Tick
                 label v | isHalf  (v * 10) = show $ roundTo 2 v
                         | otherwise        = ""

scaleLL3 = Scale "LL3" Body
           [ SRMark v (srOffset (logBase 10 v)) (markT v) (label v) |
             v <- (L.sort . (L.nubBy (~=))) ([10, 10.2 .. 15] ++ [15, 15.5 .. 30] ++ [30 .. 49]
               ++ [50, 52   ..  100] ++ [100, 105 .. 200 ] ++ [200, 210 .. 500]
               ++ [500, 550 .. 1000]
               ++ [x | base  <- [1, 1.5 .. 10],
                       expon <- [3 ..  9],
                       let x = base * 10 ^ expon ]
             )
           ]
           "log-log scale for x between 10 and 10^10. Use with LL2 (0, 1) scales to find x^0.1 (0.001, 0.01), and C scale to find x^n."
           where markT v | v `elem` ( 15:[10, 20..50] ++ [100, 150]
                                      ++ [200, 300 .. 500]
                                      ++ [10^x | x <- [3..10]])         = Major
                         | v < 30 && isHalf (v / 10)                    = Major
                         | v < 30 && isWhole v                          = Minor
                         | inRange v "[]"  30   50 && isHalf  (v / 10)  = Minor
                         | inRange v "[)"  50  200 && isWhole (v / 10)  = Minor
                         | inRange v "[)" 200  500 && isHalf  (v /100)  = Minor
                         | inRange v "[)" 500 1000 && isWhole (v /100)  = Minor
                         -- for large v, fst floatToDigits is [x] xor [x,5]
                         | v > 1000 && (fst . (N.floatToDigits 10)) v == [5]    = Major
                         | v > 1000 && (length.fst.(N.floatToDigits 10)) v == 1 = Minor
                         | otherwise            = Tick
                 label v | or (map (v~=) (15:[10,20..50] ++ [100,200,500])) = show $ round v
                         | v `elem` [10^x | x <- [3..10]] = "10^" ++ ((show . round) (logBase 10 v))
                         | otherwise  = ""

scaleLL3I = Scale "LL3I" Body
           [ SRMark v (srOffset $ logBase 10 (recip v)) (markT v) (label v) |
             v <- (L.reverse . L.sort . (L.nubBy (~=)))
                   ( 10e-10 :
                    [x | base  <- [1 .. 10],
                         expon <- [3 .. 10],
                         let x = base * 10 ^^ negate expon ]
                    ++ [ 0.001, 0.0015 .. 0.01] ++ [0.01, 0.012 .. 0.05]
                    ++ [ 0.05, 0.055 .. 0.1 ]
             )
           ]
           "Inverse log-log scale for x between 0.1 and 10^-10. Use with LL3 to find reciprocals of large numbers and som negative exponents."
           where markT v | v `elem` ( [10 ^^ negate x | x <- [3..10]]) = Major
                         | v < 10e-3 && isHalf  (normalize v) = Minor
                         | inRange v "()" 0.001 0.01 && isWhole (v * 1000)    = Minor
                         | v >= 0.01 && isHalf  (v * 10)      = Major
                         | v >= 0.01 && isWhole (v * 100)      = Minor
                         | otherwise            = Tick
                 label v | or (map (v~=) (15:[10,20..50] ++ [100,200,500])) = show $ round v
                         | v `elem` [10^x | x <- [3..10]] = "10^" ++ ((show . round) (logBase 10 v))
                         | otherwise  = ""
                 normalize x = x * (10 ^^ (negate . snd) (N.floatToDigits 10 x))

scaleR1 = Scale "√ (odd # digits)" Body
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

scaleR2 = Scale "√ (even # digits)" Body
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
                    | v < 5 && isTenth v        = show $ decimalDigit 1 (roundTo srPrec v)
                    | v == sqrt10               = "√10"
                    | otherwise                 = ""

scaleS = Scale "S" Unspecified
         [ SRMark v (1 + (srOffset . sin . radians $ v)) (markT v) (label v) |
           v <- (L.sort . (L.nubBy (~=))) (
                       degrees (asin 0.1) : [5.75, 5.80 ..  10]
                       ++ [10,  10.1 .. 20 ] ++ [20,  20.2  .. 30 ]
                       ++ [30,  30.5 .. 60 ] ++ [60 .. 80] ++ [80, 85, 90]
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

scaleSH1 = Scale "SH1" Unspecified
          [ SRMark v (srOffset . (10*) . sinh $ v) (markT v) (label v) |
            v <- (L.sort . (L.nubBy (~=))) (
                [ 0.1, 0.101 .. 0.2] ++ [0.2, 0.202 .. 0.4] ++ [0.4, 0.405 .. 0.885]
                )
          ]
          "Hyperbolic Sine for angles between 0.1 and ~0.9. Find x on SH, then C (or D) shows 10 * sinh x."
          where
            markT v | v <  0.2 && isWhole (v * 100) = Major
                    | v <  0.2 && isHalf  (v * 100) = Minor
                    | isHalf  (v *  10) = Major
                    | isWhole (v * 100) = Minor
                    | otherwise       = Tick
            label v | v < 0.2 && isWhole (v * 100) = show $ roundTo 2 v
                    | v < 0.4 && isHalf  (v *  10) = show $ roundTo 2 v
                    | isTenth v                    = show $ roundTo 1 v
                    | otherwise                    = ""

scaleSH2 = Scale "SH2" Unspecified
          [ SRMark v (srOffset . sinh $ v) (markT v) (label v) |
            v <- (L.sort . (L.nubBy (~=))) ( asinh 1 : [ 0.89, 0.9 .. 3 ])
          ]
          "Hyperbolic Sine for angles between ~0.9 and 3. Find x on SH, then C (or D) shows sinh x."
          where
            markT v | isTenth v       = Major
                    | isHalf (v * 10) = Minor
                    | v ~= asinh 1    = Offset
                    | otherwise       = Tick
            label v | isTenth v                    = show $ roundTo 1 v
                    | otherwise                    = ""

{-- TODO: Pickett marks scaleST with " at ~1.18˚ and ' at ~1.965˚. why?
          are they really for 11.9˚ and 20.1˚ on scaleS???
--}
scaleST = Scale "ST" Unspecified
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

scaleT1 = Scale "T1" Unspecified
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

scaleT2 = Scale "T2" Unspecified
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

scaleTH = Scale "TH" Unspecified
          [ SRMark v (srOffset . (10*) . tanh $ v) (markT v) (label v) |
            v <- (L.sort . (L.nubBy (~=)))
                   (  [0.1, 0.101 .. 0.2] ++ [0.2, 0.202 .. 0.4]
                   ++ [0.4, 0.405 .. 0.7] ++ [0.7, 0.71  .. 1]
                   ++ [1,   1.02  .. 1.4] ++ [1.4, 1.45  .. 2] ++ [2, 2.5, 3, 3.5]
                   )
          ]
          "Hyperbolic tangent for angles between 0.10 and 3.5. Find x on scale TH; scale C (or D) shows 10 * tanh x."
          where
            markT v | v <  0.2 && isWhole (v * 100) = Major
                    | v <  0.2 && isHalf  (v * 100) = Minor
                    | inRange v "[]" 0.2 0.7 && isHalf  (v *  10) = Major
                    | inRange v "[]" 0.2 0.7 && isWhole (v * 100) = Minor
                    | inRange v "[]" 0.7 1   && isTenth  v        = Major
                    | inRange v "[]" 0.7 1   && isHalf  (v *  10) = Minor
                    | inRange v "[]" 1   2   && isHalf   v        = Major
                    | v > 2                  && isWhole  v        = Major
                    | v > 2                  && isHalf   v        = Minor
                    | v >= 1   && isTenth v                       = Minor
                    | otherwise       = Tick
            label v | v < 0.2 && isWhole (v * 100) = show $ roundTo 2 v
                    | v < 0.4 && isHalf  (v *  10) = show $ roundTo 2 v
                    | isTenth v                    = show $ roundTo 1 v
                    | otherwise                    = ""
