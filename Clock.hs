module Clock (clockHour, clockMin, fromHourMin, toString) where

newtype Clock = Clock Int

instance Eq Clock where
  (Clock a) == (Clock b) = normalDay a == normalDay b

instance Num Clock where
  (Clock a) + (Clock b) = Clock (a + b)
  (Clock a) * (Clock b) = Clock (a * b)
  abs (Clock a) = Clock (abs a)
  signum (Clock a) = Clock (signum a)
  fromInteger a = Clock (fromIntegral a)
  negate (Clock a) = Clock (normalDay (negate a))

instance Show Clock where
  show clock =
       lpad (show (clockHour clock)) ++ ":"
    ++ lpad (show (clockMin clock))

clockHour :: Clock -> Int
clockHour (Clock m) = cyclic 24 (m `div` 60)

clockMin :: Clock -> Int
clockMin (Clock m) = cyclic 60 m

fromHourMin :: Int -> Int -> Clock
fromHourMin h m = Clock (normalDay (h * 60 + m))

toString :: Clock -> String
toString = show

cyclic :: Int -> Int -> Int
cyclic lim x | x < 0 = lim + (x `mod` lim)
             | otherwise = x `mod` lim

normalDay :: Int -> Int
normalDay = cyclic 1440

lpad :: String -> String
lpad xs = if length xs == 1 then "0" ++ xs else xs
