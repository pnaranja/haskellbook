-- Exercises and Notes for Chapter 8
main :: IO ()
main = undefined

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = (incTimes (times-1) n) + 1

--Extract recursion of incTimes
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
--applyTimes n f b = f (applyTimes (n-1) f b) -- Old implementation
applyTimes n f b = f . applyTimes (n-1) f $ b

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes times (+1) n

--Write out evaluation of applyTimes 5 (+1) 5
-- ((+1) ((+1) ((+1) ((+1) ((+1) 5)))))
-- (+1) . (+1) . (+1) . (+1) . (+1) $ 5

