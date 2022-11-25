-- not' :: Bool -> Bool
-- not' b = case b of
--         True -> False
--         False -> True

absInt n = 
    case (n >= 0) of
        True -> n
        _    -> -n


-- Zadania

isItTheAnswer :: String -> Bool
isItTheAnswer a =
    case a of
        "Love" -> True
        _      -> False


not' :: Bool -> Bool
not' b = case b of
        True -> False
        False -> True

or' :: (Bool, Bool) -> Bool
or' (a, b) = case (a, b) of
    (False, False) -> False
    _              -> True

