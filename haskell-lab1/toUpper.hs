toUpper :: Char -> Char
toUpper c = if dec > 96 && dec < 123
            then toEnum (dec-32)::Char
            else c
            where dec = fromEnum c::Int