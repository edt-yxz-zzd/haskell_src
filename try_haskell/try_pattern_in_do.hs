eq_Nothing = do a <- Nothing :: Maybe Int
                Just a
raise_Exception = do let Just a = Nothing :: Maybe Int
                     Just a

