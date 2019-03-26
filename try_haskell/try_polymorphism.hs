
-- fails:

f :: [a] -> a
f = head

f :: (a, b) -> a
f = fst


