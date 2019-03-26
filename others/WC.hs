-- file: ch01/WC.hs
-- lines beginning with "--" are comments.
-- runghc WC < xxx.txt


main = interact wordCount
--	where wordCount input = show . sum $ map length (lines input)
	where wordCount input = show (length (words input)) ++ "\n"
--	where wordCount input = show (length (lines input)) ++ "\n"




