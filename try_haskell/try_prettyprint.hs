
-- see : Text.PrettyPrint.HughesPJ :: source
-- data Doc = Union Doc Doc | ...
-- it seems only 
--      {sep, cat} ==>> sepX ==>> sep1 ==>> sepNB
--          | fill ==>> fill1 ==>> fillNB ==>> fillNBE
--      ==>> mkUnion ==>> Union
--      makes union, i.e. many layouts
--      hang ==>> sep and no one call cat
-- what is "fill"?
--  fsep, fcat ==>> fill???

import Text.PrettyPrint

copy n a = take n $ repeat a
s n = take n $ cycle ['A'..'Z']
t n = text $ s n
n = 99
beside n = t n <> t n -- text s <> text t = text (s++t)
beside_sep_space n = t n <+> t n -- 80 ==>> non-Union, i.e. one layout
-- their list version ==>> hcat/hsep one layout

-- ($$)/vcat : one layout
-- ($+$): one layout

-- sep = fold (hsep | vcat) ==>> union, two layout; space or newline
-- cat = fold (hcat | vcat) ==>> union, two layout; ++ or newline




fNM f n m = f $ fmap text [s m | y<-[1..n]]
-- sepNM 4 16 ==>> 1 line
-- sepNM 4 17 ==>> 4 lines !! 4*(17+1) == 72 v.s. 80??
--      whole list share same sep char
sepNM = fNM sep
-- fsepNM 4 16 ==>> 1 line
-- fsepNM 4 17 ==>> 2 lines !! 3 blocks in first line
--      flow layout
fsepNM = fNM fsep




