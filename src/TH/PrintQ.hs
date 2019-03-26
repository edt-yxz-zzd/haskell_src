{-# LANGUAGE TemplateHaskell #-}

module TH.PrintQ where
import Language.Haskell.TH
import TH.Data2Doc

printLn = putStrLn ""

ioQ :: Q a -> IO a
ioQ = runQ
showQ :: Show a => Q a -> IO String
showQ q = ioQ q >>= return . show
printQBy :: (a->String) -> Q a -> IO ()
printQBy f q = ioQ q >>= putStr . f
printQ :: Show a => Q a -> IO ()
-- printQ q = ioQ q >>= print
printQ = printQBy show
printLnQ :: Show a => Q a -> IO ()
printLnQ q = printQ q >> printLn
pprintAsSrcQ :: Ppr a => Q a -> IO ()
pprintAsSrcQ = printQBy pprint
pprintAsSrcLnQ :: Ppr a => Q a -> IO ()
pprintAsSrcLnQ q = pprintAsSrcQ q >> printLn


pprintQ, pprintLnQ :: Data2Doc a => Q a -> IO ()
pprintQ = printQBy pshow_data
pprintLnQ q = pprintQ q >> printLn

