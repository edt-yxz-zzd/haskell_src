{-# LANGUAGE TemplateHaskell #-}
module PrintQ where
import Language.Haskell.TH

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
pprintQ :: Ppr a => Q a -> IO ()
pprintQ = printQBy pprint
pprintLnQ :: Ppr a => Q a -> IO ()
pprintLnQ q = pprintQ q >> printLn

