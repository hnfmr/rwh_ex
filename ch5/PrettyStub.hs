-- file: ch5/PrettyStub.hs

import SimpleJSON

data Doc = ToBeDefined
         deriving (Show)
         
string :: String -> Doc
string str = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined

fsep :: [Doc] -> Doc
fsep xs = undefined
