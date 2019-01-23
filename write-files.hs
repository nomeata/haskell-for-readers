import Text.Pandoc.Definition
import Text.Pandoc.Walk
import qualified Data.ByteString.Lazy as BL
import Data.Aeson

main = BL.getContents >>= (walkM (fmap concat . mapM f) :: Pandoc -> IO Pandoc) . either error id .  eitherDecode'

f :: Block -> IO [Block]
f b@(CodeBlock (i,cs,a) code) | Just name <- lookup "file" a = do
    appendFile ("files/" ++ name) code
    appendFile ("files/" ++ name) "\n\n"
    return []
f _ = return []
