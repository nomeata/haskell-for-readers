import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter go
  where
    go :: Pandoc -> Pandoc
    go (Pandoc meta blocks) = Pandoc meta $ concatMap goBlock blocks

    goBlock b@(CodeBlock (_,cs,_) _) | "slide" `elem` cs = [b]
    goBlock (OrderedList _ bss) = concatMap goBlock (concat bss)
    goBlock (BulletList bss) = concatMap goBlock (concat bss)
    goBlock (DefinitionList bss) = concatMap goBlock (concat (concatMap snd bss))
    goBlock b@(Header _ (_,cs,_) _) | "unnumbered" `elem` cs = []
    goBlock b@(Header n (_,cs,_) _) | n > 2 = [HorizontalRule, b]
    goBlock b@(Header _ (_,cs,_) _) = [b]
    goBlock b@(Div (_,cs,_) bs) | "Exercise" `elem` cs = [b]
    goBlock b@(Div (_,cs,_) bs) | "Solution" `elem` cs = []
    goBlock b@(Div (_,cs,_) bs) = concatMap goBlock bs
    goBlock _ = []
