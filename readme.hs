#!/usr/bin/runghc

{--
 - Generate README.md with all of the pictures for ahaym/ray
 - Also moves all image files to the correct folder.
 - Run me in the project root with all the images!
--}

import System.Directory
import System.Process

mkLink file = "![](img/" ++ file ++ ")"

main = do
   callCommand "mv *.png img/"
   contents <- listDirectory "./img"
   writeFile "README.md" . unlines $ mkLink <$> contents
