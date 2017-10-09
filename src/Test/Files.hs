
module Test.Files(main) where

import Development.Shake
import Development.Shake.FilePath
import System.Directory
import Test.Type
import Control.Monad
import Data.List
import General.GetOpt

data Args = UsePredicate deriving (Eq,Show,Bounded,Enum)

main = shakeTest test optionsEnum $ \opts -> do
    want ["even.txt","odd.txt"]

    "A1-plus-B" %> \out -> do
        a1 <- readFileLines "A1"
        b  <- readFileLines "B"
        writeFileLines out $ a1 ++ b

    ["A1", "A2"] &%> \[o1, o2] -> do
        writeFileLines o1 ["This is", "A1"]
        writeFileLines o2 ["This is", "A2"]

    "B" %> \out ->
        writeFileLines out ["This is", "B"]

    -- Since &?> and &%> are implemented separately we test everything in both modes
    let deps &?%> act | UsePredicate `elem` opts = (\x -> if x `elem` deps then Just deps else Nothing) &?> act
                      | otherwise = deps &%> act

    ["even.txt","odd.txt"] &?%> \[evens,odds] -> do
        src <- readFileLines "numbers.txt"
        let (es,os) = partition even $ map read src
        writeFileLines evens $ map show es
        writeFileLines odds  $ map show os

    ["dir1/out.txt","dir2/out.txt"] &?%> \[a,b] -> do
        writeFile' a "a"
        writeFile' b "b"

    let batchTestTargets = ["An", "Bn"]
    batchTestTargets &?%> \outs -> do
        let batchTestSources = map (-<.> ".in") outs
        xs <- needHasChanged batchTestSources
        mapM_ ((`writeFile'` "1") . fst) $ filter snd $ zip outs xs

    "On" %> \out -> do
        xs <- needHasChanged batchTestTargets
        writeFileLines out $ map show xs

    (\x -> let dir = takeDirectory x in
           if takeFileName dir /= "pred" then Nothing else Just [dir </> "a.txt",dir </> "b.txt"]) &?> \outs ->
        mapM_ (`writeFile'` "") outs


test build = do
    forM_ [[],["--usepredicate"]] $ \args -> do
        let nums = unlines . map show
        writeFile "numbers.txt" $ nums [1,2,4,5,2,3,1]
        build ("--sleep":args)
        assertContents "even.txt" $ nums [2,4,2]
        assertContents "odd.txt"  $ nums [1,5,3,1]
        build ["clean"]
        build ["--no-build","--report=-"]
        build ["dir1/out.txt"]

        writeFile "An.in" "1"
        writeFile "Bn.in" "1"
        build ["On", "--sleep"]
        assertContents "On" "True\nTrue\n"
        writeFile "An.in" "1"
        build ["On"]
        assertContents "On" "True\nFalse\n"
        writeFile "Bn.in" "1"
        build ["On"]
        assertContents "On" "False\nTrue\n"
        build ["On"]
        assertContents "On" "False\nTrue\n"

        -- this should still fail, batch rule should only rebuilt An, but rebuilds nothing
        -- the reasoning is: We mess with one of the targets of the "batch" rule and expect
        -- this target to be rebuild, but nothing happens, as the source didn't change :(
        writeFile "An" "1"
        build ["On"]
        -- the desired behavior would be "True\nFalse\n" here.
        assertContents "On" "False\nFalse\n"

        writeFile "Bn" "1"
        build ["On"]
        -- the desired behavior would be "False\nTrue\n" here.
        assertContents "On" "False\nFalse\n"

    build ["pred/a.txt"]

    -- Test #496
    build ["A1-plus-B"]
    removeFile "A2"
    build ["A1-plus-B"]
