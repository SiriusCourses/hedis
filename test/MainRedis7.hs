module Main (main) where

import qualified Test.Framework as Test
import Database.Redis
import Tests
import PubSubTest

main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    Test.defaultMain (tests conn)

tests :: Connection -> [Test.Test]
tests conn = map ($conn) $ [testXCreateGroup7, testXpending7, testXAutoClaim7, testSet7, testZAdd7, testQuit]