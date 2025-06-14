module Main (main) where

import qualified Test.Framework as Test
import Database.Redis
import Tests
import PubSubTest
import System.Environment

main :: IO ()
main = do
    mhost <- lookupEnv "REDIS_HOST"
    conn <- connect $ maybe id (\x -> \ci -> ci{connectHost=x}) mhost $ defaultConnectInfo 
    Test.defaultMain (tests conn)

tests :: Connection -> [Test.Test]
tests conn = map ($ conn) $ concat
    [ testsMisc, testsKeys, testsStrings, [testHashes], testsLists, testsSets, [testHyperLogLog]
    , testsZSets, [testPubSub], [testTransaction], [testScripting]
    , testsConnection, testsClient, testsServer, [testScans, testSScan, testHScan, testZScan], [testZrangelex]
    , [testXAddRead, testXReadGroup, testXRange, testXpending, testXClaim, testXInfo, testXDel, testXTrim]
    , testPubSubThreaded
      -- should always be run last as connection gets closed after it
    , [testQuit]
    ]


testsClient :: [Test]
testsClient = [testClientId, testClientName]

testsServer :: [Test]
testsServer =
    [testServer, testBgrewriteaof, testFlushall, testInfo, testConfig
    ,testSlowlog, testDebugObject]

testsConnection :: [Test]
testsConnection = [ testConnectAuth, testConnectAuthUnexpected, testConnectAuthAcl,testConnectDb
                  , testConnectDbUnexisting, testEcho, testPing, testSelect ]

testsKeys :: [Test]
testsKeys = [ testKeys, testKeysNoncluster, testExpireAt, testSort, testGetType, testObject ]
