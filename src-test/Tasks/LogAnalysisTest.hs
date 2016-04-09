{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Tasks.LogAnalysisTest where
import Tasks.LogAnalysis
import Tasks.Log


import Test.Framework
import Test.HUnit (Assertion)
import Data.List

bigItem :: MessageTree
bigItem = (Node
              (Node Leaf (LogMessage Warning 1 "1")
                 (Node
                    (Node
                       (Node Leaf (LogMessage Warning 2 "2")
                          (Node Leaf (LogMessage Warning 3 "3")
                             (Node Leaf (LogMessage Warning 5 "5")
                                (Node Leaf (LogMessage Warning 8 "8") Leaf))))
                       (LogMessage Warning 4 "4")
                       (Node Leaf (LogMessage Warning 5 "5")
                          (Node Leaf (LogMessage Warning 8 "8") Leaf)))
                    (LogMessage Warning 7 "7")
                    (Node Leaf (LogMessage Warning 8 "8") Leaf)))
              (LogMessage Warning 6 "6")
              (Node
                 (Node
                    (Node Leaf (LogMessage Warning 2 "2")
                       (Node Leaf (LogMessage Warning 3 "3")
                          (Node Leaf (LogMessage Warning 5 "5")
                             (Node Leaf (LogMessage Warning 8 "8") Leaf))))
                    (LogMessage Warning 4 "4")
                    (Node Leaf (LogMessage Warning 5 "5")
                       (Node Leaf (LogMessage Warning 8 "8") Leaf)))
                 (LogMessage Warning 7 "7")
                 (Node Leaf (LogMessage Warning 8 "8") Leaf)))
                         
bigArray1 :: [LogMessage]
bigArray1 = toMessages [8, 5, 3, 2, 4, 7, 1]

bigArray2 :: [LogMessage]
bigArray2 = bigArray1 ++ toMessages [6]

arrayFromFile = toMessages [11, 9, 5, 8, 10, 2, 3, 6, 1, 7, 4]

createMessage :: Int -> LogMessage
createMessage timeStamp = LogMessage Warning timeStamp $ show timeStamp

toMessages :: [Int] -> [LogMessage]
toMessages = map createMessage

test_parseMessageError :: Assertion
test_parseMessageError = assertEqual (LogMessage (Error 2) 562 "help help") $ parseMessage "E 2 562 help help"

test_parseMessageInformation :: Assertion
test_parseMessageInformation = assertEqual (LogMessage Info 29 "la la la") $ parseMessage "I 29 la la la"

test_parseMessageWarning :: Assertion
test_parseMessageWarning = assertEqual (LogMessage Warning 49 "Warning!") $ parseMessage "W 49 Warning!"

test_parseMessageUnknown :: Assertion
test_parseMessageUnknown = assertEqual (Unknown "This is not in the right format") $ parseMessage "This is not in the right format"

test_parse1 :: Assertion
test_parse1 = assertEqual [(LogMessage (Error 2) 562 "help help")] $ parse "E 2 562 help help"

test_parseBig :: Assertion
test_parseBig = do
            parsedItems <- (testParse parse 10 "TestData/error.log")
            assertEqual 10 $ length parsedItems      
            
test_insertBig :: Assertion
test_insertBig = let
                insert8 = Tasks.LogAnalysis.insert (LogMessage Warning 8 "8") Leaf
                insert5 = Tasks.LogAnalysis.insert (LogMessage Warning 5 "5") insert8
                insert3 = Tasks.LogAnalysis.insert (LogMessage Warning 3 "3") insert5
                insert2 = Tasks.LogAnalysis.insert (LogMessage Warning 2 "2") insert3
                insert4 = Tasks.LogAnalysis.insert (LogMessage Warning 4 "4") insert2
                insert7 = Tasks.LogAnalysis.insert (LogMessage Warning 7 "7") insert4
                insert1 = Tasks.LogAnalysis.insert (LogMessage Warning 1 "1") insert7
                insert6 = Tasks.LogAnalysis.insert (LogMessage Warning 6 "6") insert1
                in assertEqual bigItem insert6
                
test_insertSimple :: Assertion
test_insertSimple = let 
                insert3 = Tasks.LogAnalysis.insert (LogMessage Warning 3 "3") Leaf
                insert1 = Tasks.LogAnalysis.insert (LogMessage Warning 1 "1") insert3
                insert2 = Tasks.LogAnalysis.insert (LogMessage Warning 2 "2") insert1
                in assertEqual (Node (Node Leaf 
                                           (LogMessage Warning 1 "1") 
                                           (Node Leaf (LogMessage Warning 3 "3") Leaf))
                                  (LogMessage Warning 2 "2")
                                  (Node Leaf (LogMessage Warning 3 "3") Leaf))                           
                                  insert2            
             
test_build :: Assertion
test_build = assertEqual bigItem (build bigArray2)

--test_buildFromFile :: Assertion
--test_buildFromFile = assertEqual Leaf $ build arrayFromFile
                       
test_inOrder1 :: Assertion
test_inOrder1 = assertEqual 
                            (sortBy (compare `on` \(LogMessage _ timestamp _) -> timestamp) bigArray1) 
                            (inOrder $ build bigArray1)
                            
test_inOrder2 :: Assertion
test_inOrder2 = assertEqual 
                            (sortBy (compare `on` \(LogMessage _ timestamp _) -> timestamp) bigArray2) 
                            (inOrder $ build bigArray2)
                        
test_inOrder3 :: Assertion
test_inOrder3 = assertEqual 
                            (sortBy (compare `on` \(LogMessage _ timestamp _) -> timestamp) arrayFromFile) 
                            (inOrder $ build arrayFromFile)
                           
test_readSingle :: Assertion
test_readSingle = do 
            parsedItems <- testWhatWentWrong parse whatWentWrong "TestData/sample.log"
            assertNotEqual parsedItems parsedItems