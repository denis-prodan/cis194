module Tasks.LogAnalysis where

import Tasks.Log

parseMessage :: String -> LogMessage
parseMessage ('E':rest) = let wordsList = words rest
                              firstNumber = read $ head wordsList
                              secondNumber = read (wordsList !! 1)
                          in LogMessage (Error firstNumber) secondNumber (unwords $ drop 2 wordsList) 
parseMessage ('W':rest) = let wordsList = words rest
                              firstNumber =  read $ head wordsList
                          in LogMessage Warning firstNumber (unwords $ drop 1 wordsList) 
parseMessage ('I':rest) = let wordsList = words rest
                              firstNumber = read $ head wordsList
                          in LogMessage Info firstNumber (unwords $ drop 1 wordsList) 
parseMessage x          = Unknown x

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message@(LogMessage{}) leaf@Leaf = Node leaf message Leaf
insert message@(LogMessage _ timestamp _) node@(Node{}) = Node 
                                                        (searchLeftInTree timestamp node)
                                                        message
                                                        (searchRightInTree timestamp node)

searchLeftInTree :: TimeStamp -> MessageTree -> MessageTree
searchLeftInTree _ (Node _ (Unknown _) _) = error "Unknown items shouldn't be inside tree"
searchLeftInTree _ leaf@Leaf = leaf
searchLeftInTree timeStamp node@(Node left (LogMessage _ treeTimestamp _) right) = 
        if treeTimestamp < timeStamp 
            then node
            else case searchLeftInTree timeStamp left of
                nd@(Node{}) -> nd
                Leaf -> searchLeftInTree timeStamp right  
            
searchRightInTree :: TimeStamp -> MessageTree -> MessageTree
searchRightInTree _ (Node _ (Unknown _) _) = error "Unknown items shouldn't be inside tree"
searchRightInTree _ leaf@Leaf = leaf
searchRightInTree timeStamp node@(Node left (LogMessage _ treeTimestamp _) right) = 
        if treeTimestamp >= timeStamp  
            then node
            else case searchRightInTree timeStamp right of
                nd@(Node{}) -> nd
                Leaf -> searchRightInTree timeStamp left                 
            
build :: [LogMessage] -> MessageTree
build = foldl (flip Tasks.LogAnalysis.insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder = inOrderInner (-1000000000000) 1000000000000

inOrderInner :: Int -> Int -> MessageTree -> [LogMessage]
inOrderInner _ _ Leaf = []
inOrderInner notLessThan noMoreThan (Node left message@(LogMessage _ timestamp _) right) = 
        let 
            leftPart = inOrderInner notLessThan (min timestamp noMoreThan) left    
            shouldTakeItem = (notLessThan < timestamp) &&  (timestamp < noMoreThan)           
            currentItem = [message | shouldTakeItem]
            rightPart = inOrderInner (max timestamp notLessThan) noMoreThan right                           
        in leftPart ++ currentItem ++ rightPart

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = let orderedMessages =  inOrder $ build messages
                            in map (\(LogMessage _ _ text) -> text) (filter filterMessage orderedMessages)
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c  
f `on` g = \x y -> f (g x) (g y)  
                            
filterMessage :: LogMessage -> Bool
filterMessage (LogMessage (Error severity) _ _)
                        | severity >= 50 = True
                        | otherwise      = False
filterMessage _ = False