import Log

-- Exercise 1
-- Auxiliary function to decide if a String can be parsed to Int
canParseInt :: String -> Bool
canParseInt s = 
    case (reads s :: [(Int, String)]) of
        [(x, "")] -> True
        _ -> False 

parseMessage :: String -> LogMessage
parseMessage log = case (words log) of
    ("E": c : t : msg) -> if not (canParseInt c) || not (canParseInt t)
                            then Unknown log
                            else LogMessage (Error (read c)) (read t) (unwords msg)
    ("W": t : msg) -> if not (canParseInt t)
                            then Unknown log
                            else LogMessage Warning (read t) (unwords msg)
    ("I": t : msg) -> if not (canParseInt t)
                            then Unknown log
                            else LogMessage Info (read t) (unwords msg)
    x -> Unknown log

parse :: String -> [LogMessage]
parse logs = [ parseMessage log | log <- lines logs] 


-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ ts _) (Node lmt tmsg@(LogMessage _ tst _) rmt)
    | ts < tst = Node (insert msg lmt) tmsg rmt
    | ts > tst = Node lmt tmsg (insert msg rmt)


-- Exercise 3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (lm: lms) = insert lm (build lms)


-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder (Leaf) = []
inOrder (Node lmt msg rmt) = inOrder lmt ++ [msg] ++ inOrder rmt


-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong logs = case inOrder (build logs) of
    (x: xs) -> addError x ++ whatWentWrong xs
        where
            addError (LogMessage (Error severity) _ content) = if severity >= 50 then [content] else []
            addError x = []
