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
                x -> Unknown (unwords x)

parse :: String -> [LogMessage]
parse logs = [ parseMessage log | log <- lines logs] 
