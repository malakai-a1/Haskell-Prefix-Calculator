-- Malakai Ashenafi
-- mga190001
-- 10/06/2024


import Data.Char
import Text.Read (readEither)
import System.IO (hFlush, stdout)
import GHC.Base (quotInt)

-- Expression data types
data Expr = Lit Double -- A literal value
          | Add Expr Expr -- Addition
          | Sub Expr Expr -- Subtraction
          | Mul Expr Expr -- Multiplication
          | Div Expr Expr -- Division
          | HistoryRef Int  -- A reference to a previous result (e.g., $3)
          deriving Show

eval :: [Double] -> Expr -> Either String Double -- Evaluate the expression
eval history expr = 
    case expr of -- Check the expression
        Lit x -> Right x 
        Add e1 e2 -> evalBinary (+) e1 e2
        Sub e1 e2 -> evalBinary (-) e1 e2
        Mul e1 e2 -> evalBinary (*) e1 e2
        Div e1 e2 -> do -- Check for division by zero
            v1 <- eval history e1
            v2 <- eval history e2
            if v2 == 0 
                then Left "Error: Division by zero"
                else Right (v1 / v2)
        HistoryRef idx -> -- Check for history reference
            if idx > 0 && idx <= length history -- id number must be between 1 and the latest history value.
            then Right (history !! (idx - 1))  -- Get the correct result from history.
            else Left $ "History index out of bounds: " ++ show idx
    where 
        evalBinary :: (Double -> Double -> Double) -> Expr -> Expr -> Either String Double -- Evaluate a binary operator
        evalBinary operator e1 e2 = do -- Evaluate the two expressions
            v1 <- eval history e1 -- Evaluate the first expression
            v2 <- eval history e2 -- Evaluate the second expression
            Right (operator v1 v2) -- Apply the operator to the two values

-- Parse function that tokenizes the input string and parses the tokens
parse :: String -> [Double] -> Either String Expr -- Parse the input string
parse s history = 
    case parse' (tokenize s) history of -- Parse the tokens
        Left err -> Left err -- Return error if parsing fails
        Right (expr, []) -> Right expr -- Return the expression if parsing succeeds
        Right (_, rest) -> Left $ "Unexpected token: " ++ unwords rest  -- Return error if there are extra tokens

-- Tokenize function that splits the input string into tokens
tokenize :: String -> [String] -- Tokenize the input string
tokenize [] = [] -- Empty string
tokenize ('-':'$':n) = ("-$" ++ takeWhile isDigit n) : tokenize (dropWhile isDigit n) -- Negated history reference
tokenize (c:cs) --  Non-empty string
    | isSpace c = tokenize cs
    | otherwise = let (token, rest) = span (not . isSpace) (c:cs)
                  in token : tokenize rest


parse' :: [String] -> [Double] -> Either String (Expr, [String]) -- Parse an expression
parse' [] _ = Left "Empty expression" -- Empty expression
parse' (token:rest) history = -- Parse the first token
    case token of -- Check the token
        "+" -> parseBinary Add rest history
        "-" -> parseUnaryOrBinary rest history
        "*" -> parseBinary Mul rest history
        "/" -> parseBinary Div rest history
        ('$':n) -> case readEither n of -- Check if it's a history reference
                     Left _ -> Left $ "Invalid history reference: " ++ token -- Invalid history reference
                     Right idx -> -- Check if the history reference is valid
                         if idx > 0 && idx <= length history
                         then Right (HistoryRef idx, rest)
                         else Left $ "History reference out of bounds: " ++ token
        ('-':'$':n) -> case readEither n of -- Check if it's a negated history reference
                         Left _ -> Left $ "Invalid history reference: " ++ token
                         Right idx -> -- Check if the history reference is valid
                             if idx > 0 && idx <= length history
                             then Right (Sub (Lit 0) (HistoryRef idx), rest) -- Negate history ref
                             else Left $ "History reference out of bounds: " ++ token
        _ -> case readEither token of
               Left _ -> Left $ "Invalid token: " ++ token
               Right x -> Right (Lit x, rest)

-- Parse unary minus or binary subtraction
parseUnaryOrBinary :: [String] -> [Double] -> Either String (Expr, [String]) -- Parse unary minus or binary subtraction
parseUnaryOrBinary [] _ = Left "Unexpected end of expression"
parseUnaryOrBinary (token:rest) history =
    case token of -- Check if the token is a unary minus or binary subtraction
        ('$':n) -> case readEither n of -- Check if it's a history reference
                     Left _ -> Left $ "Invalid history reference: " ++ token
                     Right idx -> -- Check if the history reference is valid
                         if idx > 0 && idx <= length history 
                         -- Negate the history reference by treating it as "0 - $n"
                         then Right (Sub (Lit 0) (HistoryRef idx), rest)
                         else Left $ "History reference out of bounds: " ++ token
        _ -> parseBinary Sub (token:rest) history -- Parse binary subtraction

-- Parse a binary operator
parseBinary :: -- Parse two expressions and combine them
    (Expr -> Expr -> Expr) 
    -> [String] 
    -> [Double] 
    -> Either String (Expr, [String])
parseBinary exprConstructor e history =
    case parse' e history of -- Parse the first expression
        Left err -> Left err
        Right (e1, rest) -> 
            case parse' rest history of --  Parse the second expression
                Left err -> Left err
                Right (e2, rest'') -- Combine the two expressions
                    -> Right (exprConstructor e1 e2, rest'') -- Return the combined expression

-- Run function that evaluates the expression and updates history
run :: String -> [Double] -> ([Double], String)
run expr history = 
    case parse expr history of
        Left err -> (history, "\nInvalid expression: " ++ err)
        Right parsedExpr ->
            case eval history parsedExpr of
                Left err -> (history, err)  -- Return error for division by zero
                Right result ->
                    let newHistory = history ++ [result]  -- Append to history
                        historyWithIds = zip [1..] newHistory  -- Assign correct IDs
                        historyStr = unlines $ map (\(i, res) -> show i ++ ": " ++ show res) historyWithIds
                    in (newHistory, historyStr)
-- Main loop function
main :: IO ()
main = do
    let loop history = do
            putStr "Enter an expression: "
            hFlush stdout -- Flush the output buffer to print the prompt
            expr <- getLine
            if expr == "quit"
                then putStrLn "Exiting..."
                else do
                    let (newHistory, output) = run expr history -- Evaluate the expression
                    putStrLn output -- Print the result or error message
                    loop newHistory -- Continue the loop with the updated history
    loop []
