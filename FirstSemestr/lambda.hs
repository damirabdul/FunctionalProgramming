module Lambda where 

data Expression = Literal String 
                | Lambda String Expression
                | Apply Expression Expression
                deriving Show

type Context = [(String, Expression)]

represent :: Expression -> String
represent (Literal a) = a
represent (Lambda x e) = "(L " ++ x ++ ". " ++ represent e ++ ")"
represent (Apply e1 e2) = "(" ++ represent e1 ++ " " ++ represent e2 ++ ")"

evaluate :: Context -> Expression -> Expression
evaluate context e@(Literal a) = maybe e id (lookup a context)
evaluate context (Lambda x e) = Lambda x (evaluate context e)
evaluate context (Apply e1 e2) = apply context (evaluate context e1) (evaluate context e2)

apply :: Context -> Expression -> Expression -> Expression
apply context (Lambda x e) e2 = evaluate ((x, e2):context) e
apply context e1 e2 = Apply e1 e2

parse :: String -> (Expression, String)
parse ('(':'L':s) = let
    (Literal a, s') = parse (tail s)
    (e, s'') = parse (drop 2 s')
    in (Lambda a e, tail s'')

parse ('(':s) = let
    (e1, s') = parse s
    (e2, s'') = parse (tail s')
    in (Apply e1 e2, tail s'')

parse (c:s) | elem c " .)" = (Literal "", c:s)
            | otherwise    = let ((Literal a), s') = parse s 
                             in (Literal (c:a), s')

--use to input any string
--run :: String -> String
--run = represent . evaluate [] . fst . parse
--main = do
--  line <- getLine
--  putStrLn$ run line

--Some tests
--Input: (((L x. (L y. x)) (L a. a)) (L b. b))
--Output: (L a. a)

--Input: ((L x. x) (L y. (L z. z)))
--Output: (L y. (L z. z))

--Input: ((L x. (L y. x)) (L a. a))
--Output: (L y. (L a. a))

--Input: (L x. ((L y. y) x))
--Output: (L x. x)

-- (((L x. (L y. x)) (L a. a)) (L b. b))
untyped = evaluate [] (Apply (Apply (Lambda "x" (Lambda "y" (Literal "x"))) (Lambda "a" (Literal "a"))) (Apply (Lambda "x" (Apply (Literal "x") (Literal "x"))) (Lambda "x" (Apply (Literal "x") (Literal "x")))))
  -- (L a. a)
cleaned = Lambda "a" (Literal "a") 