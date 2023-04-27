module BoolLexer where

    import Data.Char

    -- Define the `Token` type
    data Token t = TokenBoolConst t |
                        TokenName t |
                         TokenAnd |
                          TokenOr |
                         TokenNot |
                       TokenEqual |
                     TokenKeyword t |
                         TokenEnd
                         deriving (Show)

    -- lexString
    -- Consume a `String` that contains the program text
    -- Produces `[Token]`
    lexString :: String -> [Token String]
    lexString [] = []
    lexString ('=' : rem) = TokenEqual : (lexString rem)
    lexString ('*' : rem) = TokenAnd : (lexString rem)
    lexString ('+' : rem) = TokenOr : (lexString rem)
    lexString ('!' : rem) = TokenNot : (lexString rem)
    lexString s = case s of
                        (c : cs) | isSpace c -> lexString cs
                        (c : cs) | isAlpha c -> getName s
                    where
                        getName s = lexName i : lexString cs
                            where (i, cs) = span isAlpha s
                        lexName i = if isReservedWord i
                                    then (TokenKeyword i)
                                    else if isBoolConst i
                                        then (TokenBoolConst i)
                                        else (TokenName i)
                        isReservedWord w = elem w ["let", "in", "lambda", "call"]
                        isBoolConst w = elem w ["True", "False"]