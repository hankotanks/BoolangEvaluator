module BoolParser where
    -- imports
    import BoolDefinitions
    import BoolLexer
    
    --
    -- HELPER FUNCTION
    --

    -- parseString
    parseString :: String -> ParseTree
    parseString s = parseTokens $ lexString s

    -- parseTokens
    parseTokens :: [Token String] -> ParseTree
    parseTokens tokens = let (tree, rem) = parseExpression tokens
                             in 
                                case rem of
                                    [] -> tree
                                    _ -> error $
                                           "Error: Tokens Remaining: " ++ show rem

    -- parseExpression
    parseExpression :: [Token String] -> (ParseTree, [Token String])
    parseExpression tokens = 
        case lookAhead tokens of
            TokenBoolConst v -> if v == "True"
                                   then (ValueNode (BoolType True), (accept tokens))
                                   else (ValueNode (BoolType False), (accept tokens))
            TokenName name -> ((IdNode name), (accept tokens))
            --TokenEqual -> (EqualNode, (accept tokens))
            --TokenKeyword "in" -> (InNode, (accept tokens))
            TokenAnd -> let (fstExpr, tokens') = parseExpression $ accept tokens
                            in let (sndExpr, tokens'') = parseExpression tokens'
                                   in
                                      ((AndNode fstExpr sndExpr), tokens'')
            TokenOr -> let (fstExpr, tokens') = parseExpression $ accept tokens
                            in let (sndExpr, tokens'') = parseExpression tokens'
                                   in
                                      ((OrNode fstExpr sndExpr), tokens'')
            TokenNot -> let (expr, tokens') = parseExpression $ accept tokens
                            in 
                                (NotNode expr, tokens')
            TokenKeyword "let" -> let (id, tokens') = parseExpression $ accept tokens
                                      in let (equalSign, tokens'') = parseExpression tokens'
                                             in let (fstExpr, tokens''') = parseExpression tokens''
                                                    in let (inWord, tokens'''') = parseExpression tokens'''
                                                           in let (sndExpr, tokens''''') = parseExpression tokens''''
                                                                  in 
                                                                     ((LetNode id fstExpr sndExpr), tokens''''')
            TokenKeyword "lambda" -> let (id, tokens') = parseExpression $ accept tokens
                                         in let (inWord, tokens'') = parseExpression tokens'
                                                in let (bodyExpr, tokens''') = parseExpression tokens''
                                                       in
                                                          ((LambdaNode id bodyExpr), tokens''')

            TokenKeyword "call" -> let (id, tokens') = parseExpression $ accept tokens
                                       in let (bodyExpr, tokens'') = parseExpression tokens'
                                              in
                                                 ((CallNode id bodyExpr), tokens'')
            _ -> (EmptyNode, accept tokens)


    -- lookAhead
    lookAhead :: [Token String] -> Token String
    lookAhead [] = TokenEnd
    lookAhead (t : tokens) = t

    -- accept
    accept :: [Token String] -> [Token String]
    accept [] = error "Error: No Remaining Token to Accept"
    accept (t : tokens) = tokens
