module BoolEvaluation where

    import BoolDefinitions
    import BoolLexer
    import BoolParser
    import Env

    compileAndRun :: String -> ValueType
    compileAndRun program = evaluate (parseTokens $ lexString program) emptyEnv

    evaluate :: ParseTree -> EnvType -> ValueType
    evaluate tree env = case tree of
                        (ValueNode val) -> val
                        (NotNode val) -> let param = evaluate val env
                                             in case param of
                                                BoolType True -> BoolType False
                                                BoolType False -> BoolType True
                        (AndNode fstVal sndVal) -> let fstParam = evaluate fstVal env
                                                       sndParam = evaluate sndVal env
                                                         in case fstParam of
                                                            BoolType True -> case sndParam of
                                                                             BoolType True -> BoolType True
                                                                             BoolType False -> BoolType False
                                                            BoolType False -> BoolType False
                        (OrNode fstVal sndVal) -> let fstParam = evaluate fstVal env
                                                      sndParam = evaluate sndVal env
                                                        in case fstParam of
                                                            BoolType True -> BoolType True
                                                            BoolType False -> case sndParam of
                                                                              BoolType True -> BoolType True
                                                                              BoolType False -> BoolType False
                        (IdNode name) -> applyEnv name env
                        (LetNode (IdNode name) val body) -> let valResult = evaluate val env
                                                                in
                                                                   evaluate body (extendEnv (name, valResult) env)
                        (LambdaNode (IdNode formalParam) body) -> ClosureType (Closure formalParam body env)
                        (CallNode (IdNode funcName) expr) -> let result = applyEnv funcName env
                                                                 in
                                                                    case result of
                                                                         ClosureType (Closure paramName functionBody functionEnv) -> evaluate functionBody (extendEnv (paramName, (evaluate expr env)) functionEnv)
                                                                         _ -> error "Error: Illegal Function Call"
                        _ -> error "Error: Unable to Parse AST` Tree"