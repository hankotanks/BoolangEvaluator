module BoolDefinitions where
    data ParseTree = AndNode ParseTree ParseTree           |
                      OrNode ParseTree ParseTree           |
                     NotNode ParseTree                     |
                   ValueNode ValueType                     |
                      IdNode String                        |
                      InNode                               |
                   EqualNode                               |
                     LetNode ParseTree ParseTree ParseTree |
                  LambdaNode ParseTree ParseTree           |
                    CallNode ParseTree ParseTree           |
                   EmptyNode
                    deriving (Show)

    data ClosureStructure = Closure String ParseTree EnvType
                            deriving (Show)
    
    data ValueType = BoolType Bool |
                     ClosureType ClosureStructure
                     deriving (Show)

    type EnvType = [(String, ValueType)]

    