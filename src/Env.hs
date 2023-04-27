module Env where
    emptyEnv :: [(String, a)]
    emptyEnv = []

    extendEnv :: (String, a) -> [(String, a)] -> [(String, a)]
    extendEnv (name, val) env = env ++ [(name, val)]

    applyEnv :: String -> [(String, a)] -> a
    applyEnv target [] = error "Error: Variable Undefined"
    applyEnv target ((name, val) : env) = if target == name 
                                             then val 
                                             else applyEnv target env

    emptyEnv' :: String -> a
    emptyEnv' = \name -> error "Error: Variable Undefined"

    extendEnv' :: (String, a) -> (String -> a) -> String -> a
    extendEnv' (name, val) fenv = \key -> if name == key
                                             then val
                                             else fenv name

    applyEnv' :: String -> (String -> a) -> a
    applyEnv' name fenv = fenv name