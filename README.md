# Boolang
A useless language where booleans are the only data. Created to explore lexer and parser design

## Overview
```
 let x = lambda y in !y in
    in let z = + * True False True in 
       call x z 
```

- Define functions with `lambda [fparam] in [expr]`.
- Scope variables with `let [var] = [definition] in [expr]`.
- Call functions with `call [func] [expr]`
- Logical operators precede their arguments
   - `+ * True False True` is equivalent to _(True and False) or True_
   
## Usage
```
> cd ./src/
> ghci
ghci> :l BoolEvaluation.hs
ghci> compileAndRun "let neg = lambda i in !i in call neg False"
BoolType True
```
