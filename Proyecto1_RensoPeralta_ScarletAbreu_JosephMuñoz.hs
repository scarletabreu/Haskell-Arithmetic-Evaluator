-- Se define el tipp algebraico Expr para representar expresiones aritméticas.
-- Cada uno de los constructores representa una operación o un valor numérico.
-- Val Double es un valor numerico. 
-- Lo demás son operaciones que reciben DOS expresiones como argumentos.

-- Permite combinar Add (Val 5) (Val 3) para representar 5 + 3, por ejemplo.

-- Se hizo así, ya que en haskell es algo complicado recibir una expresión como "5 + 3" directamente
--, entonces se optó por pedir los operandos y el operador por separado, y luego construir la expresión 
-- sando parseOp (declarado más abajo).

data Expr = Val Double
           | Add Expr Expr
           | Sub Expr Expr
           | Mul Expr Expr
           | Div Expr Expr

-- applyOp viene que aplicar operacion, esto quiere decir que recibe una funcion matematica cualquiera
-- ya sea sumar, restar o multiplicar y dos valores envueltos en Maybe. 
-- Si ambos valores son Just, se aplica la operacion (op) y devuelve el resultado.

-- Si cualquiera de estos es Nothing, es decir, hay algun error, devuelve el mismo nothing.
applyOp :: (Double -> Double -> Double) -> Maybe Double -> Maybe Double -> Maybe Double
applyOp op (Just x) (Just y) = Just (op x y)
applyOp _   _             _  = Nothing

-- En este caso, la división es especial porque necesita su propia función,
-- existe la posibilidad de que el divisor sea cero, lo cual es un error.
-- Si el divisor es cero, devuelve Nothing. Si ambos son Just, realiza la división.
evalDiv :: Maybe Double -> Maybe Double -> Maybe Double
evalDiv (Just _) (Just 0) = Nothing
evalDiv (Just x) (Just y) = Just (x / y)
evalDiv _        _        = Nothing

-- Aquí se usa el Expr para evaluar la expresión. Si el valor es solo un numero,
-- devuelve el mismo numero. Si es una operacion, eval se llama recursivamente para 
-- evaluar los operandos y luego aplica la operacion correspondiente usando applyOp o evalDiv.
eval :: Expr -> Maybe Double
eval (Val n)   = Just n
eval (Add x y) = applyOp (+) (eval x) (eval y)
eval (Sub x y) = applyOp (-) (eval x) (eval y)
eval (Mul x y) = applyOp (*) (eval x) (eval y)
eval (Div x y) = evalDiv (eval x) (eval y)

-- ParseOp permite recibir el operador como texto y los dos numeros,
-- y construye el Expr que toque. Si el operador no es reconocido, devuelve un Val 
-- con el primer numero (esto es solo para evitar errores.
parseOp :: String -> Double -> Double -> Expr
parseOp "+" x y = Add (Val x) (Val y)
parseOp "-" x y = Sub (Val x) (Val y)
parseOp "*" x y = Mul (Val x) (Val y)
parseOp "/" x y = Div (Val x) (Val y)
parseOp _   x _ = Val x

-- formatResult toma el resultado de eval (que es Maybe Double) y lo convierte en una cadena legible.
-- Basicamente se encarga de mostrar el resultado o un mensaje de error si la operación no es válida 
-- o si hubo una división por cero.
formatResult :: Maybe Double -> String
formatResult (Just r) = "Resultado: " ++ show r
formatResult Nothing  = "Error: operación inválida o división por cero"

-- Esta funcion permite leer un numero desde la consola con getLine
-- Al ifinal retorna una expresión construida con parseOp, que es lo que se va a evaluar en el main.

readExpr :: IO Expr
readExpr = do
    putStr "Operando izquierdo: "
    x <- readLn :: IO Double
    putStr "Operador (+, -, *, /): "
    op <- getLine
    putStr "Operando derecho: "
    y <- readLn :: IO Double
    return (parseOp op x y)

main :: IO ()
main = do
    putStrLn "--- Evaluador de Expresiones ---"
    expr <- readExpr
    putStrLn (formatResult (eval expr))