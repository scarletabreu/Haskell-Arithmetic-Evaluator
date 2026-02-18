-- Tipo algebraico que representa un árbol abstracto de expresiones aritméticas
data Expr = Val Double
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr

-- Aplica una operación binaria si ambos operandos son Just; en otro caso propaga Nothing
applyOp :: (Double -> Double -> Double) -> Maybe Double -> Maybe Double -> Maybe Double
applyOp op (Just x) (Just y) = Just (op x y)
applyOp _  _        _        = Nothing

-- Evalúa división de forma segura retornando Nothing ante divisor cero o error previo
evalDiv :: Maybe Double -> Maybe Double -> Maybe Double
evalDiv (Just _) (Just 0) = Nothing
evalDiv (Just x) (Just y) = Just (x / y)
evalDiv _        _        = Nothing

-- Evalúa recursivamente el árbol Expr produciendo un resultado seguro en Maybe
eval :: Expr -> Maybe Double
eval (Val n)   = Just n
eval (Add x y) = applyOp (+) (eval x) (eval y)
eval (Sub x y) = applyOp (-) (eval x) (eval y)
eval (Mul x y) = applyOp (*) (eval x) (eval y)
eval (Div x y) = evalDiv (eval x) (eval y)

-- Muestra el resultado evaluado junto con una descripción
test :: String -> Expr -> IO ()
test descripcion expresion = do
    putStr (descripcion ++ ": ")
    print (eval expresion)

main :: IO ()
main = do
    putStrLn "--- Pruebas del Evaluador de Expresiones ---"

    test "Valor constante (10.5)" (Val 10.5)
    test "Suma simple (5 + 3)" (Add (Val 5) (Val 3))
    test "Compuesta ((10-2)*3)" (Mul (Sub (Val 10) (Val 2)) (Val 3))
    test "Error: Division por cero" (Div (Val 10) (Val 0))
    test "Error propagado (5 + (10/0))" (Add (Val 5) (Div (Val 10) (Val 0)))