--import Marcador
import Matriz
--import MarcaMatriz
import Solucionador
type OperatorMatrix = [[Char]]
--type Operator = Char

--type NumbersMatrix = [[Int]]


-- exemplo de matriz de operadores para tabuleiro 4x4
operatorMatrix :: OperatorMatrix
operatorMatrix = [['<', '|', '>', '|'],
                  ['^', '^', 'v', 'v'],
                  ['>', '|', '<', '|'],
                  ['|', '|', '|', '|'],
                  ['<', '|', '>', '|'],
                  ['v', 'v', '^', '^'],
                  ['>', '|', '<', '|']]

-- exemplo 2 de matriz de operadores para tabuleiro 4x4
operatorMatrix4 :: OperatorMatrix
operatorMatrix4 = [['<', '|', '<', '|'], 
                   ['v', '^', '^', 'v'], 
                   ['<', '|', '>', '|'], 
                   ['|', '|', '|', '|'], 
                   ['>', '|', '<', '|'], 
                   ['v', '^', '^', 'v'], 
                   ['>', '|', '>', '|']]

-- exemplo de matriz de operadores para tabuleiro 6x6
operatorMatrix6 :: OperatorMatrix
operatorMatrix6 = [
  ['<', '|', '>', '|', '>', '|'],
  ['^', 'v', '^', '^', '^', 'v'],
  ['>', '|', '>', '|', '>', '|'],
  ['v', 'v', 'v', '^', '^', '^'],
  ['<', '|', '<', '|', '<', '|'],
  ['|', '|', '|', '|', '|', '|'],
  ['<', '|', '<', '|', '<', '|'],
  ['^', '^', 'v', 'v', '^', 'v'],
  ['>', '|', '<', '|', '<', '|'],
  ['v', 'v', '^', '^', '^', 'v'],
  ['>', '|', '<', '|', '>', '|']]

main :: IO ()
main = do

   -- putStrLn " "
    -- Utilizando o algoritmo de resolução proposto sem backtracking
    --printMatrix(solvePuzzle operatorMatrix)
    --printMatrix(solvePuzzle operatorMatrix4)
    --printMatrix(solvePuzzle operatorMatrix6)
    --printMatrix(solvePuzzle operatorMatrix9)


    -- Utilizando somente backtracking
    printMatrix(solveMatrix operatorMatrix)
    --printMatrix(solveMatrix operatorMatrix4)
    printMatrix(solveMatrix operatorMatrix6)
    --printMatrix(solveMatrix operatorMatrix9)



        


