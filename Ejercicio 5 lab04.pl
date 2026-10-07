{
 "cells": [
  {
   "cell_type": "code",
   "execution_count": 4,
   "id": "66890d21-131d-4ad7-9758-70ef04befa0c",
   "metadata": {},
   "outputs": [],
   "source": [
    "import Data.List (sortOn)\n",
    "\n",
    "type Estado = (Int, Int)\n",
    "type Nodo = Estado\n",
    "type Costo = Int\n",
    "type Grafo = [(Nodo, [(Nodo, Costo)])]\n",
    "\n",
    "capA, capB :: Int\n",
    "capA = 5\n",
    "capB = 3\n",
    "\n",
    "transferirAB :: Estado -> Estado\n",
    "transferirAB (a, b) = \n",
    "  let p = min a (capB - b)\n",
    "  in (a - p, b + p)\n",
    "\n",
    "transferirBA :: Estado -> Estado\n",
    "transferirBA (a, b) = \n",
    "  let p = min (capA - a) b\n",
    "  in (a + p, b - p)\n",
    "\n",
    "vecinos :: Estado -> [(Estado, Costo)]\n",
    "vecinos (a,b) =\n",
    "  [ ((capA, b), 1)            -- Llenar jarra A\n",
    "  , ((a, capB), 1)            -- Llenar jarra B\n",
    "  , ((0, b), 1)               -- Vaciar jarra A\n",
    "  , ((a, 0), 1)               -- Vaciar jarra B\n",
    "  , (transferirAB (a,b), 1)   -- Verter A->B\n",
    "  , (transferirBA (a,b), 1)   -- Verter B->A\n",
    "  ]\n",
    "\n",
    "\n",
    "heuristica :: Estado -> Costo\n",
    "heuristica (a, b) = abs (a - 4)\n",
    "\n",
    "aStar :: (Nodo -> [(Nodo, Costo)]) -> (Nodo -> Costo) -> Nodo -> Nodo -> [(Nodo, Costo)]\n",
    "aStar sucesores heuristica inicio meta = buscar [(inicio, 0)] []\n",
    "  where\n",
    "    buscar [] _ = []\n",
    "    buscar ((nodo, costo):cola) visitados\n",
    "      | nodo == meta = [(nodo, costo)]\n",
    "      | nodo `elem` visitados = buscar cola visitados\n",
    "      | otherwise =\n",
    "          let nuevos = [ (v, costo + c) | (v, c) <- sucesores nodo, v `notElem` visitados ]\n",
    "              ordenados = sortOn (\\(v, c) -> c + heuristica v) (cola ++ nuevos)\n",
    "          in (nodo, costo) : buscar ordenados (nodo : visitados)\n",
    "\n",
    "inicio, meta :: Estado\n",
    "inicio = (0, 0)\n",
    "meta   = (4, 0)\n",
    "\n",
    "main :: IO ()\n",
    "main = print (aStar vecinos heuristica inicio meta)\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 5,
   "id": "4b16aa21-0cad-4d87-8a72-225bdcea52af",
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/plain": [
       "[((0,0),0),((5,0),1),((5,3),2),((2,3),2),((0,3),1),((3,0),2),((3,3),3),((2,0),3),((5,1),4),((0,2),4),((5,2),5),((4,3),6),((4,0),7)]"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "main"
   ]
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "Haskell",
   "language": "haskell",
   "name": "haskell"
  },
  "language_info": {
   "codemirror_mode": "ihaskell",
   "file_extension": ".hs",
   "mimetype": "text/x-haskell",
   "name": "haskell",
   "pygments_lexer": "Haskell",
   "version": "9.6.7"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 5
}
