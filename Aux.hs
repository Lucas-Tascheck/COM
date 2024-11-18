module Aux where
import Types

sheki :: Tipo -> [Id] -> [Var]
sheki tipo [] = []
sheki tipo [x] = [x :#: tipo]
sheki tipo (x:t) = [x :#: tipo] ++ sheki tipo t