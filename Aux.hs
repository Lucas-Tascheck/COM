module Aux where
import Types

juntaTipo :: Tipo -> [Id] -> [Var]
juntaTipo tipo [] = []
juntaTipo tipo [x] = [x :#: tipo]
juntaTipo tipo (x:t) = [x :#: tipo] ++ juntaTipo tipo t

separaLados :: [(Funcao, (Id, [Var], [Comando]))]
            -> ([Funcao], [(Id, [Var], [Comando])])
separaLados input =
    (map fst input, map snd input)