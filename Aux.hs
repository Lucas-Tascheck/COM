module Aux where
import Types
import Semantica

juntaTipo :: Tipo -> [Id] -> [Var]
juntaTipo tipo [] = []
juntaTipo tipo [x] = [x :#: tipo]
juntaTipo tipo (x:t) = [x :#: tipo] ++ juntaTipo tipo t

separaLados :: [(Funcao, (Id, [Var], [Comando]))]
            -> ([Funcao], [(Id, [Var], [Comando])])
separaLados input =
    (map fst input, map snd input)


printPrograma (Prog listaFuncao listaFuncaoBloco listaVars blocoPrincipal) = do {
    print listaFuncao;
    print listaFuncaoBloco;
    print listaVars;
    print blocoPrincipal;
}

printSemantica' p = do putStrLn (fst p)
                       printPrograma (snd p)

printSemantica p = do let sem = semantica p
                      case sem of
                        MS p -> printSemantica' p