module Semantica where

import Types
import Control.Monad (unless, when)

reset = "\x1b[0m"
vermelho = "\x1b[31m"
amarelo = "\x1b[33m"

newtype Semantica a = MS (String, a) deriving Show

instance Functor Semantica where
         fmap f (MS (s, a)) = MS (s, f a)

instance Applicative Semantica where
    pure x = MS ("", x)
    MS(s1, f) <*> MS(s2, x) = MS (s1 <> s2, f x)

instance Monad Semantica where
    MS(s, a) >>= f = let MS(s', b) = f a in MS (s++s', b)


erro s = MS (vermelho    ++ "Error: "    ++ reset ++ s ++ "\n", ())
adv s = MS (amarelo ++ "Warning: "  ++ reset ++ s ++ "\n", ())
semantica programa@(Prog lFuncao lFuncaoBloco lVars bPrincipal) = do
  bPrincipal1 <- normDouble lFuncao lFuncaoBloco lVars bPrincipal
  lFuncaoBloco1 <- normalizeReturnType lFuncao lFuncaoBloco lFuncao lFuncaoBloco
  bPrincipal3 <- normDoubleR lFuncao lFuncaoBloco1 lVars bPrincipal1
  msgDeErroSo <- verifyTypeExprFuncao lFuncao lFuncaoBloco1 lFuncao lFuncaoBloco1
  msgDeAdvSo <- callVerifyDoubleIntFuncao lFuncao lFuncaoBloco1 lFuncao lFuncaoBloco1;
  msgDeErroSo <- verifyTypeExpr lFuncao lFuncaoBloco1 lVars bPrincipal3
  msgDeAdvSo <- verifyDoubleInt lFuncao lFuncaoBloco1 lVars bPrincipal3 "";
  msgDeErroSo <- verifyIfExists lFuncao lFuncaoBloco1 lVars bPrincipal3 "";
  msgDeErroSo <- callVerifyFuncaoExists lFuncao lFuncaoBloco1 lFuncao lFuncaoBloco1;
  msgDeErroSo <- verificaSeVarRepete lVars "";
  msgDeErroSo <- verifyIfVarRepeatOnFuncao lFuncaoBloco1;
  msgDeErroSo <- verificaSeFuncaoRepete lFuncao;
  return (Prog lFuncao lFuncaoBloco1 lVars bPrincipal3)



verifyParams [] _ _ _ = return []
verifyParams (elem:xs) [] listaVars listaFuncoes = do {

     transformedRest <- verifyParams xs [] listaVars listaFuncoes;
     return (elem : transformedRest)
 }
verifyParams (elem:xs) ((_:#:tipo):tipos) listaVars listaFuncoes = do {
    transformedElem <- if tipo==TDouble then
        transformDouble elem listaVars listaFuncoes
    else if tipo==TInt then
        transformInt elem listaVars listaFuncoes elem
    else
        return elem;
    transformedRest <- verifyParams xs tipos listaVars listaFuncoes;
    return (transformedElem : transformedRest)
}

transformDouble (Add e1 e2) listaVars listaFuncoes = do
                                                    transformedE1 <- transformDouble e1 listaVars listaFuncoes;
                                                    transformedE2 <- transformDouble e2 listaVars listaFuncoes;
                                                    return (Add transformedE1 transformedE2)
transformDouble (Sub e1 e2) listaVars listaFuncoes = do
                                                    transformedE1 <- transformDouble e1 listaVars listaFuncoes;
                                                    transformedE2 <- transformDouble e2 listaVars listaFuncoes;
                                                    return (Sub transformedE1 transformedE2)
transformDouble (Mul e1 e2) listaVars listaFuncoes = do
                                                    transformedE1 <- transformDouble e1 listaVars listaFuncoes;
                                                    transformedE2 <- transformDouble e2 listaVars listaFuncoes;
                                                    return (Mul transformedE1 transformedE2)
transformDouble (Div e1 e2) listaVars listaFuncoes = do
                                                    transformedE1 <- transformDouble e1 listaVars listaFuncoes;
                                                    transformedE2 <- transformDouble e2 listaVars listaFuncoes;
                                                    return (Div transformedE1 transformedE2)
transformDouble (Neg e) listaVars listaFuncoes = do
                                                    transformedE1 <- transformDouble e listaVars listaFuncoes;
                                                    return (Neg e)
transformDouble e@(Const (CDouble _)) _ _ = return e
transformDouble e@(Const (CInt _)) _ _ = do {
    return (IntDouble e)
}
transformDouble e@(Chamada nome lExpr) listaVars listaFuncoes = do {
    transformedLExpr <- verifyParams lExpr (getTipoParams nome listaFuncoes) listaVars listaFuncoes;
    if verifyIfDeclaracaoFuncaoDouble nome listaFuncoes then do
        return (Chamada nome transformedLExpr);
    else do
        return (IntDouble (Chamada nome transformedLExpr));
}
transformDouble e@(IdVar nome) listaVars listaFuncoes = do {
    if verifyIfDeclaracaoDouble nome listaVars then
    return e;
    else
    return $ IntDouble e;
    }
transformDouble e@(Lit str) listaVars listaFuncoes = do {
    return e;
}

transformInt (Add e1 e2) listaVars listaFuncoes elemCompleto = do
                                                transformedE1 <- transformInt e1 listaVars listaFuncoes elemCompleto;
                                                transformedE2 <- transformInt e2 listaVars listaFuncoes elemCompleto;
                                                return (Add transformedE1 transformedE2);
transformInt (Sub e1 e2) listaVars listaFuncoes elemCompleto = do
                                                transformedE1 <- transformInt e1 listaVars listaFuncoes elemCompleto;
                                                transformedE2 <- transformInt e2 listaVars listaFuncoes elemCompleto;
                                                return (Sub transformedE1 transformedE2);
transformInt (Mul e1 e2) listaVars listaFuncoes elemCompleto = do
                                                transformedE1 <- transformInt e1 listaVars listaFuncoes elemCompleto;
                                                transformedE2 <- transformInt e2 listaVars listaFuncoes elemCompleto;
                                                return (Mul transformedE1 transformedE2);
transformInt (Div e1 e2) listaVars listaFuncoes elemCompleto = do
                                                transformedE1 <- transformInt e1 listaVars listaFuncoes elemCompleto;
                                                transformedE2 <- transformInt e2 listaVars listaFuncoes elemCompleto;
                                                return (Div transformedE1 transformedE2);
transformInt (Neg e) listaVars listaFuncoes elemCompleto = do
                                                transformedE <- transformInt e listaVars listaFuncoes elemCompleto;
                                                return (Neg transformedE);
transformInt e@(Const (CDouble _)) _ _ elemCompleto = do {
        return (DoubleInt e)
     }
transformInt e@(Const (CInt _)) _ _ elemCompleto = return e
transformInt e@(Chamada nome lExpr) listaVars listaFuncoes elemCompleto = do {
        transformedLExpr <- verifyParams lExpr (getTipoParams nome listaFuncoes) listaVars listaFuncoes;
        if verifyIfDeclaracaoFuncaoDouble nome listaFuncoes then do
            return (DoubleInt (Chamada nome transformedLExpr));
        else do
            return (Chamada nome transformedLExpr);
    }
transformInt e@(IdVar nome) listaVars listaFuncoes elemCompleto = do {
     if verifyIfDeclaracaoDouble nome listaVars then
        return (DoubleInt e);
     else
        return e;
     }
transformInt e@(Lit str) listaVars listaFuncoes elemCompleto = do {
    return e;
}

normDouble _ _ _ [] = return []
normDouble declaracoesFuncao blocosFuncoes declaracaoMain (elem@(Atrib nome e):xs) = do
    if verifyIfDeclaracaoDouble nome declaracaoMain then do
        transformedE <- transformDouble e declaracaoMain declaracoesFuncao
        normDouble declaracoesFuncao blocosFuncoes declaracaoMain xs >>= \rest -> return (Atrib nome transformedE : rest)
    else do
        transformedE <- transformInt e declaracaoMain declaracoesFuncao e
        normDouble declaracoesFuncao blocosFuncoes declaracaoMain xs >>= \rest -> return (Atrib nome transformedE : rest)
normDouble declaracoesFuncao blocosFuncoes declaracaoMain (elem:xs) = do
    normDouble declaracoesFuncao blocosFuncoes declaracaoMain xs >>= \rest -> return (elem : rest)

callVerifyDoubleIntFuncao [] _ _ _ = return []
callVerifyDoubleIntFuncao _ [] _ _ = return []
callVerifyDoubleIntFuncao (declaracao@(nome :->: _):restoDeclaracao) ((_,_,bloco):restoBloco) declaracoesFuncao blocosFuncoes = do
    verifica <- verifyDoubleInt declaracoesFuncao blocosFuncoes declaracao bloco nome;
    rest <- callVerifyDoubleIntFuncao restoDeclaracao restoBloco declaracoesFuncao blocosFuncoes;
    return (verifica : rest);


verifyDoubleInt _ _ _ [] _ = return [];
verifyDoubleInt declaracoesFuncao blocosFuncoes declaracaoMain (elem@(Atrib nome e):xs) onde = do
    hasDoubleInt <- verifyIfDoubleInt e declaracoesFuncao blocosFuncoes declaracaoMain;
    if hasDoubleInt then do
        traduzidoE <- translateExpr e;
        if onde == "" then do
            adv ("Conversão de double para inteiro dentro da Main em: "++nome++" = "++traduzidoE++";");
        else
            adv ("Conversão de double para inteiro dentro da função "++onde++" em: "++nome++" = "++traduzidoE++";");
        verifyDoubleInt declaracoesFuncao blocosFuncoes declaracaoMain xs onde >>= \rest -> return (elem : rest);
    else
        verifyDoubleInt declaracoesFuncao blocosFuncoes declaracaoMain xs onde >>= \rest -> return (elem : rest);
verifyDoubleInt declaracoesFuncao blocosFuncoes declaracaoMain (elem@(Ret (Just e)):xs) onde = do
    hasDoubleInt <- verifyIfDoubleInt e declaracoesFuncao blocosFuncoes declaracaoMain;
    if hasDoubleInt then do
        traduzidoE <- translateExpr e;
        if onde == "" then do
            adv ("Conversão de double para inteiro dentro da Main em: return "++traduzidoE++";");
        else
            adv ("Conversão de double para inteiro dentro da função "++onde++" em: return "++traduzidoE++";");
        verifyDoubleInt declaracoesFuncao blocosFuncoes declaracaoMain xs onde >>= \rest -> return (elem : rest);
    else
        verifyDoubleInt declaracoesFuncao blocosFuncoes declaracaoMain xs onde >>= \rest -> return (elem : rest);
verifyDoubleInt declaracoesFuncao blocosFuncoes declaracaoMain (elem@(If _ bloco blocoElse):xs) onde = do {
     verifyDoubleInt declaracoesFuncao blocosFuncoes declaracaoMain bloco onde;
     verifyDoubleInt declaracoesFuncao blocosFuncoes declaracaoMain blocoElse onde;
     verifyDoubleInt declaracoesFuncao blocosFuncoes declaracaoMain xs onde >>= \rest -> return (elem : rest);
 }
verifyDoubleInt declaracoesFuncao blocosFuncoes declaracaoMain (elem@(While _ bloco):xs) onde = do {
     verifyDoubleInt declaracoesFuncao blocosFuncoes declaracaoMain bloco onde;
     verifyDoubleInt declaracoesFuncao blocosFuncoes declaracaoMain xs onde >>= \rest -> return (elem : rest);
 }
verifyDoubleInt declaracoesFuncao blocosFuncoes declaracaoMain (_:xs) onde = do {
    verifyDoubleInt declaracoesFuncao blocosFuncoes declaracaoMain xs onde;
}

repeatFuncao _ [] = return False
repeatFuncao funcao (e@(nomeFuncao :->: _):xs) = if nomeFuncao==funcao then return True else repeatFuncao funcao xs

verificaSeFuncaoRepete [] = return []
verificaSeFuncaoRepete (x@(nomeFuncao :->: _):xs) = do {
    resu <- repeatFuncao nomeFuncao xs;
    when resu (erro ("Função "++nomeFuncao++" declarada mais de uma vez"));
    rest <- verificaSeFuncaoRepete xs;
    return (x : rest)
}

verifyIfVarRepeatOnFuncao [] = return []
verifyIfVarRepeatOnFuncao (x@(nome, dec, _):funcoes) = do {
    elem <- verificaSeVarRepete dec nome;
    rest <- verifyIfVarRepeatOnFuncao funcoes;
    return (elem:rest)
}

repeatVar _ [] = return False
repeatVar var (e@(nome :#: _):xs) = if nome==var then return True else repeatVar var xs

verificaSeVarRepete [] onde = return []
verificaSeVarRepete (x@(nome :#: _):xs) onde = do
    resu <- repeatVar nome xs
    if resu then do
        if onde=="" then
            erro ("Variável "++nome++" declarada mais de uma vez na Main")
        else
            erro ("Variável "++nome++" declarada mais de uma vez na função "++onde)
        rest <- verificaSeVarRepete xs onde
        return (x : rest)
    else do
        rest <- verificaSeVarRepete xs onde
        return (x : rest)

catchFisrtFormatedElem ((IdVar nome):_) = return ("Variável "++nome);
catchFisrtFormatedElem ((Chamada nome _):_) = return ("Função "++nome);

emptyList [] = return True
emptyList _ = return False

callVerifyFuncaoExists [] _ _ _ = return []
callVerifyFuncaoExists _ [] _ _ = return []
callVerifyFuncaoExists (declaracao@(nome :->: _):restoDeclaracao) ((_,dec,bloco):restoBloco) declaracoesFuncao blocosFuncoes = do
    verifica <- verifyIfExists declaracoesFuncao blocosFuncoes dec bloco nome;
    rest <- callVerifyFuncaoExists restoDeclaracao restoBloco declaracoesFuncao blocosFuncoes;
    return (verifica : rest);

verifyIfExists _ _ _ [] _ = return []
verifyIfExists declaracoesFuncao blocosFuncoes declaracaoMain (elem@(Atrib nome e):xs) onde = do
    varExiste <- verifyIfVarExists nome declaracaoMain
    if varExiste then do
        listaExprComErro <- verifyIfExistsOnExpr e declaracoesFuncao blocosFuncoes declaracaoMain
        listaEVazia <- emptyList listaExprComErro
        if not listaEVazia then do
            traduzidoE <- translateExpr e
            elemNaoExiste <- catchFisrtFormatedElem listaExprComErro
            if onde == "" then
                erro (elemNaoExiste++", não declarada dentro da Main em: "++nome++" = "++traduzidoE++";")
            else
                erro (elemNaoExiste++", não declarada dentro da função "++onde++" em: "++nome++" = "++traduzidoE++";")
            rest <- verifyIfExists declaracoesFuncao blocosFuncoes declaracaoMain xs onde
            return (elem : rest)
        else do
            rest <- verifyIfExists declaracoesFuncao blocosFuncoes declaracaoMain xs onde
            return (elem : rest)
    else do
        traduzidoE <- translateExpr e
        if onde == "" then do
            erro ("Variável "++nome++", não declarada dentro da Main em: "++nome++" = "++traduzidoE++";")
        else
            erro ("Variável "++nome++", não declarada dentro da função "++onde++" em: "++nome++" = "++traduzidoE++";")
        rest <- verifyIfExists declaracoesFuncao blocosFuncoes declaracaoMain xs onde
        return (elem : rest)
verifyIfExists declaracoesFuncao blocosFuncoes declaracaoMain (elem@(Ret (Just e)):xs) onde = do
    listaExprComErro <- verifyIfExistsOnExpr e declaracoesFuncao blocosFuncoes declaracaoMain
    listaEVazia <- emptyList listaExprComErro
    if not listaEVazia then do
        traduzidoE <- translateExpr e
        elemNaoExiste <- catchFisrtFormatedElem listaExprComErro
        if onde == "" then
            erro (elemNaoExiste++", não declarada dentro da Main em: return "++traduzidoE++";")
        else
            erro (elemNaoExiste++", não declarada dentro da função "++onde++" em: return "++traduzidoE++";")
        rest <- verifyIfExists declaracoesFuncao blocosFuncoes declaracaoMain xs onde
        return (elem : rest)
    else do
        rest <- verifyIfExists declaracoesFuncao blocosFuncoes declaracaoMain xs onde
        return (elem : rest)
verifyIfExists declaracoesFuncao blocosFuncoes declaracaoMain (elem@(If _ bloco blocoElse):xs) onde = do
    verifyIfExists declaracoesFuncao blocosFuncoes declaracaoMain bloco onde;
    verifyIfExists declaracoesFuncao blocosFuncoes declaracaoMain blocoElse onde;
    rest <- verifyIfExists declaracoesFuncao blocosFuncoes declaracaoMain xs onde
    return (elem : rest)
verifyIfExists declaracoesFuncao blocosFuncoes declaracaoMain (elem@(While _ bloco):xs) onde = do
    verifyIfExists declaracoesFuncao blocosFuncoes declaracaoMain bloco onde;
    rest <- verifyIfExists declaracoesFuncao blocosFuncoes declaracaoMain xs onde
    return (elem : rest)
verifyIfExists declaracoesFuncao blocosFuncoes declaracaoMain (elem:xs) onde = do
    rest <- verifyIfExists declaracoesFuncao blocosFuncoes declaracaoMain xs onde
    return (elem : rest)

verifyIfVarExists _ [] = return False
verifyIfVarExists nome ((nomeVar :#: _):xs) = if nomeVar==nome then return True else verifyIfVarExists nome xs

verifyIfFunctionExists _ [] = return False
verifyIfFunctionExists nome ((nomeFuncao :->: _):xs) = if nomeFuncao==nome then return True else verifyIfFunctionExists nome xs

verifyIfExistsOnExprLists [] _ _ _ = return [];
verifyIfExistsOnExprLists (x:xs) declaracoesFuncao blocosFuncoes declaracaoMain = do {
     transformed <- verifyIfExistsOnExpr x declaracoesFuncao blocosFuncoes declaracaoMain;
     rest <- verifyIfExistsOnExprLists xs declaracoesFuncao blocosFuncoes declaracaoMain;
     return (transformed ++ rest);
 }

verifyIfExistsOnExpr (Add e1 e2) declaracoesFuncao blocosFuncoes declaracaoMain = do {
                                                         transformedE1 <- verifyIfExistsOnExpr e1 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         transformedE2 <- verifyIfExistsOnExpr e2 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         return (transformedE1 ++ transformedE2);
                                                     }
verifyIfExistsOnExpr (Sub e1 e2) declaracoesFuncao blocosFuncoes declaracaoMain = do {
                                                         transformedE1 <- verifyIfExistsOnExpr e1 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         transformedE2 <- verifyIfExistsOnExpr e2 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         return (transformedE1 ++ transformedE2);
                                                     }
verifyIfExistsOnExpr (Mul e1 e2) declaracoesFuncao blocosFuncoes declaracaoMain = do {
                                                         transformedE1 <- verifyIfExistsOnExpr e1 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         transformedE2 <- verifyIfExistsOnExpr e2 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         return (transformedE1 ++ transformedE2);
                                                     }
verifyIfExistsOnExpr (Div e1 e2) declaracoesFuncao blocosFuncoes declaracaoMain = do {
                                                        transformedE1 <- verifyIfExistsOnExpr e1 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                        transformedE2 <- verifyIfExistsOnExpr e2 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                        return (transformedE1 ++ transformedE2);
                                                    }
verifyIfExistsOnExpr (Neg e) declaracoesFuncao blocosFuncoes declaracaoMain = do {
                                                        verifyIfExistsOnExpr e declaracoesFuncao blocosFuncoes declaracaoMain;
                                                    }
verifyIfExistsOnExpr e@(Const (CDouble _)) _ _ _ = return []
verifyIfExistsOnExpr e@(Const (CInt _)) _ _ _ = return []
verifyIfExistsOnExpr e@(Chamada nome params) declaracoesFuncao blocosFuncoes declaracaoMain = do {
    existeFuncao <- verifyIfFunctionExists nome declaracoesFuncao;
    if existeFuncao then do
        verifyIfExistsOnExprLists params declaracoesFuncao blocosFuncoes declaracaoMain;
    else
        return [e]
}
verifyIfExistsOnExpr e@(IdVar nome) declaracoesFuncao blocosFuncoes declaracaoMain = do {
    varExiste <- verifyIfVarExists nome declaracaoMain;
    if varExiste then
        return [];
    else
        return [e];
}
verifyIfExistsOnExpr e@(Lit _) _ _ _ = return []
verifyIfExistsOnExpr e@(DoubleInt elem) declaracoesFuncao blocosFuncoes declaracaoMain = do {
    verifyIfExistsOnExpr elem declaracoesFuncao blocosFuncoes declaracaoMain;
 }
verifyIfExistsOnExpr e@(IntDouble elem) declaracoesFuncao blocosFuncoes declaracaoMain = do {
    verifyIfExistsOnExpr elem declaracoesFuncao blocosFuncoes declaracaoMain;
 }

verifyIfDoubleInt (Add e1 e2) declaracoesFuncao blocosFuncoes declaracaoMain = do {
                                                         transformedE1 <- verifyIfDoubleInt e1 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         transformedE2 <- verifyIfDoubleInt e2 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         return (transformedE1 || transformedE2);
                                                     }
verifyIfDoubleInt (Sub e1 e2) declaracoesFuncao blocosFuncoes declaracaoMain = do {
                                                         transformedE1 <- verifyIfDoubleInt e1 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         transformedE2 <- verifyIfDoubleInt e2 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         return (transformedE1 || transformedE2);
                                                     }
verifyIfDoubleInt (Mul e1 e2) declaracoesFuncao blocosFuncoes declaracaoMain = do {
                                                         transformedE1 <- verifyIfDoubleInt e1 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         transformedE2 <- verifyIfDoubleInt e2 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         return (transformedE1 || transformedE2);
                                                     }
verifyIfDoubleInt (Div e1 e2) declaracoesFuncao blocosFuncoes declaracaoMain = do {
                                                         transformedE1 <- verifyIfDoubleInt e1 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         transformedE2 <- verifyIfDoubleInt e2 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         return (transformedE1 || transformedE2);
                                                     }
verifyIfDoubleInt (Neg e) declaracoesFuncao blocosFuncoes declaracaoMain = do {
                                                         verifyIfDoubleInt e declaracoesFuncao blocosFuncoes declaracaoMain;
                                                     }
verifyIfDoubleInt e@(Const (CDouble _)) _ _ _ = return False
verifyIfDoubleInt e@(Const (CInt _)) _ _ _ = return False
verifyIfDoubleInt e@(Chamada nome params) declaracoesFuncao blocosFuncoes declaracaoMain = do {
    doubleIntEmParams <- verifyIfDoubleIntEmParams params declaracoesFuncao blocosFuncoes declaracaoMain;
    paramsTraduzido <- translateExprComoParams params;
    when doubleIntEmParams (erro ("Conversão de double para inteiro nos parametros da função: "++nome++"( "++paramsTraduzido++" )"));
    return False
 }
verifyIfDoubleInt e@(IdVar nome) declaracoesFuncao blocosFuncoes declaracaoMain = do {
     return False
 }
verifyIfDoubleInt e@(Lit _) _ _ _ = return False
verifyIfDoubleInt e@(DoubleInt elem) _ _ _ = do {
     return (True)
 }
verifyIfDoubleInt e _ _ _ = return False

verifyIfDoubleIntEmParams [] _ _ _ = return False
verifyIfDoubleIntEmParams (x:xs) declaracoesFuncao blocosFuncoes declaracaoMain = do {
     traduzido <- verifyIfDoubleInt x declaracoesFuncao blocosFuncoes declaracaoMain;
     rest <- verifyIfDoubleIntEmParams xs declaracoesFuncao blocosFuncoes declaracaoMain;
     return (traduzido || rest)
 }

verifyTypeExprFuncao [] [] _ _ = return []
verifyTypeExprFuncao (y:ys) ((_,declaracoes,bloco):xs) declaracoesFuncao blocosFuncoes = do {
    elem <- verifyTypeExpr declaracoesFuncao blocosFuncoes declaracoes bloco;
    rest <- verifyTypeExprFuncao ys xs declaracoesFuncao blocosFuncoes;
    return (elem : rest)
}

verifyTypeExpr _ _ _ [] = return []
verifyTypeExpr declaracoesFuncao blocosFuncoes declaracaoMain (elem@(Atrib nome e):xs) = do
    case getTipoVar nome declaracaoMain of
        Just tipo -> do {
             compartivel <- verifyTypesWithExpr e tipo declaracoesFuncao blocosFuncoes declaracaoMain;
             if not compartivel then do
                 traduzidoE <- translateExpr e
                 erro ("Tipos incompatíveis em: " ++ nome ++ " = " ++ traduzidoE ++ ";")
                 rest <- verifyTypeExpr declaracoesFuncao blocosFuncoes declaracaoMain xs
                 return (elem : rest)
             else do
                 rest <- verifyTypeExpr declaracoesFuncao blocosFuncoes declaracaoMain xs
                 return (elem : rest)
             }
        Nothing -> do
            traduzidoE <- translateExpr e
            erro ("Variável " ++ nome ++ " não definida em: " ++ nome ++ " = " ++ traduzidoE ++ ";")
            rest <- verifyTypeExpr declaracoesFuncao blocosFuncoes declaracaoMain xs
            return (elem : rest)
verifyTypeExpr declaracoesFuncao blocosFuncoes declaracaoMain (elem@(If _ bloco blocoElse):xs) = do
    discart1 <- verifyTypeExpr declaracoesFuncao blocosFuncoes declaracaoMain bloco;
    discart2 <- verifyTypeExpr declaracoesFuncao blocosFuncoes declaracaoMain blocoElse;
    rest <- verifyTypeExpr declaracoesFuncao blocosFuncoes declaracaoMain xs;
    return (elem : rest);
verifyTypeExpr declaracoesFuncao blocosFuncoes declaracaoMain (elem@(While cExprL bloco):xs) = do
    lista <- getListaTiposExprL cExprL declaracoesFuncao blocosFuncoes declaracaoMain;
    saoCompativeis <- listCompatibleTypes lista;
    if not saoCompativeis then do
        traduzidoE <- translateExprL cExprL;
        erro ("Tipos incompatíveis dentro do while ( "++ traduzidoE++" )")
        rest <- verifyTypeExpr declaracoesFuncao blocosFuncoes declaracaoMain xs
        return (elem : rest)
    else do
        rest <- verifyTypeExpr declaracoesFuncao blocosFuncoes declaracaoMain xs
        return (elem : rest)
    discart1 <- verifyTypeExpr declaracoesFuncao blocosFuncoes declaracaoMain bloco;
    rest <- verifyTypeExpr declaracoesFuncao blocosFuncoes declaracaoMain xs;
    return (elem : rest);
verifyTypeExpr declaracoesFuncao blocosFuncoes declaracaoMain (elem@(Ret (Just exp)):xs) = do
            primeiroTipo <- getPriTipoComExpr exp declaracoesFuncao blocosFuncoes declaracaoMain;
            compartivel <- verifyTypesWithExpr exp primeiroTipo declaracoesFuncao blocosFuncoes declaracaoMain;
            if not compartivel then do
                traduzidoE <- translateExpr exp
                erro ("Tipos incompatíveis em: return " ++ traduzidoE ++ ";")
                rest <- verifyTypeExpr declaracoesFuncao blocosFuncoes declaracaoMain xs
                return (elem : rest)
            else do
                rest <- verifyTypeExpr declaracoesFuncao blocosFuncoes declaracaoMain xs
                return (elem : rest)
verifyTypeExpr declaracoesFuncao blocosFuncoes declaracaoMain (x:xs) = do {
     rest <- verifyTypeExpr declaracoesFuncao blocosFuncoes declaracaoMain xs;
     return (x : rest);
}

listCompatibleTypes' tipo [] = return True
listCompatibleTypes' tipo (x:xs) = do {
     if tipo==TDouble || tipo==TInt then
         if x==TString || x==TVoid then
             return False
         else
             listCompatibleTypes' tipo xs
     else if (tipo==TString) then
         if (x==TDouble || x==TInt || x==TVoid) then
             return False
         else
             listCompatibleTypes' tipo xs
     else
         if (x/=TVoid) then
             return False
         else
             listCompatibleTypes' tipo xs
 }

listCompatibleTypes [] = return True
listCompatibleTypes (tipo:rest) = do{
    elem <- listCompatibleTypes' tipo rest;
    rest <- listCompatibleTypes rest;
    return (elem && rest)
}

getListaTiposExprL (And e1 e2) declaracoesFuncao blocosFuncoes declaracaoMain = do {
                                                            transformedE1 <- getListaTiposExprL e1 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                            transformedE2 <- getListaTiposExprL e2 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                            return (transformedE1 ++ transformedE2)
                                                         }
getListaTiposExprL (Or e1 e2) declaracoesFuncao blocosFuncoes declaracaoMain = do {
                                                            transformedE1 <- getListaTiposExprL e1 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                            transformedE2 <- getListaTiposExprL e2 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                            return (transformedE1 ++ transformedE2)
                                                         }
getListaTiposExprL (Not e) declaracoesFuncao blocosFuncoes declaracaoMain =  do {
                                                            getListaTiposExprL e declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         }
getListaTiposExprL (Rel expR) declaracoesFuncao blocosFuncoes declaracaoMain = do {
    getListaTiposExprR expR declaracoesFuncao blocosFuncoes declaracaoMain
}

getListaTiposExprR e@(Req e1 e2) declaracoesFuncao blocosFuncoes declaracaoMain = do {
    res1 <- getTipoExp e1 declaracoesFuncao blocosFuncoes declaracaoMain;
    res2 <- getTipoExp e2 declaracoesFuncao blocosFuncoes declaracaoMain;
    return (res1 ++ res2)
 }
getListaTiposExprR e@(Rdif e1 e2) declaracoesFuncao blocosFuncoes declaracaoMain = do {
    res1 <- getTipoExp e1 declaracoesFuncao blocosFuncoes declaracaoMain;
    res2 <- getTipoExp e2 declaracoesFuncao blocosFuncoes declaracaoMain;
    return (res1 ++ res2)
 }
getListaTiposExprR e@(Rle e1 e2) declaracoesFuncao blocosFuncoes declaracaoMain = do {
    res1 <- getTipoExp e1 declaracoesFuncao blocosFuncoes declaracaoMain;
    res2 <- getTipoExp e2 declaracoesFuncao blocosFuncoes declaracaoMain;
    return (res1 ++ res2)
 }
getListaTiposExprR e@(Rge e1 e2) declaracoesFuncao blocosFuncoes declaracaoMain = do {
    res1 <- getTipoExp e1 declaracoesFuncao blocosFuncoes declaracaoMain;
    res2 <- getTipoExp e2 declaracoesFuncao blocosFuncoes declaracaoMain;
    return (res1 ++ res2)
 }
getListaTiposExprR e@(Rlt e1 e2) declaracoesFuncao blocosFuncoes declaracaoMain = do {
    res1 <- getTipoExp e1 declaracoesFuncao blocosFuncoes declaracaoMain;
    res2 <- getTipoExp e2 declaracoesFuncao blocosFuncoes declaracaoMain;
    return (res1 ++ res2)
 }
getListaTiposExprR e@(Rgt e1 e2) declaracoesFuncao blocosFuncoes declaracaoMain = do {
    res1 <- getTipoExp e1 declaracoesFuncao blocosFuncoes declaracaoMain;
    res2 <- getTipoExp e2 declaracoesFuncao blocosFuncoes declaracaoMain;
    return (res1 ++ res2)
 }

getTipoExp (Add e1 e2) declaracoesFuncao blocosFuncoes declaracaoMain = do {
                                                         transformedE1 <- getTipoExp e1 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         transformedE2 <- getTipoExp e2 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         return (transformedE1 ++ transformedE2);
                                                     }
getTipoExp (Sub e1 e2) declaracoesFuncao blocosFuncoes declaracaoMain = do {
                                                         transformedE1 <- getTipoExp e1 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         transformedE2 <- getTipoExp e2 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         return (transformedE1 ++ transformedE2);
                                                     }
getTipoExp (Mul e1 e2) declaracoesFuncao blocosFuncoes declaracaoMain = do {
                                                         transformedE1 <- getTipoExp e1 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         transformedE2 <- getTipoExp e2 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         return (transformedE1 ++ transformedE2);
                                                     }
getTipoExp (Div e1 e2) declaracoesFuncao blocosFuncoes declaracaoMain = do {
                                                         transformedE1 <- getTipoExp e1 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         transformedE2 <- getTipoExp e2 declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         return (transformedE1 ++ transformedE2);
                                                     }
getTipoExp (Neg e) declaracoesFuncao blocosFuncoes declaracaoMain = do {
                                                         getTipoExp e declaracoesFuncao blocosFuncoes declaracaoMain;
                                                     }
getTipoExp e@(Const (CDouble _)) _ _ _ = return [TDouble]
getTipoExp e@(Const (CInt _)) _ _ _ = return [TInt]
getTipoExp e@(Chamada nome params) declaracoesFuncao blocosFuncoes declaracaoMain = do {
    paramsCompativel <- verifyTypesParams params declaracoesFuncao blocosFuncoes declaracaoMain;
    paramsTraduzido <- translateExprComoParams params;
    unless paramsCompativel (erro ("Tipos incompatíveis nos parametros da função: "++nome++"( "++paramsTraduzido++" )"));
    quantParams <- getQuantidadeParams nome declaracoesFuncao;
    when (quantParams > length params) (erro ("Quantidade a menos de parametros na chamada da função: "++nome++"( "++paramsTraduzido++" )"));
    when (quantParams < length params) (erro ("Quantidade a mais de parametros na chamada da função: "++nome++"( "++paramsTraduzido++" )"));
    tipoFuncao <- getTipoFuncao nome declaracoesFuncao;
    case tipoFuncao of
        Just tipoFuncao -> return [TVoid];
        Nothing -> return [TVoid];

 }
getTipoExp e@(IdVar nome) declaracoesFuncao blocosFuncoes declaracaoMain = do {
    tipoVar <- getTipoVar nome declaracaoMain;
    return [tipoVar]
 }
getTipoExp e@(Lit _) _ _ _ = return [TString]
getTipoExp e@(IntDouble elem) _ _ _ = return [TDouble]
getTipoExp e _ _ _ = return [TInt]

verifyTypesWithExpr (Add e1 e2) tipo declaracoesFuncao blocosFuncoes declaracaoMain = do {
                                                         transformedE1 <- verifyTypesWithExpr e1 tipo declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         transformedE2 <- verifyTypesWithExpr e2 tipo declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         return (transformedE1 && transformedE2);
                                                     }
verifyTypesWithExpr (Sub e1 e2) tipo declaracoesFuncao blocosFuncoes declaracaoMain = do {
                                                         transformedE1 <- verifyTypesWithExpr e1 tipo declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         transformedE2 <- verifyTypesWithExpr e2 tipo declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         return (transformedE1 && transformedE2);
                                                     }
verifyTypesWithExpr (Mul e1 e2) tipo declaracoesFuncao blocosFuncoes declaracaoMain = do {
                                                         transformedE1 <- verifyTypesWithExpr e1 tipo declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         transformedE2 <- verifyTypesWithExpr e2 tipo declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         return (transformedE1 && transformedE2);
                                                     }
verifyTypesWithExpr (Div e1 e2) tipo declaracoesFuncao blocosFuncoes declaracaoMain = do {
                                                         transformedE1 <- verifyTypesWithExpr e1 tipo declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         transformedE2 <- verifyTypesWithExpr e2 tipo declaracoesFuncao blocosFuncoes declaracaoMain;
                                                         return (transformedE1 && transformedE2);
                                                     }
verifyTypesWithExpr (Neg e) tipo declaracoesFuncao blocosFuncoes declaracaoMain = do {
                                                         verifyTypesWithExpr e tipo declaracoesFuncao blocosFuncoes declaracaoMain;
                                                     }
verifyTypesWithExpr e@(Const (CDouble _)) tipo _ _ _ = return (tipo==TDouble || tipo==TInt)
verifyTypesWithExpr e@(Const (CInt _)) tipo _ _ _ = return (tipo==TDouble || tipo==TInt)
verifyTypesWithExpr e@(Chamada nome params) tipo declaracoesFuncao blocosFuncoes declaracaoMain = do {
    paramsCompativel <- verifyTypesParams params declaracoesFuncao blocosFuncoes declaracaoMain;
    paramsTraduzido <- translateExprComoParams params;
    unless paramsCompativel (erro ("Tipos incompatíveis nos parametros da função: "++nome++"( "++paramsTraduzido++" )"));
    quantParams <- getQuantidadeParams nome declaracoesFuncao;
    when (quantParams > length params) (erro ("Quantidade a menos de parametros na chamada da função: "++nome++"( "++paramsTraduzido++" )"));
    when (quantParams < length params) (erro ("Quantidade a mais de parametros na chamada da função: "++nome++"( "++paramsTraduzido++" )"));
    tipoFuncao <- getTipoFuncao nome declaracoesFuncao;
    case tipoFuncao of
        Just tipoFuncao -> do {
         if tipoFuncao==TDouble || tipoFuncao==TInt then
             return (tipo==TDouble || tipo==TInt)
         else
             return (tipo==TString)
         }
        Nothing -> return False;

 }
verifyTypesWithExpr e@(IdVar nome) tipo declaracoesFuncao blocosFuncoes declaracaoMain = do {
     tipoVar <- getTipoVar nome declaracaoMain;
     if tipoVar==TDouble || tipoVar==TInt then
         return (tipo==TDouble || tipo==TInt)
     else
         return (tipo==TString)
 }
verifyTypesWithExpr e@(Lit _) tipo _ _ _ = return (tipo==TString)
verifyTypesWithExpr e@(IntDouble elem) tipo _ _ _ = do {
     return (tipo==TDouble || tipo==TInt)
 }
verifyTypesWithExpr e tipo _ _ _ = return (tipo==TDouble || tipo==TInt)

verifyTypesParams [] _ _ _ = return True
verifyTypesParams (x:xs) declaracoesFuncao blocosFuncoes declaracao = do {
     tipo <- getPriTipoComExpr x declaracoesFuncao blocosFuncoes declaracao;
     resu <- verifyTypesWithExpr x tipo declaracoesFuncao blocosFuncoes declaracao;
     rest <- verifyTypesParams xs declaracoesFuncao blocosFuncoes declaracao;
     return (rest && resu)
 }

getPriTipoComExpr (Add e1 e2) declaracoesFuncao blocosFuncoes declaracaoMain = getPriTipoComExpr e1 declaracoesFuncao blocosFuncoes declaracaoMain;
getPriTipoComExpr (Sub e1 e2) declaracoesFuncao blocosFuncoes declaracaoMain = getPriTipoComExpr e1 declaracoesFuncao blocosFuncoes declaracaoMain;
getPriTipoComExpr (Mul e1 e2) declaracoesFuncao blocosFuncoes declaracaoMain = getPriTipoComExpr e1 declaracoesFuncao blocosFuncoes declaracaoMain;
getPriTipoComExpr (Div e1 e2) declaracoesFuncao blocosFuncoes declaracaoMain = getPriTipoComExpr e1 declaracoesFuncao blocosFuncoes declaracaoMain;
getPriTipoComExpr (Neg e) declaracoesFuncao blocosFuncoes declaracaoMain = getPriTipoComExpr e declaracoesFuncao blocosFuncoes declaracaoMain
getPriTipoComExpr e@(Const (CDouble _)) _ _ _ = return TDouble
getPriTipoComExpr e@(Const (CInt _)) _ _ _ = return TInt
getPriTipoComExpr e@(Chamada nome _) declaracoesFuncao blocosFuncoes declaracaoMain = do { -- ficar de olho
     tipo <- getTipoFuncao nome declaracoesFuncao;
     case tipo of
         Just tipo -> return tipo;
 }
getPriTipoComExpr e@(IdVar nome) declaracoesFuncao blocosFuncoes declaracaoMain = getTipoVar nome declaracaoMain
getPriTipoComExpr e@(Lit _) _ _ _ = return TString
getPriTipoComExpr e _ _ _ = return TInt

-- 1 parte retorno
normalizeReturnType [] [] _ _ = return []
normalizeReturnType ((nome :->: (_, tipo)):ys) ((_,declaracoes,bloco):xs) declaracoesFuncao blocosFuncoes = do {
     transformedBloco <- normalizeReturnType' declaracoesFuncao blocosFuncoes declaracoes bloco tipo;
     rest <- normalizeReturnType ys xs declaracoesFuncao blocosFuncoes;
     return ((nome, declaracoes, transformedBloco) : rest)
 }

normalizeReturnType' _ _ _ [] _ = return []
normalizeReturnType' declaracoesFuncao blocosFuncoes listaVars (elem@(Ret (Just e)):xs) tipo = do {
    if tipo==TDouble then do
        transformedE <- transformDouble e listaVars declaracoesFuncao;
        normalizeReturnType' declaracoesFuncao blocosFuncoes listaVars xs tipo >>= \rest -> return (Ret (Just transformedE) : rest)
    else if tipo==TInt then do
        transformedE <- transformInt e listaVars declaracoesFuncao e;
        normalizeReturnType' declaracoesFuncao blocosFuncoes listaVars xs tipo >>= \rest -> return (Ret (Just transformedE) : rest)
    else do
        traduzidoE <- translateExpr e
        erro ("Tipo de retorno incompativel em: "++traduzidoE);
        normalizeReturnType' declaracoesFuncao blocosFuncoes listaVars xs tipo >>= \rest -> return (elem : rest);
}
normalizeReturnType' declaracoesFuncao blocosFuncoes listaVars (elem:xs) tipo = do
    normalizeReturnType' declaracoesFuncao blocosFuncoes listaVars xs tipo >>= \rest -> return (elem : rest);


verifyExprDouble (Add e1 e2) listaVars listaFuncoes = do {
                                                         transformedE1 <- verifyExprDouble e1 listaVars listaFuncoes;
                                                         transformedE2 <- verifyExprDouble e2 listaVars listaFuncoes;
                                                         return (transformedE1 || transformedE2);
                                                     }
verifyExprDouble (Sub e1 e2) listaVars listaFuncoes = do {
                                                         transformedE1 <- verifyExprDouble e1 listaVars listaFuncoes;
                                                         transformedE2 <- verifyExprDouble e2 listaVars listaFuncoes;
                                                         return (transformedE1 || transformedE2);
                                                     }
verifyExprDouble (Mul e1 e2) listaVars listaFuncoes = do {
                                                         transformedE1 <- verifyExprDouble e1 listaVars listaFuncoes;
                                                         transformedE2 <- verifyExprDouble e2 listaVars listaFuncoes;
                                                         return (transformedE1 || transformedE2);
                                                     }
verifyExprDouble (Div e1 e2) listaVars listaFuncoes = do {
                                                         transformedE1 <- verifyExprDouble e1 listaVars listaFuncoes;
                                                         transformedE2 <- verifyExprDouble e2 listaVars listaFuncoes;
                                                         return (transformedE1 || transformedE2);
                                                     }
verifyExprDouble (Neg e) listaVars listaFuncoes = do {
                                                         verifyExprDouble e listaVars listaFuncoes;
                                                     }
verifyExprDouble e@(Const (CDouble _)) _ _ = return True
verifyExprDouble e@(Const (CInt _)) _ _ = return False
verifyExprDouble e@(Chamada nome _) listaVars listaFuncoes = return (verifyIfDeclaracaoFuncaoDouble nome listaFuncoes)
verifyExprDouble e@(IdVar nome) listaVars listaFuncoes = return (verifyIfDeclaracaoDouble nome listaVars)
verifyExprDouble e@(Lit _) _ _ = return False

transformExprRDouble (Req e1@(Lit _) e2@(Lit _)) listaVars listaFuncoes = return (Req e1 e2)
transformExprRDouble (Req e1@(Lit _) e2) listaVars listaFuncoes = do
                                                        traduzidoE1 <- translateExpr e1
                                                        traduzidoE2 <- translateExpr e2
                                                        erro ("Expressão relacional incompativel em: " ++ traduzidoE1 ++ " == " ++ traduzidoE2)
                                                        return (Req e1 e2)
transformExprRDouble (Req e1 e2@(Lit _)) listaVars listaFuncoes = do
                                                        traduzidoE1 <- translateExpr e1
                                                        traduzidoE2 <- translateExpr e2
                                                        erro ("Expressão relacional incompativel em: " ++ traduzidoE1 ++ " == " ++ traduzidoE2)
                                                        return (Req e1 e2)
transformExprRDouble (Req e1 e2) listaVars listaFuncoes = do {
                                                        transformedE1 <- transformDouble e1 listaVars listaFuncoes;
                                                        transformedE2 <- transformDouble e2 listaVars listaFuncoes;
                                                        return (Req transformedE1 transformedE2);
                                                    }
transformExprRDouble (Rdif e1@(Lit _) e2@(Lit _)) listaVars listaFuncoes = return (Rdif e1 e2)
transformExprRDouble (Rdif e1@(Lit _) e2) listaVars listaFuncoes = do
                                                        traduzidoE1 <- translateExpr e1
                                                        traduzidoE2 <- translateExpr e2
                                                        erro ("Expressão relacional incompativel em: " ++ traduzidoE1 ++ " /= " ++ traduzidoE2)
                                                        return (Rdif e1 e2)
transformExprRDouble (Rdif e1 e2@(Lit _)) listaVars listaFuncoes = do
                                                        traduzidoE1 <- translateExpr e1
                                                        traduzidoE2 <- translateExpr e2
                                                        erro ("Expressão relacional incompativel em: " ++ traduzidoE1 ++ " /= " ++ traduzidoE2)
                                                        return (Rdif e1 e2)
transformExprRDouble (Rdif e1 e2) listaVars listaFuncoes = do {
                                                        transformedE1 <- transformDouble e1 listaVars listaFuncoes;
                                                        transformedE2 <- transformDouble e2 listaVars listaFuncoes;
                                                        return (Rdif transformedE1 transformedE2);
                                                    }
transformExprRDouble (Rle e1@(Lit _) e2@(Lit _)) listaVars listaFuncoes = return (Rle e1 e2)
transformExprRDouble (Rle e1@(Lit _) e2) listaVars listaFuncoes = do
                                                        traduzidoE1 <- translateExpr e1
                                                        traduzidoE2 <- translateExpr e2
                                                        erro ("Expressão relacional incompativel em: " ++ traduzidoE1 ++ " <= " ++ traduzidoE2)
                                                        return (Rle e1 e2)
transformExprRDouble (Rle e1 e2@(Lit _)) listaVars listaFuncoes = do
                                                        traduzidoE1 <- translateExpr e1
                                                        traduzidoE2 <- translateExpr e2
                                                        erro ("Expressão relacional incompativel em: " ++ traduzidoE1 ++ " <= " ++ traduzidoE2)
                                                        return (Rle e1 e2)
transformExprRDouble (Rle e1 e2) listaVars listaFuncoes = do {
                                                        transformedE1 <- transformDouble e1 listaVars listaFuncoes;
                                                        transformedE2 <- transformDouble e2 listaVars listaFuncoes;
                                                        return (Rle transformedE1 transformedE2);
                                                    }
transformExprRDouble (Rge e1@(Lit _) e2@(Lit _)) listaVars listaFuncoes = return (Rge e1 e2)
transformExprRDouble (Rge e1@(Lit _) e2) listaVars listaFuncoes = do
                                                        traduzidoE1 <- translateExpr e1
                                                        traduzidoE2 <- translateExpr e2
                                                        erro ("Expressão relacional incompativel em: " ++ traduzidoE1 ++ " >= " ++ traduzidoE2)
                                                        return (Rge e1 e2)
transformExprRDouble (Rge e1 e2@(Lit _)) listaVars listaFuncoes = do
                                                        traduzidoE1 <- translateExpr e1
                                                        traduzidoE2 <- translateExpr e2
                                                        erro ("Expressão relacional incompativel em: " ++ traduzidoE1 ++ " >= " ++ traduzidoE2)
                                                        return (Rge e1 e2)
transformExprRDouble (Rge e1 e2) listaVars listaFuncoes =  do {
                                                        transformedE1 <- transformDouble e1 listaVars listaFuncoes;
                                                        transformedE2 <- transformDouble e2 listaVars listaFuncoes;
                                                        return (Rge transformedE1 transformedE2);
                                                    }
transformExprRDouble (Rlt e1@(Lit _) e2@(Lit _)) listaVars listaFuncoes = return (Rlt e1 e2)
transformExprRDouble (Rlt e1@(Lit _) e2) listaVars listaFuncoes = do
                                                        traduzidoE1 <- translateExpr e1
                                                        traduzidoE2 <- translateExpr e2
                                                        erro ("Expressão relacional incompativel em: " ++ traduzidoE1 ++ " < " ++ traduzidoE2)
                                                        return (Rlt e1 e2)
transformExprRDouble (Rlt e1 e2@(Lit _)) listaVars listaFuncoes = do
                                                        traduzidoE1 <- translateExpr e1
                                                        traduzidoE2 <- translateExpr e2
                                                        erro ("Expressão relacional incompativel em: " ++ traduzidoE1 ++ " < " ++ traduzidoE2)
                                                        return (Rlt e1 e2)
transformExprRDouble (Rlt e1 e2) listaVars listaFuncoes =  do {
                                                        transformedE1 <- transformDouble e1 listaVars listaFuncoes;
                                                        transformedE2 <- transformDouble e2 listaVars listaFuncoes;
                                                        return (Rlt transformedE1 transformedE2);
                                                    }
transformExprRDouble (Rgt e1@(Lit _) e2@(Lit _)) listaVars listaFuncoes = return (Rgt e1 e2)
transformExprRDouble (Rgt e1@(Lit _) e2) listaVars listaFuncoes = do
                                                        traduzidoE1 <- translateExpr e1
                                                        traduzidoE2 <- translateExpr e2
                                                        erro ("Expressão relacional incompativel em: " ++ traduzidoE1 ++ " > " ++ traduzidoE2)
                                                        return (Rgt e1 e2)
transformExprRDouble (Rgt e1 e2@(Lit _)) listaVars listaFuncoes = do
                                                        traduzidoE1 <- translateExpr e1
                                                        traduzidoE2 <- translateExpr e2
                                                        erro ("Expressão relacional incompativel em: " ++ traduzidoE1 ++ " > " ++ traduzidoE2)
                                                        return (Rgt e1 e2)
transformExprRDouble (Rgt e1 e2) listaVars listaFuncoes =  do {
                                                        transformedE1 <- transformDouble e1 listaVars listaFuncoes;
                                                        transformedE2 <- transformDouble e2 listaVars listaFuncoes;
                                                        return (Rgt transformedE1 transformedE2);
                                                    }


verifyExprRDouble e@(Req e1 e2) listaVars listaFuncoes = do {
    res1 <- verifyExprDouble e1 listaVars listaFuncoes;
    res2 <- verifyExprDouble e2 listaVars listaFuncoes;
    if res1 || res2 then
        transformExprRDouble e listaVars listaFuncoes
    else
        return e
 }
verifyExprRDouble e@(Rdif e1 e2) listaVars listaFuncoes = do {
    res1 <- verifyExprDouble e1 listaVars listaFuncoes;
    res2 <- verifyExprDouble e2 listaVars listaFuncoes;
    if res1 || res2 then
        transformExprRDouble e listaVars listaFuncoes
    else
        return e
 }
verifyExprRDouble e@(Rle e1 e2) listaVars listaFuncoes = do {
    res1 <- verifyExprDouble e1 listaVars listaFuncoes;
    res2 <- verifyExprDouble e2 listaVars listaFuncoes;
    if res1 || res2 then
        transformExprRDouble e listaVars listaFuncoes
    else
        return e
 }
verifyExprRDouble e@(Rge e1 e2) listaVars listaFuncoes = do {
    res1 <- verifyExprDouble e1 listaVars listaFuncoes;
    res2 <- verifyExprDouble e2 listaVars listaFuncoes;
    if res1 || res2 then
        transformExprRDouble e listaVars listaFuncoes
    else
        return e
 }
verifyExprRDouble e@(Rlt e1 e2) listaVars listaFuncoes = do {
    res1 <- verifyExprDouble e1 listaVars listaFuncoes;
    res2 <- verifyExprDouble e2 listaVars listaFuncoes;
    if res1 || res2 then
        transformExprRDouble e listaVars listaFuncoes
    else
        return e
 }
verifyExprRDouble e@(Rgt e1 e2) listaVars listaFuncoes = do {
    res1 <- verifyExprDouble e1 listaVars listaFuncoes;
    res2 <- verifyExprDouble e2 listaVars listaFuncoes;
    if res1 || res2 then
        transformExprRDouble e listaVars listaFuncoes
    else
        return e
 }

verifyExprLDouble (And e1 e2) listaVars listaFuncoes = do {
                                                            transformedE1 <- verifyExprLDouble e1 listaVars listaFuncoes;
                                                            transformedE2 <- verifyExprLDouble e2 listaVars listaFuncoes;
                                                            return (And transformedE1 transformedE2)
                                                         }
verifyExprLDouble (Or e1 e2) listaVars listaFuncoes = do {
                                                            transformedE1 <- verifyExprLDouble e1 listaVars listaFuncoes;
                                                            transformedE2 <- verifyExprLDouble e2 listaVars listaFuncoes;
                                                            return (Or transformedE1 transformedE2)
                                                         }
verifyExprLDouble (Not e) listaVars listaFuncoes =  do {
                                                            transformedE <- verifyExprLDouble e listaVars listaFuncoes;
                                                            return (Not transformedE)
                                                         }
verifyExprLDouble (Rel expR) listaVars listaFuncoes = do {
    transformedExpR <- verifyExprRDouble expR listaVars listaFuncoes;
    return (Rel transformedExpR)
 }

normDoubleR _ _ _ [] = return []
normDoubleR declaracoesFuncao blocosFuncoes declaracaoMain (elem@(If exprL bloco blocoElse):xs) = do {
        transformedExprL <- verifyExprLDouble exprL declaracaoMain declaracoesFuncao;
        transformedBloco <- normDouble declaracoesFuncao blocosFuncoes declaracaoMain bloco;
        transformedBlocoElse <- normDouble declaracoesFuncao blocosFuncoes declaracaoMain blocoElse;
        rest <- normDoubleR declaracoesFuncao blocosFuncoes declaracaoMain xs;
        return (If transformedExprL transformedBloco transformedBlocoElse : rest)
    }
normDoubleR declaracoesFuncao blocosFuncoes declaracaoMain (elem@(While exprL bloco):xs) = do {
        transformedExprL <- verifyExprLDouble exprL declaracaoMain declaracoesFuncao;
        transformedBloco <- normDouble declaracoesFuncao blocosFuncoes declaracaoMain bloco;
        rest <- normDoubleR declaracoesFuncao blocosFuncoes declaracaoMain xs;
        return (While transformedExprL transformedBloco : rest)
    }
normDoubleR declaracoesFuncao blocosFuncoes declaracaoMain (elem:xs) = do {
         rest <- normDoubleR declaracoesFuncao blocosFuncoes declaracaoMain xs;
         return (elem : rest)
     }


verifyIfDeclaracaoDouble _ [] = False
verifyIfDeclaracaoDouble var ((nome :#: TDouble):xs) = (var == nome) || verifyIfDeclaracaoDouble var xs
verifyIfDeclaracaoDouble var (x:xs) = verifyIfDeclaracaoDouble var xs

verifyIfDeclaracaoFuncaoDouble _ [] = False
verifyIfDeclaracaoFuncaoDouble var (nome :->: (_,TDouble):xs) = (var == nome) || verifyIfDeclaracaoFuncaoDouble var xs
verifyIfDeclaracaoFuncaoDouble var (x:xs) = verifyIfDeclaracaoFuncaoDouble var xs

verificaSeDeclaracaoFuncaoInt _ [] = False
verificaSeDeclaracaoFuncaoInt var (nome :->: (_,TInt):xs) = (var == nome) || verificaSeDeclaracaoFuncaoInt var xs
verificaSeDeclaracaoFuncaoInt var (x:xs) = verificaSeDeclaracaoFuncaoInt var xs

translateExprL (And e1 e2) = do {
                                                            transformedE1 <- translateExprL e1;
                                                            transformedE2 <- translateExprL e2;
                                                            return (transformedE1 ++ "&" ++ transformedE2)
                                                         }
translateExprL (Or e1 e2) = do {
                                                            transformedE1 <- translateExprL e1;
                                                            transformedE2 <- translateExprL e2;
                                                            return (transformedE1 ++ "|" ++ transformedE2)
                                                         }
translateExprL (Not e) =  do {
                                                            translateExprL e;
                                                         }
translateExprL (Rel expR) = do {
    translateExprR expR
}

translateExprR e@(Req e1 e2) = do {
    res1 <- translateExpr e1;
    res2 <- translateExpr e2;
    return (res1 ++ "==" ++ res2)
 }
translateExprR e@(Rdif e1 e2) = do {
    res1 <- translateExpr e1;
    res2 <- translateExpr e2;
    return (res1 ++ "/=" ++ res2)
 }
translateExprR e@(Rle e1 e2) = do {
    res1 <- translateExpr e1;
    res2 <- translateExpr e2;
    return (res1 ++ "<=" ++ res2)
 }
translateExprR e@(Rge e1 e2) = do {
    res1 <- translateExpr e1;
    res2 <- translateExpr e2;
    return (res1 ++ ">=" ++ res2)
 }
translateExprR e@(Rlt e1 e2) = do {
    res1 <- translateExpr e1;
    res2 <- translateExpr e2;
    return (res1 ++ "<" ++ res2)
 }
translateExprR e@(Rgt e1 e2) = do {
    res1 <- translateExpr e1;
    res2 <- translateExpr e2;
    return (res1 ++ ">" ++ res2)
 }

translateExpr (Add e1 e2) = do {
                            transformedE1 <- translateExpr e1;
                            transformedE2 <- translateExpr e2;
                            return (transformedE1++" + "++transformedE2);
                        }
translateExpr (Sub e1 e2) = do {
                            transformedE1 <- translateExpr e1;
                            transformedE2 <- translateExpr e2;
                            return (transformedE1++" - "++transformedE2);
                        }
translateExpr (Mul e1 e2) = do {
                            transformedE1 <- translateExpr e1;
                            transformedE2 <- translateExpr e2;
                            return (transformedE1++" * "++transformedE2);
                        }
translateExpr (Div e1 e2) = do {
                            transformedE1 <- translateExpr e1;
                            transformedE2 <- translateExpr e2;
                            return (transformedE1++" / "++transformedE2);
                        }
translateExpr (Neg e) = do {
                        return ("- "++show e);
                    }
translateExpr e@(Const (CDouble v)) = return (show v)
translateExpr e@(Const (CInt v)) = return (show v)
translateExpr e@(Chamada nome v) = do {
     translatedV <- mapM translateExpr v;
     return (nome++"("++unwords translatedV++")")
 }
translateExpr e@(IdVar nome) = return nome;
translateExpr e@(Lit v) = return ("'"++v++"'");
translateExpr e@(IntDouble e1) = do { translateExpr e1; }
translateExpr e@(DoubleInt e1) = do { translateExpr e1; }

translateExprComoParams [] = return ""
translateExprComoParams (x:[]) = do {
     translateExpr x;
 }
translateExprComoParams (x:xs) = do {
     traduzidoE <- translateExpr x;
     rest <- translateExprComoParams xs;
     return (traduzidoE ++ ", " ++ rest)
 }

getTipoVar _ [] = return TVoid
getTipoVar nome ((nomeVar :#: tipo):restoDec) = if nomeVar==nome then return tipo else getTipoVar nome restoDec;


getTipoParams _ [] = []
getTipoParams nome ((nomeFuncao :->: (listaVars,_)):funcoes) = if nome==nomeFuncao then listaVars else getTipoParams nome funcoes

getTipoFuncao nome [] = return Nothing
getTipoFuncao nome ((nomeFuncao :->: (_,tipo)):funcoes) = if nome==nomeFuncao then return (Just tipo) else getTipoFuncao nome funcoes

getQuantidadeParams _ [] = return 0
getQuantidadeParams nome ((nomeFuncao :->: (listaVars,_)):funcoes) = if nome==nomeFuncao then return (length listaVars) else getQuantidadeParams nome funcoes