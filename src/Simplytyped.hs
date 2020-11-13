module Simplytyped
  ( conversion
  ,    -- conversion a terminos localmente sin nombre
    eval
  ,          -- evaluador
    infer
  ,         -- inferidor de tipos
    quote          -- valores -> terminos
  )
where

import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( (>>=) )
import           Text.PrettyPrint.HughesPJ      ( render )
import           PrettyPrinter
import           Common

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' []

-- toma una lista de variables ligadas y el lambda termino y devuelve el termino
conversion' :: [String] -> LamTerm -> Term
conversion' b (LVar n    ) = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (LApp t u  ) = conversion' b t :@: conversion' b u
conversion' b (LAbs n t u) = Lam t (conversion' (n : b) u)
conversion' b (LLet s t u) = Let (conversion' b t) (conversion' (s:b) u)
conversion' b (LAs term tipo) = As (conversion' b term) tipo
conversion' b LUnit           = Unit
conversion' b (LFst term) = Fst (conversion' b term)
conversion' b (LSnd term) = Snd (conversion' b term)
conversion' b (LPair term1 term2) = Pair (conversion' b term1) (conversion' b term2)
conversion' b LZero = Zero
conversion' b (LSuc term) = Suc (conversion' b term)  
conversion' b (LRec t1 t2 t3) = Rec (conversion' b t1) (conversion' b t2) (conversion' b t3)   

-----------------------
--- sub
-----------------------
--El  primer  argumento  indica  la  cantidad  de  abstracciones  bajo  la  cual  se  realizara  la  
--substitucion,  el  segundo argumento es el termino a substituir, 
--y el tercero el termino donde se efectuara la substitucion.

sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n   )           = Free n
sub i t (u   :@: v)           = sub i t u :@: sub i t v
sub i t (Lam t'  u)           = Lam t' (sub (i + 1) t u)
sub i t (Let t1 t2)           = Let t1 (sub (i + 1) t t2)
sub i t (As term tipo)        = As (sub i t term) tipo
sub i t Unit                  = Unit
sub i t (Fst term)            = Fst (sub i t term)
sub i t (Snd term)            = Snd (sub i t term)
sub i t (Pair t1 t2)          = Pair (sub i t t1) (sub i t t2)
sub i t Zero                  = Zero
sub i t (Suc t1)              = Suc (sub i t t1)
sub i t (Rec t1 t2 t3)        = Rec (sub i t t1) (sub i t t2) (sub i t t3)

 -- evaluador de términos
 -- Toma el enterno de variables libres, esto es [(Name, (value, type))] lo cual representa cada variable junto su valor y su tipo
eval :: NameEnv Value Type -> Term -> Value
eval _ (Bound _             ) = error "variable ligada inesperada en eval"
eval e (Free  n             ) = fst $ fromJust $ lookup n e
eval _ (Lam      t   u      ) = VLam t u
eval e (Lam _ u  :@: Lam s v) = eval e (sub 0 (Lam s v) u)
eval e (Lam t u1 :@: u2) = let v2 = eval e u2 in eval e (sub 0 (quote v2) u1) 
eval e (u        :@: v      ) = case eval e u of
  VLam t u' -> eval e (Lam t u' :@: v)
  _         -> error "Error de tipo en run-time, verificar type checker"
eval e (Let t1 t2)            =  eval e (sub 0 t1 t2)
eval e (As term tipo)         =  eval e term
eval e Unit                   =  VUnit
eval e (Fst (Pair a _))       =  eval e a
eval e (Snd (Pair _ b))       =  eval e b
eval e (Fst _)                =  error "Error de tipo en run-time, verificar type checker"
eval e (Snd _)                =  error "Error de tipo en run-time, verificar type checker"
eval e (Pair t1 t2)           =  VPair (eval e t1) (eval e t2)
eval e Zero                   =  VNum NZero
eval e (Suc term)             =  case eval e term of
                                    VNum n -> VNum (NSuc n)
                                    _      -> error "sucesor de un termino no nat"
eval e (Rec t1 t2 t3)           =  case eval e t3 of
                                    VNum NZero -> eval e t1
                                    VNum (NSuc n)  -> let nro = quote(VNum n)
                                                      in eval e (t2 :@: (Rec t1 t2 nro) :@: nro)


-----------------------
--- quoting
-----------------------

quote :: Value -> Term
quote (VLam t f)      = Lam t f
quote (VUnit)         = Unit
quote (VPair v1 v2)   = Pair (quote v1) (quote v2)
quote (VNum (NZero))  = Zero
quote (VNum (NSuc n)) = Suc (quote (VNum n))

----------------------
--- type checker
-----------------------

-- type checker
--Infiere el tipo
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []


-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=)
  :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

--Toma un contexto (una lista de tipos), el entorno de variables libres y el termino
infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free  n) = case lookup n e of
  Nothing     -> notfoundError n
  Just (_, t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt -> infer' c e u >>= \tu ->
  case tt of
    FunT t1 t2 -> if (tu == t1) then ret t2 else matchError t1 tu
    _          -> notfunError tt
infer' c e (Lam t u) = infer' (t : c) e u >>= \tu -> ret $ FunT t tu

-- No lo consideramos variable libre, por ende, lo agregamos al contexto y NO al ambiente (environment)
infer' c e (Let t1 t2) = infer' c e t1 >>= \tt1 -> infer' (tt1:c) e t2 

infer' c e (As term tipo) = infer' c e term >>= \tt -> if tt == tipo then ret tipo else matchError tipo tt
infer' c e Unit = ret UnitT
infer' c e (Pair t1 t2) = infer' c e t1 >>= \tt1 -> infer' (tt1:c) e t2 >>= \tt2 -> ret (PairT tt1 tt2)
infer' c e (Fst t) = infer' c e t >>= \tt ->  case tt of
                                                PairT t1 t2 -> ret t1
                                                _           -> err "Se aplica fst a algo que no es una tupla"
infer' c e (Snd t) = infer' c e t >>= \tt ->  case tt of
                                                PairT t1 t2 -> ret t2
                                                _           -> err "Se aplica snd a algo que no es una tupla"

infer' c e (Zero)  = ret NatT
infer' c e (Suc t) = infer' c e t >>= \tt -> case tt of
                                              NatT -> ret NatT
                                              _           -> matchError NatT tt

infer' c e (Rec t1 t2 t3) = infer' c e t1 >>= \tt1 -> infer' c e t2 >>= \tt2 -> infer' c e t3 >>= \tt3 ->
                                      case tt2 of
                                        (FunT x (FunT NatT y)) -> if x == tt1 && y == tt1
                                                                    then case tt3 of
                                                                          NatT -> ret tt1
                                                                          _    -> matchError NatT tt3 
                                                                    else matchError (FunT tt1 (FunT NatT tt1)) tt2
                                                                    
                                        _ -> matchError (FunT tt1 (FunT NatT tt1)) tt2 
