{-# LANGUAGE LambdaCase #-}
module EvalExpr where

import Data.Bits

import Syntax


equalLength :: [a] -> [b] -> Bool
equalLength a b = length a == length b

arrEqual :: Eq a => [a] -> [a] -> Bool
arrEqual [] [] = True
arrEqual [] b  = False
arrEqual a  [] = False
arrEqual (hd1:tl1) (hd2:tl2) = hd1 == hd2 && arrEqual tl1 tl2


arrSize :: [Integer] -> Integer
arrSize = foldl (*) 1


sameSize :: [Integer] -> [Expr] -> Bool
sameSize sz es = arrSize sz == fromIntegral (length es)


arrayIndex :: [Expr] -> Either String Int
arrayIndex idxs =
  case f idxs of
    Right val -> Right $ val - 1
    err       -> err
  where
    f :: [Expr] -> Either String Int
    f = \case
      [] -> Right 1
      (hd:tl) ->
        let hd' = evalConstantIntExpr hd in
          case hd' of
            Right hd'' ->
              let rest = f tl in
                case rest of
                  Right val ->
                    Right $ (hd'' + 1) * val
                  err -> err
            err -> err



-- C++ like boolean conversion
intToBool :: Int -> Bool
intToBool val = if (val /= 0) then True else False

evalConstantIntExpr :: Expr -> Either String Int
evalConstantIntExpr = \case
  ConstE (IntegerV val) -> Right $ fromInteger val
  ConstE (BooleanV val) -> Right $ fromEnum val

  Arith op e1 e2 ->
    let v1 = evalConstantIntExpr e1 in
      let v2 = evalConstantIntExpr e2 in
        case (v1, v2) of
          (Right v1', Right v2') ->
            Right $ case op of
              Plus   -> v1' + v2'
              Sub    -> v1' - v2'
              Xor    -> v1' `xor` v2'
              Mul    -> v1' * v2'
              Div    -> v1' `div` v2'
              Modulo -> v1' `mod` v2'
              BAnd   -> v1' .&. v2'
              BOr    -> v1' .|. v2'
              And    -> fromEnum $ (intToBool v1') && (intToBool v2')
              Or     -> fromEnum $ (intToBool v1') || (intToBool v2')
              Great  -> fromEnum $ (intToBool v1') > (intToBool v2')
              GreatEq-> fromEnum $ (intToBool v1') >= (intToBool v2')
              Less   -> fromEnum $ (intToBool v1') < (intToBool v2')
              LessEq -> fromEnum $ (intToBool v1') <= (intToBool v2')
              NotEq  -> fromEnum $ v1' /= v2'
              Eq     -> fromEnum $ v1' == v2'
          (err, Right _) -> err
          (Right _, err) -> err
          (err, _)       -> err

  Not e ->
    let v = evalConstantIntExpr e in
      case v of
        Right v' ->
          Right $ fromEnum $ not $ intToBool v'
        err -> err

  _ -> Left "Expression not a constant expression"


evalConstantBooleanExpr :: Expr -> Bool
evalConstantBooleanExpr = \case
  ConstE (IntegerV val) -> intToBool $ fromInteger val
  ConstE (BooleanV val) -> val

  Arith op e1 e2 ->
    let v1 = evalConstantBooleanExpr e1 in
      let v2 = evalConstantBooleanExpr e2 in
        case op of
          Plus   -> intToBool $ (fromEnum v1) + (fromEnum v2)
          Sub    -> intToBool $ (fromEnum v1) - (fromEnum v2)
          Xor    -> intToBool $ (fromEnum v1) `xor` (fromEnum v2)
          Mul    -> intToBool $ (fromEnum v1) * (fromEnum v2)
          Div    -> intToBool $ (fromEnum v1) `div` (fromEnum v2)
          Modulo -> intToBool $ (fromEnum v1) `mod` (fromEnum v2)
          BAnd   -> intToBool $ (fromEnum v1) .&. (fromEnum v2)
          BOr    -> intToBool $ (fromEnum v1) .|. (fromEnum v2)
          And    -> v1 && v2
          Or     -> v1 || v2
          Great  -> v1 > v2
          GreatEq-> v1 >= v2
          Less   -> v1 < v2
          LessEq -> v1 <= v2
          NotEq  -> v1 /= v2
          Eq     -> v1 == v2
