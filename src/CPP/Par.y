-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module CPP.Par where
import CPP.Abs as Abs
import CPP.Lex

}

%name pProgram Program
%name pDef Def
%name pListDef ListDef
%name pArg Arg
%name pListArg ListArg
%name pStm Stm
%name pListStm ListStm
%name pExp15 Exp15
%name pExp14 Exp14
%name pExp13 Exp13
%name pExp12 Exp12
%name pExp11 Exp11
%name pExp9 Exp9
%name pExp8 Exp8
%name pExp4 Exp4
%name pExp3 Exp3
%name pExp2 Exp2
%name pExp1 Exp1
%name pExp Exp
%name pExp5 Exp5
%name pExp6 Exp6
%name pExp7 Exp7
%name pExp10 Exp10
%name pListExp ListExp
%name pType Type
%name pListId ListId
-- no lexer declaration
%monad { Either String } { (>>=) } { return }
%tokentype {Token}
%token
  '!=' { PT _ (TS _ 1) }
  '&&' { PT _ (TS _ 2) }
  '(' { PT _ (TS _ 3) }
  ')' { PT _ (TS _ 4) }
  '*' { PT _ (TS _ 5) }
  '+' { PT _ (TS _ 6) }
  '++' { PT _ (TS _ 7) }
  ',' { PT _ (TS _ 8) }
  '-' { PT _ (TS _ 9) }
  '--' { PT _ (TS _ 10) }
  '/' { PT _ (TS _ 11) }
  ';' { PT _ (TS _ 12) }
  '<' { PT _ (TS _ 13) }
  '<=' { PT _ (TS _ 14) }
  '=' { PT _ (TS _ 15) }
  '==' { PT _ (TS _ 16) }
  '>' { PT _ (TS _ 17) }
  '>=' { PT _ (TS _ 18) }
  'bool' { PT _ (TS _ 19) }
  'double' { PT _ (TS _ 20) }
  'else' { PT _ (TS _ 21) }
  'false' { PT _ (TS _ 22) }
  'if' { PT _ (TS _ 23) }
  'int' { PT _ (TS _ 24) }
  'return' { PT _ (TS _ 25) }
  'string' { PT _ (TS _ 26) }
  'true' { PT _ (TS _ 27) }
  'void' { PT _ (TS _ 28) }
  'while' { PT _ (TS _ 29) }
  '{' { PT _ (TS _ 30) }
  '||' { PT _ (TS _ 31) }
  '}' { PT _ (TS _ 32) }
  L_integ  { PT _ (TI $$) }
  L_doubl  { PT _ (TD $$) }
  L_quoted { PT _ (TL $$) }
  L_Id { PT _ (T_Id $$) }

%%

Integer :: { Integer }
Integer  : L_integ  { (read ( $1)) :: Integer }

Double  :: { Double }
Double   : L_doubl  { (read ( $1)) :: Double }

String  :: { String }
String   : L_quoted {  $1 }

Id :: { Id}
Id  : L_Id { Id ($1)}

Program :: { Program }
Program : ListDef { Abs.PDefs (reverse $1) }
Def :: { Def }
Def : Type Id '(' ListArg ')' '{' ListStm '}' { Abs.DFun $1 $2 $4 (reverse $7) }
ListDef :: { [Def] }
ListDef : {- empty -} { [] } | ListDef Def { flip (:) $1 $2 }
Arg :: { Arg }
Arg : Type Id { Abs.ADecl $1 $2 }
ListArg :: { [Arg] }
ListArg : {- empty -} { [] }
        | Arg { (:[]) $1 }
        | Arg ',' ListArg { (:) $1 $3 }
Stm :: { Stm }
Stm : Exp ';' { Abs.SExp $1 }
    | Type ListId ';' { Abs.SDecls $1 $2 }
    | Type Id '=' Exp ';' { Abs.SInit $1 $2 $4 }
    | 'return' Exp ';' { Abs.SReturn $2 }
    | 'return' ';' { Abs.SReturnVoid }
    | 'while' '(' Exp ')' Stm { Abs.SWhile $3 $5 }
    | '{' ListStm '}' { Abs.SBlock (reverse $2) }
    | 'if' '(' Exp ')' Stm 'else' Stm { Abs.SIfElse $3 $5 $7 }
ListStm :: { [Stm] }
ListStm : {- empty -} { [] } | ListStm Stm { flip (:) $1 $2 }
Exp15 :: { Exp }
Exp15 : 'true' { Abs.ETrue }
      | 'false' { Abs.EFalse }
      | Integer { Abs.EInt $1 }
      | Double { Abs.EDouble $1 }
      | String { Abs.EString $1 }
      | Id { Abs.EId $1 }
      | Id '(' ListExp ')' { Abs.EApp $1 $3 }
      | '(' Exp ')' { $2 }
Exp14 :: { Exp }
Exp14 : Exp15 '++' { Abs.EPIncr $1 }
      | Exp15 '--' { Abs.EPDecr $1 }
      | Exp15 { $1 }
Exp13 :: { Exp }
Exp13 : '++' Exp14 { Abs.EIncr $2 }
      | '--' Exp14 { Abs.EDecr $2 }
      | Exp14 { $1 }
Exp12 :: { Exp }
Exp12 : Exp12 '*' Exp13 { Abs.ETimes $1 $3 }
      | Exp12 '/' Exp13 { Abs.EDiv $1 $3 }
      | Exp13 { $1 }
Exp11 :: { Exp }
Exp11 : Exp11 '+' Exp12 { Abs.EPlus $1 $3 }
      | Exp11 '-' Exp12 { Abs.EMinus $1 $3 }
      | Exp12 { $1 }
Exp9 :: { Exp }
Exp9 : Exp9 '<' Exp10 { Abs.ELt $1 $3 }
     | Exp9 '>' Exp10 { Abs.EGt $1 $3 }
     | Exp9 '<=' Exp10 { Abs.ELtEq $1 $3 }
     | Exp9 '>=' Exp10 { Abs.EGtEq $1 $3 }
     | Exp10 { $1 }
Exp8 :: { Exp }
Exp8 : Exp8 '==' Exp9 { Abs.EEq $1 $3 }
     | Exp8 '!=' Exp9 { Abs.ENEq $1 $3 }
     | Exp9 { $1 }
Exp4 :: { Exp }
Exp4 : Exp4 '&&' Exp5 { Abs.EAnd $1 $3 } | Exp5 { $1 }
Exp3 :: { Exp }
Exp3 : Exp3 '||' Exp4 { Abs.EOr $1 $3 } | Exp4 { $1 }
Exp2 :: { Exp }
Exp2 : '(' Type ')' Exp3 { Abs.ECast $2 $4 } | Exp3 { $1 }
Exp1 :: { Exp }
Exp1 : Exp2 '=' Exp1 { Abs.EAss $1 $3 } | Exp2 { $1 }
Exp :: { Exp }
Exp : Exp1 { $1 }
Exp5 :: { Exp }
Exp5 : Exp6 { $1 }
Exp6 :: { Exp }
Exp6 : Exp7 { $1 }
Exp7 :: { Exp }
Exp7 : Exp8 { $1 }
Exp10 :: { Exp }
Exp10 : Exp11 { $1 }
ListExp :: { [Exp] }
ListExp : {- empty -} { [] }
        | Exp { (:[]) $1 }
        | Exp ',' ListExp { (:) $1 $3 }
Type :: { Type }
Type : 'bool' { Abs.Type_bool }
     | 'int' { Abs.Type_int }
     | 'double' { Abs.Type_double }
     | 'void' { Abs.Type_void }
     | 'string' { Abs.Type_string }
ListId :: { [Id] }
ListId : Id { (:[]) $1 } | Id ',' ListId { (:) $1 $3 }
{

happyError :: [Token] -> Either String a
happyError ts =
  Left $ "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ id(prToken t) ++ "'"

}
