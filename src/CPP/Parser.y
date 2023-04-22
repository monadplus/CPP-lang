{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module CPP.Parser (parse) where

import CPP.AST as AST
import CPP.Lexer
}

%name pProgram Program
%monad { Either String } { (>>=) } { return }
%tokentype {Token}

%token
  L_Id { PT _ (TkIdent $$) }
  L_quoted { PT _ (TkString $$) }
  L_integ  { PT _ (TkInteger $$) }
  L_doubl  { PT _ (TkDouble $$) }
  'false' { PT _ TkFalse }
  'true' { PT _ TkTrue }

  '=' { PT _ TkEqual }
  '-' { PT _ TkMinus }
  '+' { PT _ TkPlus }
  '*' { PT _ TkAsterisk }
  '/' { PT _ TkSlash }
  ',' { PT _ TkComma }
  ';' { PT _ TkSemiColon }
  '++' { PT _ TkIncr }
  '--' { PT _ TkDecr }

  '(' { PT _ TkLParen }
  ')' { PT _ TkRParen }
  '[' { PT _ TkLBracket }
  ']' { PT _ TkRBracket }
  '{' { PT _ TkLBraces }
  '}' { PT _ TkRBraces }
  
  '<' { PT _ TkLess }
  '<=' { PT _ TkLessEq }
  '>' { PT _ TkGreater }
  '>=' { PT _ TkGreaterEq }
  '!=' { PT _ TkNeq }
  '==' { PT _ TkEq }
  '&&' { PT _ TkAnd }
  '||' { PT _ TkOr }

  'if' { PT _ TkIf }
  'else' { PT _ TkElse }
  'while' { PT _ TkWhile }
  'for' { PT _ TkFor }
  'return' { PT _ TkReturn }

  'int' { PT _ TkTyInt }
  'double' { PT _ TkTyDouble }
  'bool' { PT _ TkTyBool }
  'string' { PT _ TkTyString }
  'void' { PT _ TkTyVoid }
%%

Integer :: { Integer }
Integer  : L_integ  { $1 }

Double  :: { Double }
Double   : L_doubl  { $1 }

String  :: { String }
String   : L_quoted { $1 }

Id :: { Id }
Id  : L_Id { Id $1 }

Program :: { UProgram }
Program : ListDef { AST.PDefs (reverse $1) }

Def :: { UDef }
Def : Type Id '(' ListArg ')' '{' ListStm '}' { AST.DFun $1 $2 $4 (reverse $7) }

ListDef :: { [UDef] }
ListDef : {- empty -} { [] } | ListDef Def { flip (:) $1 $2 }

Arg :: { Arg }
Arg : Type Id { AST.ADecl $1 $2 }

ListArg :: { [Arg] }
ListArg : {- empty -} { [] }
        | Arg { (:[]) $1 }
        | Arg ',' ListArg { (:) $1 $3 }

Stm :: { UStm }
Stm : Exp ';' { AST.SExp $1 }
    | Type ListId ';' { AST.SDecls $1 $2 }
    | Type Id '=' Exp ';' { AST.SInit $1 $2 $4 }
    | 'return' Exp ';' { AST.SReturn $2 }
    | 'return' ';' { AST.SReturnVoid }
    | 'while' '(' Exp ')' Stm { AST.SWhile $3 $5 }
    | 'for' '(' Stm Exp ';' Exp ')' Stm { AST.for $3 $4 $6 $8 }
    | '{' ListStm '}' { AST.SBlock (reverse $2) }
    | 'if' '(' Exp ')' Stm Else { AST.SIfElse $3 $5 $6 }

Else :: { AST.UElse }
Else : 'else' Stm { AST.EElse $2 }
     | {- empty -} { AST.EEmpty }

ListStm :: { [UStm] }
ListStm : {- empty -} { [] } | ListStm Stm { flip (:) $1 $2 }

Exp15 :: { UExp }
Exp15 : 'true' { AST.ETrue }
      | 'false' { AST.EFalse }
      | Integer { AST.EInt $1 }
      | Double { AST.EDouble $1 }
      | String { AST.EString $1 }
      | Id { AST.EId $1 }
      | Id '(' ListExp ')' { AST.EApp $1 $3 }
      | '(' Exp ')' { $2 }

Exp14 :: { UExp }
Exp14 : Exp15 '++' { AST.EPIncr $1 }
      | Exp15 '--' { AST.EPDecr $1 }
      | Exp15 { $1 }

Exp13 :: { UExp }
Exp13 : '++' Exp14 { AST.EIncr $2 }
      | '--' Exp14 { AST.EDecr $2 }
      | Exp14 { $1 }

Exp12 :: { UExp }
Exp12 : Exp12 '*' Exp13 { AST.ETimes $1 $3 }
      | Exp12 '/' Exp13 { AST.EDiv $1 $3 }
      | Exp13 { $1 }

Exp11 :: { UExp }
Exp11 : Exp11 '+' Exp12 { AST.EPlus $1 $3 }
      | Exp11 '-' Exp12 { AST.EMinus $1 $3 }
      | Exp12 { $1 }

Exp9 :: { UExp }
Exp9 : Exp9 '<' Exp10 { AST.ELt $1 $3 }
     | Exp9 '>' Exp10 { AST.EGt $1 $3 }
     | Exp9 '<=' Exp10 { AST.ELtEq $1 $3 }
     | Exp9 '>=' Exp10 { AST.EGtEq $1 $3 }
     | Exp10 { $1 }

Exp8 :: { UExp }
Exp8 : Exp8 '==' Exp9 { AST.EEq $1 $3 }
     | Exp8 '!=' Exp9 { AST.ENEq $1 $3 }
     | Exp9 { $1 }

Exp4 :: { UExp }
Exp4 : Exp4 '&&' Exp5 { AST.EAnd $1 $3 } | Exp5 { $1 }

Exp3 :: { UExp }
Exp3 : Exp3 '||' Exp4 { AST.EOr $1 $3 } | Exp4 { $1 }

Exp2 :: { UExp }
Exp2 : '(' Type ')' Exp3 { AST.ECast $2 $4 } | Exp3 { $1 }

Exp1 :: { UExp }
Exp1 : Exp2 '=' Exp1 { AST.EAss $1 $3 } | Exp2 { $1 }

Exp :: { UExp }
Exp : Exp1 { $1 }

Exp5 :: { UExp }
Exp5 : Exp6 { $1 }

Exp6 :: { UExp }
Exp6 : Exp7 { $1 }

Exp7 :: { UExp }
Exp7 : Exp8 { $1 }

Exp10 :: { UExp }
Exp10 : Exp11 { $1 }

ListExp :: { [UExp] }
ListExp : {- empty -} { [] }
        | Exp { (:[]) $1 }
        | Exp ',' ListExp { (:) $1 $3 }

Type :: { Type }
Type : 'bool' { AST.Type_bool }
     | 'int' { AST.Type_int }
     | 'double' { AST.Type_double }
     | 'void' { AST.Type_void }
     | 'string' { AST.Type_string }

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

parse :: String -> Either String AST.UProgram
parse = pProgram . tokens
}
