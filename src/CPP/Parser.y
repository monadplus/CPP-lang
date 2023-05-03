{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module CPP.Parser (lexer, parseProgram, runParser) where

import CPP.AST as AST
import CPP.Lexer
import CPP.Lexer.Support

import Control.Monad.Except (throwError)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
}

%name parseProgram Program

%tokentype { Token }
%monad { Lexer }
%lexer { lexer } { TkEOF }

%errorhandlertype explist
%error { parseError }

%token
  L_Id     { TkIdent $$ }
  L_quoted { TkString $$ }
  L_integ  { TkInteger $$ }
  L_doubl  { TkDouble $$ }

  'false'  { TkFalse }
  'true'   { TkTrue }

  '='      { TkEqual }
  '-'      { TkMinus }
  '+'      { TkPlus }
  '*'      { TkAsterisk }
  '/'      { TkSlash }
  ','      { TkComma }
  ';'      { TkSemiColon }
  '++'     { TkIncr }
  '--'     { TkDecr }

  '('      { TkLParen }
  ')'      { TkRParen }
  '['      { TkLBracket }
  ']'      { TkRBracket }
  '{'      { TkLBraces }
  '}'      { TkRBraces }
  
  '<'      { TkLess }
  '<='     { TkLessEq }
  '>'      { TkGreater }
  '>='     { TkGreaterEq }
  '!='     { TkNeq }
  '=='     { TkEq }
  '&&'     { TkAnd }
  '||'     { TkOr }

  'if'     { TkIf }
  'else'   { TkElse }
  'while'  { TkWhile }
  'for'    { TkFor }
  'return' { TkReturn }

  'int'    { TkTyInt }
  'double' { TkTyDouble }
  'bool'   { TkTyBool }
  'string' { TkTyString }
  'void'   { TkTyVoid }
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
Program : ListDef { AST.PDefs (List.reverse $1) }

Def :: { UDef }
Def : Type Id '(' ListArg ')' '{' ListStm '}' { AST.DFun $1 $2 $4 List.reverse $7) }

ListDef :: { [UDef] }
ListDef : {- empty -} { [] } 
        | ListDef Def { flip (:) $1 $2 }

Arg :: { Arg }
Arg : Type Id ListArraySize { AST.ADecl (mkArrTy $1 $3) $2 }
    | Type Id { AST.ADecl $1 $2 }

ListArg :: { [Arg] }
ListArg : {- empty -} { [] }
        | Arg { (:[]) $1 }
        | Arg ',' ListArg { (:) $1 $3 }

Stm :: { UStm }
Stm : Exp ';' { AST.SExp $1 }
    | Type ListId ';' { AST.SDecls $1 $2 }
    | Type ListId ListArraySize ';' { AST.SDecls (mkArrTy $1 $3) $2 }
    | Type Id '=' Exp ';' { AST.SInit $1 $2 $4 }
    | Type Id ListArraySize '=' Exp ';' { AST.SInit (mkArrTy $1 $3) $2 $5 }
    | 'return' Exp ';' { AST.SReturn $2 }
    | 'return' ';' { AST.SReturnVoid }
    | 'while' '(' Exp ')' Stm { AST.SWhile $3 $5 }
    | 'for' '(' Stm Exp ';' Exp ')' Stm { AST.for $3 $4 $6 $8 }
    | '{' ListStm '}' { AST.SBlock List.reverse $2) }
    | 'if' '(' Exp ')' Stm Else { AST.SIfElse $3 $5 $6 }

Else :: { AST.UElse }
Else : 'else' Stm { AST.EElse $2 }
     | {- empty -} { AST.EEmpty }

ListStm :: { [UStm] }
ListStm : {- empty -} { [] } 
        | ListStm Stm { flip (:) $1 $2 }

ListArraySize :: { NonEmpty Integer }
ListArraySize : '[' L_integ ']' { NonEmpty.singleton $2 }
              | ListArraySize '[' L_integ ']' { flip (NonEmpty.cons) $1 $3 }

ExpsSepByComma :: { [ UExp ] }
ExpsSepByComma : Exp { [ $1 ] }
               | ExpsSepByComma ',' Exp { flip (:) $1 $3 }

Exp :: { UExp }
Exp : Exp1 { $1 }

Exp1 :: { UExp }
Exp1 : Exp2 '=' Exp1 { AST.EAss $1 $3 } 
     | Exp2 { $1 }

Exp2 :: { UExp }
Exp2 : '(' Type ')' Exp3 { AST.ECast $2 $4 } 
     | Exp3 { $1 }

Exp3 :: { UExp }
Exp3 : Exp3 '||' Exp4 { AST.EOr $1 $3 } 
     | Exp4 { $1 }

Exp4 :: { UExp }
Exp4 : Exp4 '&&' Exp5 { AST.EAnd $1 $3 } 
     | Exp5 { $1 }

Exp5 :: { UExp }
Exp5 : Exp5 '==' Exp6 { AST.EEq $1 $3 }
     | Exp5 '!=' Exp6 { AST.ENEq $1 $3 }
     | Exp6 { $1 }

Exp6 :: { UExp }
Exp6 : Exp6 '<' Exp7 { AST.ELt $1 $3 }
     | Exp6 '>' Exp7 { AST.EGt $1 $3 }
     | Exp6 '<=' Exp7 { AST.ELtEq $1 $3 }
     | Exp6 '>=' Exp7 { AST.EGtEq $1 $3 }
     | Exp7 { $1 }

Exp7 :: { UExp }
Exp7 : Exp7 '+' Exp8 { AST.EPlus $1 $3 }
      | Exp7 '-' Exp8 { AST.EMinus $1 $3 }
      | Exp8 { $1 }

Exp8 :: { UExp }
Exp8 : Exp8 '*' Exp9 { AST.ETimes $1 $3 }
      | Exp8 '/' Exp9 { AST.EDiv $1 $3 }
      | Exp9 { $1 }

Exp9 :: { UExp }
Exp9 : '++' Exp10 { AST.EIncr $2 }
      | '--' Exp10 { AST.EDecr $2 }
      | Exp10 { $1 }

Exp10 :: { UExp }
Exp10 : Exp11 '++' { AST.EPIncr $1 }
      | Exp11 '--' { AST.EPDecr $1 }
      | Exp11 { $1 }

Exp11 :: { UExp }
Exp11 : 'true' { AST.ETrue }
      | 'false' { AST.EFalse }
      | Integer { AST.EInt $1 }
      | Double { AST.EDouble $1 }
      | String { AST.EString $1 }
      | Id { AST.EId $1 }
      | Id '(' ListExp ')' { AST.EApp $1 $3 }
      | Id IndexExp { AST.EApp $1 $2 }
      | '{' ExpsSepByComma '}' { AST.EArr (List.reverse $2) }
      | '(' Exp ')' { $2 }

ListExp :: { [UExp] }
ListExp : {- empty -} { [] }
        | Exp { (:[]) $1 }
        | Exp ',' ListExp { (:) $1 $3 }

IndexExp :: { NonEmpty UExp }
IndexExp : '[' Exp ']' { NonEmpty.singleton $2 }
         | IndexExp '[' Exp ']' { flip (NonEmpty.cons) $1 $3 }

Type :: { Type }
Type : 'bool' { AST.Type_bool }
     | 'int' { AST.Type_int }
     | 'double' { AST.Type_double }
     | 'void' { AST.Type_void }
     | 'string' { AST.Type_string }

ListId :: { [Id] }
ListId : Id { (:[]) $1 } 
       | Id ',' ListId { (:) $1 $3 }

{
mkArrTy :: Type -> NonEmpty Integer -> Type
mkArrTy ty size = AST.TyArray ty (NonEmpty.reverse size)

lexer :: (Token -> Lexer a) -> Lexer a
lexer cont = scan >>= cont

parseError = throwError . show

runParser :: Lexer a -> (String -> Either String a)
runParser = runLexer
}
