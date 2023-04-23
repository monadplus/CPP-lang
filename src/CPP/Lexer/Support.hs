{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CPP.Lexer.Support
  ( -- * Lexing
    Token (..),

    -- ** Alex must implement
    AlexInput (..),
    alexGetByte,
    alexInputPrevChar,

    -- ** Lexer Monad
    Lexer (..),
    runLexer,
    LexerState (..),
    initState,
    startCode,
    pushStartCode,
    popStartCode,
    emit,
    token,

    -- ** Auxiliary functions
    unescapeInitTail,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.Char (ord)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Word

data Token
  = TkIdent !String
  | -- Values
    TkString !String
  | TkInteger !Integer
  | TkDouble !Double
  | TkFalse
  | TkTrue
  | -- Symbols
    TkEqual
  | TkMinus
  | TkPlus
  | TkAsterisk
  | TkSlash
  | TkComma
  | TkSemiColon
  | TkIncr
  | TkDecr
  | -- Parenthesis
    TkLParen
  | TkRParen
  | TkLBracket
  | TkRBracket
  | TkLBraces
  | TkRBraces
  | -- Binary operators
    TkLess
  | TkLessEq
  | TkGreater
  | TkGreaterEq
  | TkNeq
  | TkEq
  | TkAnd
  | TkOr
  | -- Keywords
    TkIf
  | TkElse
  | TkWhile
  | TkFor
  | TkReturn
  | -- Types
    TkTyInt
  | TkTyDouble
  | TkTyBool
  | TkTyString
  | TkTyVoid
  | -- EOF
    TkEOF
  deriving stock (Eq, Ord)

instance Show Token where
  show t = case t of
    (TkIdent s) -> show s
    (TkString s) -> s
    (TkInteger i) -> show i
    (TkDouble d) -> show d
    TkFalse -> "false"
    TkTrue -> "true"
    TkEqual -> "="
    TkMinus -> "-"
    TkPlus -> "+"
    TkAsterisk -> "*"
    TkSlash -> "/"
    TkComma -> ","
    TkSemiColon -> ";"
    TkIncr -> "++"
    TkDecr -> "--"
    TkLParen -> "("
    TkRParen -> ")"
    TkLBracket -> "["
    TkRBracket -> "]"
    TkLBraces -> "{"
    TkRBraces -> "}"
    TkLess -> "<"
    TkLessEq -> "<="
    TkGreater -> ">"
    TkGreaterEq -> ">="
    TkNeq -> "!="
    TkEq -> "=="
    TkAnd -> "&&"
    TkOr -> "||"
    TkIf -> "if"
    TkElse -> "else"
    TkWhile -> "while"
    TkFor -> "for"
    TkReturn -> "return"
    TkTyInt -> "int"
    TkTyDouble -> "double"
    TkTyBool -> "bool"
    TkTyString -> "string"
    TkTyVoid -> "void"
    TkEOF -> "eof"

data AlexInput = Input
  { inpLine :: {-# UNPACK #-} !Int,
    inpColumn :: {-# UNPACK #-} !Int,
    inpLast :: {-# UNPACK #-} !Char,
    inpStream :: String
  }
  deriving stock (Eq, Show)

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte inp@Input {inpStream = str} = advance <$> List.uncons str
  where
    advance ('\n', rest) =
      ( fromIntegral (ord '\n'),
        Input
          { inpLine = inpLine inp + 1,
            inpColumn = 1,
            inpLast = '\n',
            inpStream = rest
          }
      )
    advance (c, rest) =
      ( fromIntegral (ord c),
        Input
          { inpLine = inpLine inp,
            inpColumn = inpColumn inp + 1,
            inpLast = c,
            inpStream = rest
          }
      )

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = inpLast

newtype Lexer a = Lexer {_getLexer :: StateT LexerState (Either String) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState LexerState,
      MonadError String
    )

data LexerState = LexerState
  { lexerInput :: {-# UNPACK #-} !AlexInput,
    lexerStartCodes :: {-# UNPACK #-} !(NonEmpty Int)
  }
  deriving stock (Eq, Show)

initState :: String -> LexerState
initState str =
  LexerState
    { lexerInput = Input 0 1 '\n' str,
      lexerStartCodes = 0 :| []
    }

runLexer :: Lexer a -> String -> Either String a
runLexer act s = evalStateT (_getLexer act) (initState s)

startCode :: Lexer Int
startCode = gets (NE.head . lexerStartCodes)

pushStartCode :: Int -> Lexer ()
pushStartCode i = modify' $ \st ->
  st
    { lexerStartCodes = NE.cons i (lexerStartCodes st)
    }

popStartCode :: Lexer ()
popStartCode = modify' $ \st ->
  st
    { lexerStartCodes =
        case lexerStartCodes st of
          _ :| [] -> 0 :| []
          _ :| (x : xs) -> x :| xs
    }

emit :: (String -> Token) -> String -> Lexer Token
emit = (pure .)

token :: Token -> String -> Lexer Token
token = const . pure

unescapeInitTail :: String -> String
unescapeInitTail = unesc . tail
  where
    unesc s = case s of
      '\\' : c : cs | c `elem` ['\"', '\\', '\''] -> c : unesc cs
      '\\' : 'n' : cs -> '\n' : unesc cs
      '\\' : 't' : cs -> '\t' : unesc cs
      '\\' : 'r' : cs -> '\r' : unesc cs
      '\\' : 'f' : cs -> '\f' : unesc cs
      ['"'] -> []
      c : cs -> c : unesc cs
      _ -> []
