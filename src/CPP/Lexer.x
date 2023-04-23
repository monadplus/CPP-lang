{
{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -w #-}
module CPP.Lexer where

import qualified Data.Bits
import Data.Word (Word8)
import Data.Char (ord)
}

$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha     = [$small $large]

$digit     = [0-9]
$binit     = 0-1
$octit	   = 0-7
$hexit     = [0-9 A-F a-f]

$idchar    = [$alpha $digit \']
@varid     = $small $idchar*

@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@binary      = $binit+
@exponent    = [eE] [\-\+] @decimal

$universal = [. \n]

:-

-- Line comments
"#" [.]* ;
"//" [.]* ;

-- Block comments
\/ \* [$universal # \*]* \* ([$universal # [\* \/]] [$universal # \*]* \* | \*)* \/ ;

$white+ ;

"false"  { \p _ -> PT p TkFalse     }
"true"   { \p _ -> PT p TkTrue      }
"--"     { \p _ -> PT p TkDecr      }
"-"      { \p _ -> PT p TkMinus     }
"++"     { \p _ -> PT p TkIncr      }
"+"      { \p _ -> PT p TkPlus      }
","      { \p _ -> PT p TkComma     }
"*"      { \p _ -> PT p TkAsterisk  }
"/"      { \p _ -> PT p TkSlash     }
";"      { \p _ -> PT p TkSemiColon }
"<"      { \p _ -> PT p TkLess      }
"<="     { \p _ -> PT p TkLessEq    }
">"      { \p _ -> PT p TkGreater   }
">="     { \p _ -> PT p TkGreaterEq }
"=="     { \p _ -> PT p TkEq        }
"="      { \p _ -> PT p TkEqual     }
"!="     { \p _ -> PT p TkNeq       }
"&&"     { \p _ -> PT p TkAnd       }
"||"     { \p _ -> PT p TkOr        }
"if"     { \p _ -> PT p TkIf        }
"else"   { \p _ -> PT p TkElse      }
"while"  { \p _ -> PT p TkWhile     }
"for"    { \p _ -> PT p TkFor       }
"return" { \p _ -> PT p TkReturn    }
"int"    { \p _ -> PT p TkTyInt     }
"double" { \p _ -> PT p TkTyDouble  }
"bool"   { \p _ -> PT p TkTyBool    }
"string" { \p _ -> PT p TkTyString  }
"void"   { \p _ -> PT p TkTyVoid    }
\(       { \p _ -> PT p TkLParen    }
\)       { \p _ -> PT p TkRParen    }
\[       { \p _ -> PT p TkLBracket  }
\]       { \p _ -> PT p TkRBracket  }
\{       { \p _ -> PT p TkLBraces   }
\}       { \p _ -> PT p TkRBraces   }

@varid { \p s -> PT p (TkIdent s) }

\" ([$universal # [\" \\ \n]] | (\\ (\" | \\ | \' | n | t | r | f)))* \"
    { \p s -> PT p (TkString $ unescapeInitTail s) }

-- TODO: 'read' is not enough, you must parse those.
-- | 0[bB] @binary
-- | 0[oO] @octal
-- | 0[xX] @hexadecimal
@decimal 
  { \p s -> PT p (TkInteger (read s)) }

@decimal \. @decimal @exponent?
  | @decimal @exponent	{ \p s -> PT p (TkDouble (read s)) }

{
data Token
  = PT  Posn Tok
  | Err Posn
  deriving (Eq, Show, Ord)

data Tok
  = TkIdent !String
  | TkString !String
  | TkInteger !Integer
  | TkDouble !Double
  | TkFalse
  | TkTrue
  -- Symbols
  | TkEqual
  | TkMinus
  | TkPlus
  | TkAsterisk
  | TkSlash
  | TkComma
  | TkSemiColon
  | TkIncr
  | TkDecr
  -- Parenthesis
  | TkLParen
  | TkRParen
  | TkLBracket  
  | TkRBracket  
  | TkLBraces
  | TkRBraces
  -- Binary operators
  | TkLess
  | TkLessEq
  | TkGreater
  | TkGreaterEq
  | TkNeq
  | TkEq
  | TkAnd
  | TkOr
  -- Keywords
  | TkIf
  | TkElse
  | TkWhile
  | TkFor
  | TkReturn
  -- Types
  | TkTyInt
  | TkTyDouble
  | TkTyBool
  | TkTyString
  | TkTyVoid
  deriving (Eq, Ord)
  
instance Show Tok where
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

printPosn :: Posn -> String
printPosn (Pn _ l c) = "line " ++ show l ++ ", column " ++ show c

tokenPos :: [Token] -> String
tokenPos (t:_) = printPosn (tokenPosn t)
tokenPos [] = "end of file"

tokenPosn :: Token -> Posn
tokenPosn (PT p _) = p
tokenPosn (Err p) = p

tokenLineCol :: Token -> (Int, Int)
tokenLineCol = posLineCol . tokenPosn

posLineCol :: Posn -> (Int, Int)
posLineCol (Pn _ l c) = (l,c)

unescapeInitTail :: String -> String
unescapeInitTail = unesc . tail
  where
  unesc s = case s of
    '\\':c:cs | elem c ['\"', '\\', '\''] -> c : unesc cs
    '\\':'n':cs  -> '\n' : unesc cs
    '\\':'t':cs  -> '\t' : unesc cs
    '\\':'r':cs  -> '\r' : unesc cs
    '\\':'f':cs  -> '\f' : unesc cs
    '"':[]    -> []
    c:cs      -> c : unesc cs
    _         -> []

-------------------------------------------------------------------
-- Alex wrapper code.
-- A modified "posn" wrapper.
-------------------------------------------------------------------

data Posn = Pn !Int !Int !Int
      deriving (Eq, Show,Ord)

alexStartPos :: Posn
alexStartPos = Pn 0 1 1

alexMove :: Posn -> Char -> Posn
alexMove (Pn a l c) '\t' = Pn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (Pn a l c) '\n' = Pn (a+1) (l+1)   1
alexMove (Pn a l c) _    = Pn (a+1)  l     (c+1)

type Byte = Word8

type AlexInput = (Posn,     -- current position,
                  Char,     -- previous char
                  [Byte],   -- pending bytes on the current char
                  String)   -- current input string

tokens :: String -> [Token]
tokens str = go (alexStartPos, '\n', [], str)
    where
      go :: AlexInput -> [Token]
      go inp@(pos, _, _, str) =
               case alexScan inp 0 of
                AlexEOF                   -> []
                AlexError (pos, _, _, _)  -> [Err pos]
                AlexSkip  inp' len        -> go inp'
                AlexToken inp' len act    -> act pos (take len str) : (go inp')

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p, c, (b:bs), s) = Just (b, (p, c, bs, s))
alexGetByte (p, _, [], s) =
  case s of
    []  -> Nothing
    (c:s) ->
             let p'     = alexMove p c
                 (b:bs) = utf8Encode c
              in p' `seq` Just (b, (p', c, bs, s))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p, c, bs, s) = c

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
}
