{
{-# OPTIONS_GHC -w #-}
module CPP.Lexer (scan) where

import CPP.Lexer.Support

import Control.Monad.State
import Control.Monad.Except
import Data.List.NonEmpty (NonEmpty (..), (<|))
}

%encoding "latin1"

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

<0> $white+ ;
<0> "#" [.]* ; -- Remove
<0> "//" [.]* ; -- Line comments
<0> \/ \* [$universal # \*]* \* ([$universal # [\* \/]] [$universal # \*]* \* | \*)* \/ ; -- Block comments

-- Reserved Keywords
<0> "false"  { token TkFalse     }
<0> "true"   { token TkTrue      }
<0> "--"     { token TkDecr      }
<0> "-"      { token TkMinus     }
<0> "++"     { token TkIncr      }
<0> "+"      { token TkPlus      }
<0> ","      { token TkComma     }
<0> "*"      { token TkAsterisk  }
<0> "/"      { token TkSlash     }
<0> ";"      { token TkSemiColon }
<0> "<"      { token TkLess      }
<0> "<="     { token TkLessEq    }
<0> ">"      { token TkGreater   }
<0> ">="     { token TkGreaterEq }
<0> "=="     { token TkEq        }
<0> "="      { token TkEqual     }
<0> "!="     { token TkNeq       }
<0> "&&"     { token TkAnd       }
<0> "||"     { token TkOr        }
<0> "if"     { token TkIf        }
<0> "else"   { token TkElse      }
<0> "while"  { token TkWhile     }
<0> "for"    { token TkFor       }
<0> "return" { token TkReturn    }
<0> "int"    { token TkTyInt     }
<0> "double" { token TkTyDouble  }
<0> "bool"   { token TkTyBool    }
<0> "string" { token TkTyString  }
<0> "void"   { token TkTyVoid    }
<0> \(       { token TkLParen    }
<0> \)       { token TkRParen    }
<0> \[       { token TkLBracket  }
<0> \]       { token TkRBracket  }
<0> \{       { token TkLBraces   }
<0> \}       { token TkRBraces   }

-- Idents
<0> @varid { emit TkIdent }

-- Strings
<0> \" ([$universal # [\" \\ \n]] | (\\ (\" | \\ | \' | n | t | r | f)))* \"
    { emit (TkString . unescapeInitTail) }

-- Integer
<0> @decimal 
  { emit (TkInteger . read) }
-- TODO: 'read' is not enough, you must parse those.
-- | 0[bB] @binary
-- | 0[oO] @octal
-- | 0[xX] @hexadecimal

-- Floating point
<0> @decimal \. @decimal @exponent?
    | @decimal @exponent	
    { emit (TkDouble . read) }

{
handleEOF :: Lexer Token
handleEOF = pure TkEOF

scan :: Lexer Token
scan = do
  input@(Input _ _ _ string) <- gets lexerInput
  startcode <- startCode
  case alexScan input startcode of
    AlexEOF -> handleEOF
    AlexError (Input _ _ _ inp) ->
      throwError $ "Lexical error: " ++ show (head inp)
    AlexSkip input' _ -> do
      modify' $ \s -> s { lexerInput = input' }
      scan
    AlexToken input' tokl action -> do
      modify' $ \s -> s { lexerInput = input' }
      action (take tokl string)
}
