module Test.CodeGen.Good01
  ( src,
    instrs,
  )
where

import CPP.JVM.Jasmin
import Data.String.QQ
import Data.Functor.Identity

src :: String
src =
  [s|
int main ()
{
  int x;
  x = 10;
  int i = 10;
  i = i + x;
  printInt(i);
}
|]

instrs :: [Instr]
instrs =
  runIdentity $
    withinMainClass $
      withinPublicStaticMethod mainMethod main
 where
   main :: Identity [Instr]
   main = Identity
     [ IConst0,
       IStore1,
       Bipush 10,
       Dup,
       IStore1,
       Pop,
       Bipush 10,
       IStore2,
       ILoad2,
       ILoad1,
       IAdd,
       Dup,
       IStore2,
       Pop,
       ILoad2,
       Invokestatic (Method {_mClass="Runtime", _mName="printInt", _mParameters=[I], _mReturn=V})
     ]
