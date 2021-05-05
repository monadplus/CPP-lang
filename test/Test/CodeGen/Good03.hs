module Test.CodeGen.Good03
  ( src,
    instrs,
  )
where

import CPP.JVM.Jasmin
import Control.Applicative (liftA2)
import Data.Functor.Identity
import Data.String.QQ

src :: String
src =
  [s|
int factr (int n)
{
  if (n<2)
    return 1 ;
  else
    return (n * factr(n-1)) ;
}

int main ()
{
  int i = factr(7) ;
  printInt(i);
}
|]

instrs :: [Instr]
instrs =
  runIdentity $
    withinMainClass $
      liftA2
        (++)
        (withinPublicStaticMethod factrMethod factrBody)
        (withinPublicStaticMethod mainMethod mainBody)
  where
    factrMethod :: Method
    factrMethod = Method mainClass "factr" [I] I

    factrBody :: Identity [Instr]
    factrBody =
      let label0 = "Label0" :: Label
          label1 = "Label1" :: Label
          label2 = "Label2" :: Label
       in Identity
            [ IConst1,
              ILoad0,
              IConst2,
              IfICmp label2 Lt,
              Pop,
              IConst0,
              AddLabel label2,
              IfCmp label0 Eq,
              IConst1,
              IReturn,
              Goto label1,
              AddLabel label0,
              ILoad0,
              ILoad0,
              IConst1,
              ISub,
              Invokestatic (Method {_mClass = "Main", _mName = "factr", _mParameters = [I], _mReturn = I}),
              IMul,
              IReturn,
              AddLabel label1
            ]

    mainBody :: Identity [Instr]
    mainBody =
      Identity
        [ Bipush 7,
          Invokestatic (Method {_mClass = "Main", _mName = "factr", _mParameters = [I], _mReturn = I}),
          IStore1,
          ILoad1,
          Invokestatic (Method {_mClass = "Runtime", _mName = "printInt", _mParameters = [I], _mReturn = V})
        ]
