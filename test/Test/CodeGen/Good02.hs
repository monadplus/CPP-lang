module Test.CodeGen.Good02
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
bool even(int x) {
  return (2*(x/2) == x);
}

int main ()
{
  int j;
  for(int i = 0; i < 10; i++) {
    if (even(i)) j = j + 1 ;
  }
  printInt(j);
}
|]

instrs :: [Instr]
instrs =
  runIdentity $
    withinMainClass $
      liftA2
        (++)
        (withinPublicStaticMethod evenMethod evenBody)
        (withinPublicStaticMethod mainMethod mainBody)
  where
    evenMethod :: Method
    evenMethod = Method mainClass "even" [I] Z

    evenBody :: Identity [Instr]
    evenBody =
      let label = "Label0" :: Label
       in Identity
            [ IConst1,
              IConst2,
              ILoad0,
              IConst2,
              IDiv,
              IMul,
              ILoad0,
              IfICmp label Eq,
              Pop,
              IConst0,
              AddLabel label,
              IReturn
            ]

    mainBody :: Identity [Instr]
    mainBody =
      let label1 = "Label1" :: Label
          label2 = "Label2" :: Label
          label3 = "Label3" :: Label
       in Identity
            [ IConst0,
              IStore1,
              IConst0,
              IStore2,
              AddLabel label1,
              ILoad2,
              Bipush 10,
              IfICmp label2 Ge,
              ILoad2,
              Invokestatic (Method {_mClass = "Main", _mName = "even", _mParameters = [I], _mReturn = Z}),
              IfCmp label3 Eq,
              ILoad1,
              IConst1,
              IAdd,
              Dup,
              IStore1,
              Pop,
              AddLabel label3,
              ILoad2,
              Dup,
              IConst1,
              IAdd,
              IStore2,
              Pop,
              Goto label1,
              AddLabel label2,
              ILoad1,
              Invokestatic (Method {_mClass = "Runtime", _mName = "printInt", _mParameters = [I], _mReturn = V})
            ]
