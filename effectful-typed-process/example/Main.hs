import Effectful.Monad
import Effectful.Process.Typed

main :: IO ()
main = runEff . runTypedProcess $ true

true :: TypedProcess :> es => Eff es ()
true = Effectful.Process.Typed.runProcess_ $ shell "true"
