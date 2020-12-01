import Test.Hspec
import qualified AdventSpec.Day1 as D1
import qualified AdventSpec.Day2 as D2

main :: IO ()
main = hspec $ do
  D1.spec
  D2.spec