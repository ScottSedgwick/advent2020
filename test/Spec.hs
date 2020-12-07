import Test.Hspec
-- import qualified AdventSpec.Day1 as D1
-- import qualified AdventSpec.Day2 as D2
-- import qualified AdventSpec.Day3 as D3
-- import qualified AdventSpec.Day4 as D4
import qualified AdventSpec.Day5 as D5
-- import qualified AdventSpec.Day6 as D6
import qualified AdventSpec.Day7 as D7

main :: IO ()
main = hspec $ do
  -- D1.spec
  -- D2.spec
  -- D3.spec
  -- D4.spec
  D5.spec
  -- D6.spec
  D7.spec