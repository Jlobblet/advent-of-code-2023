import Lib (parseState, part1)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [example1Test, example2Test]

exampleTest :: String -> String -> Int -> TestTree
exampleTest name input expected =
  testCase name $ part1 (parseState input) @?= expected

example1Test :: TestTree
example1Test = exampleTest "Example 1" example 2
  where
    example =
      "RL\n\
      \\n\
      \AAA = (BBB, CCC)\n\
      \BBB = (DDD, EEE)\n\
      \CCC = (ZZZ, GGG)\n\
      \DDD = (DDD, DDD)\n\
      \EEE = (EEE, EEE)\n\
      \GGG = (GGG, GGG)\n\
      \ZZZ = (ZZZ, ZZZ)\n"

example2Test :: TestTree
example2Test = exampleTest "Example 2" example 6
  where
    example =
      "LLR\n\
      \\n\
      \AAA = (BBB, BBB)\n\
      \BBB = (AAA, ZZZ)\n\
      \ZZZ = (ZZZ, ZZZ)\n"
