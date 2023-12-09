import Lib (Document, parseDocument, part1, part2)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [example1Test, example2Test, example3Test]

exampleTest :: (Document -> Int) -> String -> String -> Int -> TestTree
exampleTest part name input expected =
  testCase name $ part (parseDocument input) @?= expected

example1Test :: TestTree
example1Test = exampleTest part1 "Example 1" example 2
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
example2Test = exampleTest part1 "Example 2" example 6
  where
    example =
      "LLR\n\
      \\n\
      \AAA = (BBB, BBB)\n\
      \BBB = (AAA, ZZZ)\n\
      \ZZZ = (ZZZ, ZZZ)\n"

example3Test :: TestTree
example3Test = exampleTest part2 "Example 3" example 6
  where
    example =
      "LR\n\
      \\n\
      \11A = (11B, XXX)\n\
      \11B = (XXX, 11Z)\n\
      \11Z = (11B, XXX)\n\
      \22A = (22B, XXX)\n\
      \22B = (22C, 22C)\n\
      \22C = (22Z, 22Z)\n\
      \22Z = (22B, 22B)\n\
      \XXX = (XXX, XXX)\n"
