import Test.Hspec
import Syntax
import Parser


resDir = "test/resources/" :: FilePath

main :: IO ()
main = hspec $ do
  -- EXAMPLE
  describe "Addition (Example)" $ do
    let onePlusOne = 1 + 1

    it "1 + 1 should be greater than 1" $ do
      (onePlusOne > 1) `shouldBe` True
    it "1 + 1 should equal 2" $ do
      onePlusOne `shouldBe` 2


  describe "Parsing" $ do
    it "Basic shape declaration in grid def should parse correctly (simple-decl-and-grid)" $ do
      let file = "simple-decl-and-grid.aros"
      code <- readFile (resDir ++ file)
      let (Right ast) = (parseAros file code)
      let correctAst = Program
                        [ SDecl
                         "myShape" (UShape [ SMPoint (VVec (Vector (ILit 1) (ILit 1))) ])
                        ]
                        (GridDef
                        (SShape
                                (SVector (ILit 5) (ILit 5))
                                [ SMIdent "myShape" (VVec (Vector (ILit 1) (ILit 2))) ]))
      ast `shouldBe` correctAst
