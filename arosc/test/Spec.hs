import Test.Hspec
import Syntax
import qualified Parser
import qualified Lexer


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
    it "Basic shape declaration in grid def should parse correctly (01-simple-decl-and-grid)" $ do
      code <- readFile (resDir ++ "01-simple-decl-and-grid.aros")
      let ast = (Parser.parseAros . Lexer.lexAros) code
      let correctAst = Program
                        [ SDecl
                        "myShape" (UShape [ SMPoint (VVec (Vector (ILit 1) (ILit 1))) ])
                        ]
                        (GridDef
                        (SShape
                                (SVector (ILit 5) (ILit 5))
                                [ SMIdent "myShape" (VVec (Vector (ILit 1) (ILit 2))) ]))
      ast `shouldBe` correctAst
