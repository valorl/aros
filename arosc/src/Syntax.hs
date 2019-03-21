-- |

module Syntax where

-- TOKENS
data Token = TokenIntLit Int
           | TokenInt
           | TokenVec
           | TokenShape
           | TokenAt
           | TokenGrid
           | TokenPoint
           | TokenEq
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDiv
           | TokenLParen
           | TokenRParen
           | TokenLBrace
           | TokenRBrace
           | TokenLBracket
           | TokenRBracket
           | TokenComma
           | TokenIdent String
           | TokenEOF
           deriving (Eq,Show)









-- AST

data Op = Add | Sub | Mul | Div
           deriving (Eq,Show)


data IExp = ILit Int                             -- 1
          | IIdent String                   -- myInt
          | IParen IExp                     -- ( ... )
          | IOp IExp Op IExp                -- ... (+|-|*|/) ...
           deriving (Eq,Show)

data Vector = Vector IExp IExp                 -- ( ... , ... )
           deriving (Eq,Show)

data VExp = VVec Vector                     -- ( ... , ... )
          | VIdent String                   -- myVec
          | VParen VExp                     -- ( ( ... ) )
          | VOp VExp Op VExp                -- ( ..., ... ) (+|-|*|/) ( ... , ... )
          | VScaMul IExp VExp               -- 5 * (3,2) or (3,2) * 5 as well (evaluation is the same)
           deriving (Eq,Show)

data SVector = SVector IExp IExp               -- [ ... , ... ]
           deriving (Eq,Show)

data ShapeMan = SMIdent String VExp        -- myThing at (1,1)
               | SMShape Shape VExp         -- { ... } at (1,1)
               | SMPoint VExp               -- point   at (1,1)
           deriving (Eq,Show)

data Shape = UShape [ShapeMan]             -- { ... }
           | SShape SVector [ShapeMan]     -- [5,5] { ... }
           deriving (Eq,Show)

data VarDecl = IDecl String IExp            -- int myInt = 1
             | VDecl String VExp            -- vec myVec = (1,1)
             | SDecl String Shape           -- shape myShape = { ... }
           deriving (Eq,Show)

data GridDef = GridDef Shape      -- ... grid [120,120] { ... }
           deriving (Eq,Show)

data Program = Program [VarDecl] GridDef -- other defs later
           deriving (Eq,Show)
