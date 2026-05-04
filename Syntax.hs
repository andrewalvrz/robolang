module Syntax where

import Data.List (intercalate)

{-

CONTEXT FREE GRAMMAR — RoboArena

<program>    ::= <init_arena> <decl_list> <init_robot> <stmt_list>

<init_arena> ::= "init_arena" <number> <number>

<decl_list>  ::= <decl> | <decl> "\n" <decl_list>
<decl>       ::= "place" "(" <number> "," <number> ")" <string>

<init_robot> ::= "robot" "(" <number> "," <number> ")" <number> <number>

<stmt_list>  ::= <stmt> | <stmt> "\n" <stmt_list>
<stmt>       ::= "move" <number>
               | "turn" <angle>
               | "grab"
               | "check_status"
               | "update_capacity" <number>
               | "if_collected" <string> "{" <stmt_list> "}" "{" <stmt_list> "}"
               | "display_arena"
               | "display_robot"

<angle>      ::= "0" | "90" | "180" | "270"
<number>     ::= <digit>+ [ "." <digit>+ ]
<digit>      ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
<string>     ::= a sequence of characters

-}

type Position = (Double, Double)

data Arena = Arena
    { width  :: Double
    , height :: Double
    , items  :: [(Position, String)]
    } deriving (Eq)

data RobotState = RobotState
    { xPos        :: Double
    , yPos        :: Double
    , heading     :: Double
    , capacity    :: Int
    , collections :: [String]
    , isAlive     :: Bool
    } deriving (Eq)

data Env = Env
    { robot :: RobotState
    , arena :: Arena
    } deriving (Eq)

data Program = Program InitArena [Decl] InitRobot [Stmt]

data InitArena = InitArena Double Double

data InitRobot = Robot Position Double Int

data Decl = Place Position String

data Stmt
    = Move Double
    | Turn Double
    | Grab
    | CheckStatus
    | UpdateCapacity Int
    | IfCollected String [Stmt] [Stmt]
    | DisplayArena
    | DisplayRobot
    deriving (Eq)

data BoundsResult = InBounds | OutOfBounds String
    deriving (Eq)

data CollectionResult = AllCollected | ItemsRemaining [String]
    deriving (Eq)

instance Show InitArena where
    show (InitArena w h) = "init_arena " ++ show w ++ " " ++ show h

instance Show InitRobot where
    show (Robot (x, y) h c) =
        "robot (" ++ show x ++ ", " ++ show y ++ ") " ++ show h ++ " " ++ show c

instance Show Decl where
    show (Place (x, y) name) =
        "place (" ++ show x ++ ", " ++ show y ++ ") " ++ "\"" ++ name ++ "\""

instance Show Stmt where
    show (Move d)        = "move " ++ show d
    show (Turn a)        = "turn " ++ show a
    show Grab            = "grab"
    show CheckStatus     = "check_status"
    show (UpdateCapacity n) = "update_capacity " ++ show n
    show (IfCollected item thenBranch elseBranch) =
        "if_collected \"" ++ item ++ "\" { "
        ++ showStmtList thenBranch ++ " } { "
        ++ showStmtList elseBranch ++ " }"
    show DisplayArena    = "display_arena"
    show DisplayRobot    = "display_robot"

showStmtList :: [Stmt] -> String
showStmtList stmts = intercalate "; " (map show stmts)

instance Show Arena where
    show a = "--- ARENA ---\n"
        ++ "Size    : " ++ show (width a) ++ " x " ++ show (height a) ++ "\n"
        ++ "Items   : " ++ showItems (items a) ++ "\n"
        ++ "-------------"

showItems :: [(Position, String)] -> String
showItems [] = "none"
showItems itms = intercalate ", " (map showOneItem itms)
  where
    showOneItem ((x, y), name) = name ++ " at (" ++ show x ++ ", " ++ show y ++ ")"

instance Show RobotState where
    show s = "--- ROBOT ---\n"
        ++ "Position    : (" ++ show (xPos s) ++ ", " ++ show (yPos s) ++ ")\n"
        ++ "Heading     : " ++ show (heading s) ++ " degrees\n"
        ++ "Capacity    : " ++ show (capacity s) ++ "\n"
        ++ "Collected   : " ++ showCollections (collections s) ++ "\n"
        ++ "Status      : " ++ (if isAlive s then "FUNCTIONAL" else "CRASHED") ++ "\n"
        ++ "-------------"
      where
        showCollections [] = "none"
        showCollections cs = intercalate ", " cs

instance Show Env where
    show e = show (arena e) ++ "\n" ++ show (robot e)

instance Show Program where
    show (Program initA decls initR stmts) =
        show initA ++ "\n"
        ++ showDecls decls
        ++ show initR ++ "\n"
        ++ showStmts stmts
      where
        showDecls [] = ""
        showDecls ds = unlines (map show ds)
        showStmts [] = ""
        showStmts ss = unlines (map show ss)

instance Show BoundsResult where
    show InBounds = "PASS — Robot is within arena bounds"
    show (OutOfBounds msg) = "FAIL — " ++ msg

instance Show CollectionResult where
    show AllCollected = "PASS — All items collected"
    show (ItemsRemaining remaining) =
        "INCOMPLETE — Items still in arena: " ++ intercalate ", " remaining

printProgram :: Program -> String
printProgram = show

printEnv :: Env -> String
printEnv = show