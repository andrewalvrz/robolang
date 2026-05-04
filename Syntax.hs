module Syntax where
import System.Process (CreateProcess(env))

-- Context Free Grammar

{-

<program>    ::= <stmt_list>
<stmt_list>  ::= <stmt> | <stmt> "\n" <stmt_list>

<stmt>       ::= "init_arena" <number> <number>
               | "move" <number>
               | "turn" <angle>
               | "grab"
               | "check_status"

<angle>      ::= "90" | "180" | "270" | "0"
<number>     ::= <digit>+ [ "." <digit>+ ]
<digit>      ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

-}
type Position = (Double, Double)
data Arena = Arena {
    width :: Double,
    height :: Double,
    items  :: [(Position, String)]  -- List of items with their positions
} deriving(Eq)

data RobotState = RobotState {

    xPos :: Double,
    yPos :: Double,
    heading :: Double,
    capacity :: Int,
    collections :: [String],  -- List of collected items
    isalive :: Bool
} deriving(Eq)

-- type Env = RobotState
data Env = Env {
    robot :: RobotState,
    arena :: Arena
} deriving(Eq)
-- initialEnv = Env
--     {
--         robot = RobotState {
--             xPos = 0.0,
--             yPos = 0.0,
--             heading = 0.0,
--             capacity = 0,
--             collections = [],
--             isalive = False
--         },
--         arena = Arena {
--             width = 0.0,
--             height = 0.0,
--             items = []
--         }
--     }

data Program = Program InitArena [Decl] InitRobot [Stmt]
data InitArena = InitArena Double Double 
data InitRobot = Robot Position Double Int     -- Command to set robot's initial position, heading and capacity
data Decl =  Place Position String     -- Command to place an item at a position
-- 2. The Abstract Syntax (Statements)
-- Each constructor is a command the robot can execute.
data Stmt
    = Move Double             -- Command to move distance
    | Turn Double             -- Command to turn degrees
    | Grab                    -- Interaction command
    | CheckStatus             
    | UpdateCapity Int
    | IfCollected String [Stmt] [Stmt]  -- Conditional based on collected items
    | DisplayArena
    | DisplayRobot
    deriving (Eq)


evaluate :: Program -> Env
evaluate (Program initA decl initRobot stmts) = let env' = evaluateD decl (Env robot arena)
                                                    in evaluateSS stmts env'
    where
    initialize :: InitArena -> Arena
    initialize (InitArena w h) = Arena w h []

    arena = initialize initA

    robotC :: InitRobot -> RobotState
    robotC (Robot (x, y) h c) = RobotState x y h c [] True

    robot = robotC initRobot


evaluateD :: [Decl] -> Env -> Env
evaluateD [] env = env
evaluateD ((Place (x, y) s) : decls) env = undefined

evaluateSS :: [Stmt] -> Env -> Env
evaluateSS ss env = env




instance Show Stmt where
    show (InitArena w h) = "init_arena " ++ show w ++ " " ++ show h
    show (Move d)        = "move " ++ show d
    show (Turn a)        = "turn " ++ show a
    show Grab            = "grab"
    show CheckStatus     = "check_status"

instance Show RobotState where
    show s = "--- ROBOT STATUS ---\n" ++
             "Position : (" ++ show (xPos s) ++ ", " ++ show (yPos s) ++ ")\n" ++
             "Heading  : " ++ show (heading s) ++ " degrees\n" ++
             "Arena    : " ++ show (arenaWidth s) ++ "x" ++ show (arenaHeight s) ++ "\n" ++
             "Status   : " ++ (if isalive s then "FUNCTIONAL" else "CRASHED") ++
             "\n--------------------"

-- 3. The Program
-- A program is a functional sequence of these statements.
type Program = [Stmt]

-- 4. Helper for the check-off
printProg :: Program -> String
printProg = unlines . map show

