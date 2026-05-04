module Semantics where

import Syntax
import Debug.Trace (trace)

evaluate :: Program -> Env
evaluate (Program initA decls initR stmts) =
    let arenaVal = initArena initA
        robotVal = initRobot initR
        startEnv = Env robotVal arenaVal
        envWithItems = evaluateDecls decls startEnv
    in  evaluateStmts stmts envWithItems

initArena :: InitArena -> Arena
initArena (InitArena w h) = Arena w h []

initRobot :: InitRobot -> RobotState
initRobot (Robot (x, y) h c) = RobotState x y h c [] True

evaluateDecls :: [Decl] -> Env -> Env
evaluateDecls [] env = env
evaluateDecls (d:ds) env =
    let env' = evaluateDecl d env
    in  evaluateDecls ds env'

evaluateDecl :: Decl -> Env -> Env
evaluateDecl (Place pos name) env =
    let currentArena = arena env
        currentItems = items currentArena
        newItems = (pos, name) : currentItems
        newArena = currentArena { items = newItems }
    in  env { arena = newArena }

evaluateStmts :: [Stmt] -> Env -> Env
evaluateStmts [] env = env
evaluateStmts (s:ss) env =
    let env' = evaluateStmt s env
    in  evaluateStmts ss env'

evaluateStmt :: Stmt -> Env -> Env

evaluateStmt (Move d) env =
    let r       = robot env
        rad     = heading r * pi / 180
        newX    = xPos r + d * cos rad
        newY    = yPos r + d * sin rad
        newRobot = r { xPos = newX, yPos = newY }
    in  env { robot = newRobot }

evaluateStmt (Turn a) env =
    let r = robot env
        newRobot = r { heading = heading r + a }
    in  env { robot = newRobot }

evaluateStmt Grab env =
    let r = robot env
        a = arena env
        robotPos = (xPos r, yPos r)
    in  case findNearestItem robotPos (items a) of
            Nothing -> trace "GRAB: No items within reach" env
            Just (pos, name) ->
                if length (collections r) >= capacity r
                then trace "GRAB: Robot is at full capacity" env
                else
                    let newCollections = collections r ++ [name]
                        newRobot = r { collections = newCollections }
                        newItems = removeItem pos name (items a)
                        newArena = a { items = newItems }
                    in  Env newRobot newArena

evaluateStmt CheckStatus env =
    trace ("\n" ++ show (robot env)) env

evaluateStmt (UpdateCapacity n) env =
    let r = robot env
        newRobot = r { capacity = n }
    in  env { robot = newRobot }

evaluateStmt (IfCollected item thenBranch elseBranch) env =
    let r = robot env
    in  if elem item (collections r)
        then evaluateStmts thenBranch env
        else evaluateStmts elseBranch env

evaluateStmt DisplayArena env =
    trace ("\n" ++ show (arena env)) env

evaluateStmt DisplayRobot env =
    trace ("\n" ++ show (robot env)) env

distance :: Position -> Position -> Double
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))

findNearestItem :: Position -> [(Position, String)] -> Maybe (Position, String)
findNearestItem _ [] = Nothing
findNearestItem robotPos ((pos, name):rest) =
    if distance robotPos pos <= 1.0
    then Just (pos, name)
    else findNearestItem robotPos rest

removeItem :: Position -> String -> [(Position, String)] -> [(Position, String)]
removeItem pos name itemList =
    filter (\(p, n) -> not (p == pos && n == name)) itemList

boundsCheck :: Env -> BoundsResult
boundsCheck env =
    let r = robot env
        a = arena env
        x = xPos r
        y = yPos r
        w = width a
        h = height a
    in  if x >= 0 && x <= w && y >= 0 && y <= h
        then InBounds
        else OutOfBounds ("Robot at (" ++ show x ++ ", " ++ show y
                          ++ ") is outside arena bounds (0,0)-("
                          ++ show w ++ ", " ++ show h ++ ")")

collectionCheck :: Env -> CollectionResult
collectionCheck env =
    let a = arena env
        remaining = items a
    in  case remaining of
            [] -> AllCollected
            _  ->
                let names = map (\(_, name) -> name) remaining
                in  ItemsRemaining names

runChecks :: Env -> String
runChecks env =
    "\n=== SEMANTIC CHECKS ===\n"
    ++ "Bounds check     : " ++ show (boundsCheck env) ++ "\n"
    ++ "Collection check : " ++ show (collectionCheck env) ++ "\n"
    ++ "========================"