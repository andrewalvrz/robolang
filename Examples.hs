module Main where

import Semantics
import Syntax
import Debug.Trace (trace)

printP :: Program -> IO ()
printP = putStrLn . printProgram

printE :: Env -> IO ()
printE = putStrLn . printEnv

evaluator :: Program -> IO ()
evaluator = putStrLn . printEnv . evaluate

arena1 :: InitArena
arena1 = InitArena 10.0 10.0

d1 :: Decl
d1 = Place (2.0, 0.0) "Wrench"

d2 :: Decl
d2 = Place (2.0, 3.0) "Battery"

d3 :: Decl
d3 = Place (7.0, 0.0) "Chip"

robot1 :: InitRobot
robot1 = Robot (0.0, 0.0) 0.0 3

s1 :: Stmt
s1 = DisplayArena

s2 :: Stmt
s2 = DisplayRobot

s3 :: Stmt
s3 = Move 2.0

s4 :: Stmt
s4 = Grab

s5 :: Stmt
s5 = DisplayRobot

s6 :: Stmt
s6 = Turn 90.0

s7 :: Stmt
s7 = Move 3.0

s8 :: Stmt
s8 = Grab

s9 :: Stmt
s9 = DisplayRobot

s10 :: Stmt
s10 = DisplayArena

example1 :: Program
example1 = Program arena1 [d1, d2, d3] robot1
    [s1, s2, s3, s4, s5, s6, s7, s8, s9, s10]

arena2 :: InitArena
arena2 = InitArena 5.0 5.0

d4 :: Decl
d4 = Place (1.0, 0.0) "Key"

robot2 :: InitRobot
robot2 = Robot (0.0, 0.0) 0.0 2

s11 :: Stmt
s11 = DisplayArena

s12 :: Stmt
s12 = Move 1.0

s13 :: Stmt
s13 = Grab

s14 :: Stmt
s14 = IfCollected "Key"
    [DisplayRobot, CheckStatus]
    [Move 1.0, Grab]

s15 :: Stmt
s15 = CheckStatus

example2 :: Program
example2 = Program arena2 [d4] robot2
    [s11, s12, s13, s14, s15]

arena3 :: InitArena
arena3 = InitArena 5.0 5.0

robot3 :: InitRobot
robot3 = Robot (0.0, 0.0) 0.0 1

s16 :: Stmt
s16 = Move 20.0

example3 :: Program
example3 = Program arena3 [] robot3 [s16]

main :: IO ()
main = do
    putStrLn "============================================"
    putStrLn "   EXAMPLE 1: Movement and Grabbing"
    putStrLn "============================================"
    putStrLn ""
    putStrLn "Source program:"
    printP example1
    putStrLn "Evaluating..."
    let result1 = evaluate example1
    result1 `seq` putStrLn ""
    putStrLn (runChecks result1)

    putStrLn ""
    putStrLn "============================================"
    putStrLn "   EXAMPLE 2: Conditional Branching"
    putStrLn "============================================"
    putStrLn ""
    putStrLn "Source program:"
    printP example2
    putStrLn "Evaluating..."
    let result2 = evaluate example2
    result2 `seq` putStrLn ""
    putStrLn (runChecks result2)

    putStrLn ""
    putStrLn "============================================"
    putStrLn "   EXAMPLE 3: Out of Bounds"
    putStrLn "============================================"
    putStrLn ""
    putStrLn "Source program:"
    printP example3
    putStrLn "Evaluating..."
    let result3 = evaluate example3
    result3 `seq` putStrLn ""
    putStrLn (runChecks result3)