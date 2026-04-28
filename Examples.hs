module Examples where
import Syntax

-- Program 1: The Standard Square
-- Shows the robot navigating a 10x10 space.
p1 :: Program
p1 = [ InitArena 10.0 10.0
     , Move 5.0
     , Turn 90.0
     , Move 5.0
     ]

-- Program 2: The 'Crash' Test

p2 :: Program
p2 = [ InitArena 10.0 10.0
     , Move 15.0  
     ]

-- Program 3: Search and Rescue
p3 :: Program
p3 = [ InitArena 20.0 20.0
     , Move 10.0
     , Grab
     , Turn 180.0
     , Move 10.0
     ]

p4 :: Program
p4 = [ InitArena 10.0 10.0
     , Move 5.0
     ]    