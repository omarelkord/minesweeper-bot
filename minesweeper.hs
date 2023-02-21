{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

--hi amora

type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show, Eq)

up :: MyState -> MyState
up (S (x,y) cells string prevState)
    | x==0 = Null
    | otherwise = S (x-1,y) cells "up" (S (x,y) cells string prevState)

down :: MyState -> MyState
down (S (x,y) cells string prevState)
    | x==4 = Null
    | otherwise = S (x+1,y) cells "down" (S (x,y) cells string prevState)

left :: MyState -> MyState
left (S (x,y) cells string prevState)
    | y==0 = Null
    | otherwise = S (x,y-1) cells "left" (S (x,y) cells string prevState)

right :: MyState -> MyState
right (S (x,y) cells string prevState)
    | y==4 = Null
    | otherwise = S (x,y+1) cells "right" (S (x,y) cells string prevState)

remove x list = filter (/=x) list

collect :: MyState -> MyState
collect (S cell cellList string prevState)
    | remove cell cellList == cellList = Null
    | otherwise = S cell (remove cell cellList) "collect" (S cell cellList string prevState)

nextMyStates :: MyState -> [MyState]
nextMyStates myState = remove Null [up myState, down myState, left myState, right myState, collect myState]

isGoal :: MyState -> Bool
isGoal (S cell cellList string prevState) = null cellList

search :: [MyState] -> MyState
search (x:xs)
    | isGoal x = x
    | otherwise = search (xs ++ nextMyStates x)

constructSolution :: MyState -> [String]
constructSolution Null = []
constructSolution (S cell cellList string prevState) 
    | string == "" = constructSolution prevState
    | otherwise = constructSolution prevState ++ [string]

solve cell cellList = constructSolution (search [initialState]) where initialState = S cell cellList "" Null
