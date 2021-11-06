module Main where

import Flow

import Control.Lens hiding ((.>), (|>))

import Day12 hiding (main)
import Day12 qualified

rotate :: Relative -> Location -> Location
rotate r l = Location (s * l^.y) (s * negate (l^.x))
  where
    s = relative2int r

data ProblemState = ProblemState
  { _ship :: Location
  , _flag :: Location
  }
makeLenses ''ProblemState
deriving instance Show ProblemState

step :: Command -> ProblemState -> ProblemState
step (Command (Move c) n) s = s |> flag %~ updateLocation c n
step (Command (Turn r) n) s = s |> flag %~ iterate (rotate r) .> (!! n)
step (Command F        n) s = s |> ship +~ fromIntegral n * s^.flag

initialState :: ProblemState
initialState = ProblemState
  { _ship = Location
    { _x = 0
    , _y = 0
    }
  , _flag = Location
    { _x = 10
    , _y = 1
    }
  }

main :: IO ()
main = Day12.main step initialState (view ship)
