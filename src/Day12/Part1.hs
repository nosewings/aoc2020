module Main where

import Flow
import Control.Lens hiding ((.>), (|>))

import Day12 hiding (main)
import Day12 qualified

updateDirection :: Relative -> Int -> Cardinal -> Cardinal
updateDirection r n = fromEnum .> (+ relative2int r * n) .> (`mod` 4) .> toEnum

data ShipState = ShipState
  { _direction :: Cardinal
  , _location :: Location
  }
makeLenses ''ShipState
deriving instance Show ShipState

step :: Command -> ShipState -> ShipState
step (Command (Move c) n) s = s |> location  %~ updateLocation  c             n
step (Command (Turn r) n) s = s |> direction %~ updateDirection r             n
step (Command F        n) s = s |> location  %~ updateLocation (s^.direction) n

initialState :: ShipState
initialState = ShipState
  { _direction = E
  , _location = Location
    { _x = 0
    , _y = 0
    }
  }

main :: IO ()
main = Day12.main step initialState (view location)
