module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / orbitalTime planet

earthPeriod :: Float
earthPeriod = 3155760

orbitalTime :: Planet -> Float
orbitalTime Mercury = earthPeriod * 0.2408467
orbitalTime Venus = earthPeriod * 0.61519726
orbitalTime Earth = earthPeriod
orbitalTime Mars = earthPeriod * 1.8808158
orbitalTime Jupiter = earthPeriod * 11.862615
orbitalTime Saturn = earthPeriod * 29.447498
orbitalTime Uranus = earthPeriod * 84.016846
orbitalTime Neptune = earthPeriod * 164.79132
