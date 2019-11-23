--
-- Flight data structure
--

-- Purely used to construct Flights
data FlightType = TypeBombardierQ400
                | TypeCessna172

-- Use Data.FixedList to restrict the number of rows here itself
type FlightName = String
data Flight = BombardierQ400 (FlightName, [Row2x2])
            | Cessna172 (FlightName, Row2)

instance Show Flight where
  show (Cessna172 (name, r)) = name ++ " -> Cessna 172\n" ++ showRow r
  show (BombardierQ400 (name, rs)) = name ++ " -> Bombardier Q400\n"
                                     ++ foldl (\acc r -> acc ++ showRow r) "" rs

newFlight :: FlightType -> String -> Flight
newFlight TypeCessna172 name = Cessna172 (name, newRow :: Row2)
newFlight TypeBombardierQ400 name = BombardierQ400 (name, replicate 19 (newRow :: Row2x2))
--
-- Representing a Row of seats in a Flight
--

class Row r where
  newRow :: r
  showRow :: r -> String

newtype Row2 = Row2 (Seat, Seat)
newtype Row2x2 = Row2x2 (Seat, Seat, Seat, Seat)

instance Row Row2 where
  newRow = Row2 (newSeat, newSeat)
  showRow _ = "O _ _ O\n"
instance Row Row2x2 where
  newRow = Row2x2 (newSeat, newSeat, newSeat, newSeat)
  showRow _ = "O _ _ || _ _ O\n"

--
-- Representing a single seat in a flight
--
type Seat = Maybe Booking
type Booking = String

newSeat :: Seat
newSeat = Nothing
