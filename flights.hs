{-# LANGUAGE FlexibleInstances #-}

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

--
-- Representing a Row of seats in a Flight
--

class Row r where
  newRow :: r
  showRow :: r -> String
  -- used to book seats in a row
  updateRow :: r -> Char -> TicketRef -> Maybe r

type Row2 = (Seat, Seat)
type Row2x2 = (Seat, Seat, Seat, Seat)

instance Row Row2 where
  newRow = (newSeat, newSeat)
  showRow (s1, s2) = "O " ++ showSeat s1 ++ " " ++ showSeat s2 ++ " O\n"

  updateRow (Nothing, s2) 'A' tr = return (return tr, s2)
  updateRow (Just s1s, _) 'A' _ = Nothing
  updateRow (s1, Nothing) 'B' tr = return (s1, return tr)
  updateRow (_, Just s2s) 'B' _ = Nothing

instance Row Row2x2 where
  newRow = (newSeat, newSeat, newSeat, newSeat)
  showRow _ = "O _ _ || _ _ O\n"

  updateRow _ _ _ = Nothing

--
-- Representing a single seat in a flight
--
type Seat = Maybe Booking
type Booking = String

newSeat :: Seat
newSeat = Nothing

type SeatNumber = (Int, Char)

showSeat :: Seat -> String
showSeat Nothing = "_"
showSeat _ = "X"

--
-- Ticket
--

type TicketRef = String

--
-- Business logic / interaction
--

newFlight :: FlightType -> String -> Flight
newFlight TypeCessna172 name = Cessna172 (name, newRow :: Row2)
newFlight TypeBombardierQ400 name = BombardierQ400 (name, replicate 19 (newRow :: Row2x2))

-- Book particular seat
bookSeat :: SeatNumber -> TicketRef -> Flight -> Maybe Flight
bookSeat (1, offset) tr (Cessna172 (n, row)) = fmap (\r -> Cessna172 (n, r)) $ updateRow row offset tr
-- there is only a single row on the Cessna 172
bookSeat (_, _) _ (Cessna172 _) = Nothing
-- booking not supported for the Bombardier Q400 yet
bookSeat _ _ _ = Nothing

--
-- Usuage
--

-- Buy a new flight!
-- let a = newFlight TypeCessna172 "Bird"
-- Book yourself and a friend a seat!
-- return a >>= bookSeat (1, 'A') "me" >>= bookSeat (1, 'B') "my friend"
-- Try booking invalid seats, you'll get back Nothing
