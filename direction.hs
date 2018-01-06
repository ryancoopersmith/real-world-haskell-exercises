data Direction = LeftTurn | RightTurn | StraightTurn | None
    deriving (Show)

pythagorean :: ((Int,Int),(Int,Int)) -> Double
pythagorean  ((x1,y1),(x2,y2)) = sqrt (((abs (x2 - x1)) ** 2) + (abs (y2 - y1)) ** 2)

getLargestAngle :: (Double,Double,Double) -> Double
getLargestAngle (a,b,c) = acos ((sqrt a) + (sqrt b) - (sqrt c)) / (2 * a * b)

-- NOTE: not finished
whichDirection :: ((Int,Int),(Int,Int),(Int,Int)) -> Direction
whichDirection ((x1,y1),(x2,y2),(x3,y3))
| abLength == 0 || bcLength == 0 = None -- one or more segments have no length so None
| (x1 == x2 && x2 == x3) || (y1 == y2 && y2 == y3) = StraightTurn -- either horizontal or vertical straight line so StraightTurn
| abLength == bcLength && bcLength == acLength = -- we've got an equilateral
| otherwise = if acLength > abLength && acLength > bcLength
              then largestAngle
              else -- use law of sines here
where abLength = pythagorean ((x1,y1),(x2,y2))
      bcLength = pythagorean ((x2,y2),(x3,y3))
      acLength = pythagorean ((x1,y1),(x3,y3))
      largestAngle = getLargestAngle abLength bcLength acLength
