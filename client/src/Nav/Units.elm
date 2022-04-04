module Nav.Units exposing (..)

type Meters = Meters Float

getMeters : Meters -> Float
getMeters (Meters x) = x

type Rad = Rad Float

getRad : Rad -> Float
getRad (Rad x) = x

type Deg = Deg Float

getDeg : Deg -> Float
getDeg (Deg x) = x 

degToRad : Deg -> Rad
degToRad (Deg x) = 
  Rad (degrees x)

radToDeg : Rad -> Deg
radToDeg (Rad x) =
  Deg (x * 180.0/pi)
