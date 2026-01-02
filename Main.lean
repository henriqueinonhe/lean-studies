structure Vec where
  x : Float
  y : Float

def Vec.magnitude (self : Vec) : Float :=
  Float.sqrt (self.x ^ 2 + self.y ^ 2)

def Vec.theta (self : Vec) : Float :=
  Float.atan (self.y / self.x)

def Vec.opposite (self : Vec) : Vec :=
  { x := -self.x, y := -self.y }

def Vec.scale (self : Vec) (k : Float) : Vec :=
  { x := k * self.x, y := k * self.y }

def Vec.rotate (self : Vec) (angle : Float) : Vec :=
  let theta := self.theta
  { x := Float.cos (theta + angle), y := Float.sin (theta + angle) : Vec}.scale self.magnitude

def vecAdd (p1 : Vec) (p2 : Vec) : Vec :=
  { x := p1.x + p2.x, y := p1.y + p2.y }

def vecSub (p1 : Vec) (p2 : Vec) : Vec :=
  vecAdd p1 p2.opposite

def distance (p1 : Vec) (p2 : Vec) : Float :=
  (vecSub p2 p1).magnitude

def dot (p1 : Vec) (p2 : Vec) : Float :=
  (p1.x * p2.x) + (p1.y * p2.y)

def angle (p1 : Vec) (p2 : Vec) : Float :=
  Float.acos ((dot p1 p2) / (p1.magnitude * p2.magnitude))

def PI := (Float.acos 0) * 2

def radToDegree (rad : Float) : Float :=
  360 * rad / (2 * PI)

def degreeToRad (degree : Float) : Float :=
  (degree * 2 * PI) / 360

def p1 : Vec :=
  { x := 2, y := 2 }

def p2 : Vec :=
  { x := 1, y:= 0 }

#eval p1.rotate (PI)
