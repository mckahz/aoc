interface Vec3
  exposes [Vec3, fromStr, add, sub, scale, abs]
  imports []

fromStr : Str -> Result Vec3 [NotAVec]
fromStr = \str ->
  nums =
    str
    |> Str.split ","
    |> List.map Str.trim
    |> List.keepOks Str.toF64

  when nums is
    [x, y, z] -> Ok { x, y, z }
    _ -> Err NotAVec

Coord: F64

Vec3 :
  { x: Coord
  , y: Coord
  , z: Coord
  }

abs : Vec3 -> Vec3
abs = \v ->
  { x: Num.abs v.x
  , y: Num.abs v.y
  , z: Num.abs v.z
  }

scale : Vec3, Coord -> Vec3
scale = \v, c ->
  { x: c * v.x
  , y: c * v.y
  , z: c * v.z
  }

add : Vec3, Vec3 -> Vec3
add = \v1, v2 ->
  { x: v1.x + v2.x
  , y: v1.y + v2.y
  , z: v1.z + v2.z
  }

sub : Vec3, Vec3 -> Vec3
sub = \v1, v2 ->
  { x: v1.x - v2.x
  , y: v1.y - v2.y
  , z: v1.z - v2.z
  }
