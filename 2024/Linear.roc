module [V2, zero, taxiCab, up, left, down, right, M2x2]

N : I32

# Vector

V2 : {
    x : N,
    y : N,
}

zero : V2
zero = { x: 0, y: 0 }

taxiCab : V2, V2 -> N
taxiCab = \{ x: x1, y: y1 }, { x: x2, y: y2 } ->
    Num.abs (y2 - y1) + Num.abs (x2 - x1)

left : V2 -> V2
left = \p -> { p & x: p.x - 1 }

right : V2 -> V2
right = \p -> { p & x: p.x + 1 }

up : V2 -> V2
up = \p -> { p & y: p.y - 1 }

down : V2 -> V2
down = \p -> { p & y: p.y + 1 }

maxx : List V2 -> V2

# Matrix

M2x2 : {
    m11 : N,
    m12 : N,
    m21 : N,
    m22 : N,
}

transpose : M2x2 -> M2x2
