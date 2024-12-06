interface Day03.Schematic
  exposes [Schematic, getChunks]
  imports [Matrix.{ Matrix, Coord }]


Schematic : Matrix Str
Cell : [Connected Coord Str, Isolated Coord Str, Star Coord, Symbol, Dot, Newline]
Chunk : [Noise (List Coord) Str, Part (List Coord) Str, Gear Coord, Symbol, Other]

symbols = Str.graphemes "-/@=$%&*#+"

getCells : Schematic -> List Cell
getCells = \schematic ->
  width = Matrix.getWidth schematic
  height = Matrix.getHeight schematic

  flattenedSchematic =
    schematic
      |> List.intersperse ["\n"]
      |> List.join

  List.range { start: At 0, end: Length ((width + 1) * height - 1) }
    |> List.map2 flattenedSchematic \i, grapheme ->
        row = i // (width + 1)
        col = i % (width + 1)
        when grapheme is
          "\n" -> Newline
          "." -> Dot
          "*" -> Star (row, col)
          _ if symbols |> List.contains grapheme ->
            Symbol
          _ if grapheme |> Str.toNat |> Result.isOk ->
            neighborGraphemes =
              rowOff <- List.joinMap (List.range { start: At (if row == 0 then 0 else -1), end: At 1 })
              colOff <- List.joinMap (List.range { start: At (if col == 0 then 0 else -1), end: At 1 })
              tooCloseToHome = rowOff == 0 && colOff == 0
              {} <- List.joinMap (if tooCloseToHome then [] else [{}])
              Ok schematic
                |> Result.try \rows -> List.get rows (Num.toNat (Num.toI32 row + rowOff))
                |> Result.try \line -> List.get line (Num.toNat (Num.toI32 col + colOff))
                |> Result.map List.single
                |> Result.withDefault []
            if neighborGraphemes |> List.any \g -> symbols |> List.contains g then
              Connected (row, col) grapheme
            else
              Isolated (row, col) grapheme
          _ ->
            dbg grapheme
            crash "these should be the only options for grapheme"

getChunks : Schematic -> List Chunk
getChunks = \schematic ->
  chunks, cell <- List.walk (getCells schematic) [Other]
  replaceLast : List elem, elem -> List elem
  replaceLast = \list, elem ->
    list |> List.dropLast 1 |> List.append elem
  when cell is
    Isolated coord digit ->
      when chunks is
        [.., Noise coords num] ->
          chunks |> replaceLast (Noise (List.append coords coord) (Str.concat num digit))
        [.., Part coords num] ->
          chunks |> replaceLast (Part (List.append coords coord) (Str.concat num digit))
        [.., Other] ->
          chunks |> List.append (Noise [coord] digit)
        [.., Gear _] | [.., Symbol] ->
          chunks |> List.append (Part [coord] digit)
        [] -> chunks

    Connected coord digit ->
      when chunks is
        [.., Noise coords num] | [.., Part coords num] ->
          chunks |> replaceLast (Part (List.append coords coord) (Str.concat num digit))
        [.., Other] | [.., Gear _] | [.., Symbol] ->
          chunks |> List.append (Part [coord] digit)
        [] -> chunks

    Star coord ->
      when chunks is
        [.., Other] ->
          chunks |> List.append (Gear coord)
        [.., Noise _ _] ->
          chunks |> replaceLast Other |> List.append (Gear coord)
        [.., Part _ _] | [.., Gear _] | [.., Symbol] ->
          chunks |> List.append (Gear coord)
        [] -> chunks

    Symbol ->
      when chunks is
        [.., Noise coords num] ->
          chunks |> replaceLast (Part coords num) |> List.append Symbol
        [] | [.., Part _ _] | [.., Gear _] | [.., Symbol] | [.., Other] -> chunks |> List.append Symbol

    Dot ->
      when chunks is
        [] ->
          [Other]
        [.., Other] ->
          chunks
        [.., Part _ _] | [.., Gear _] | [.., Symbol] ->
          chunks |> List.append Other
        [.., Noise _ _] ->
          chunks |> replaceLast Other

    Newline ->
      when chunks is
        [.., Noise _ _] ->
          chunks |> replaceLast Other
        [.., Part _ _] ->
          chunks |> List.append Other
        [] | [.., Gear _] | [.., Symbol] | [.., Other] -> chunks
