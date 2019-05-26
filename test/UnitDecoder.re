open UnitType;

let rec decodeUnit = json =>
    json |> Json.Decode.nullAs(Unit)
