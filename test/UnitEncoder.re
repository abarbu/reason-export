open UnitType;

let rec encodeUnit  = (x : unit) =>
    Json.Encode.null
