open UselessType;

let rec decodeUseless = json =>
    json |> Json.Decode.nullAs(Useless)
