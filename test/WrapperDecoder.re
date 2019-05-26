open WrapperType;

let rec decodeWrapper = json =>
    Wrapper (json |> Json.Decode.field ("arg0"
                                       ,Json.Decode.int),)
