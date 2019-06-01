open PositionType;

let rec encodePosition  = (x : position) =>
    switch(x) {
        | Beginning => Json.Encode.string("Beginning")
        | Middle => Json.Encode.string("Middle")
        | End => Json.Encode.string("End")}
