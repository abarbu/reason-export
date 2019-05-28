open PositionType;

let rec encodePosition  = (x : position) =>
    switch(x) {
        | Beginning => Json.Encode.object_([( "tag", Json.Encode.string("Beginning") )])
        | Middle => Json.Encode.object_([( "tag", Json.Encode.string("Middle") )])
        | End => Json.Encode.object_([( "tag", Json.Encode.string("End") )])}
