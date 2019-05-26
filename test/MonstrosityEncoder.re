open MonstrosityType;

let rec encodeMonstrosity  = (x : monstrosity) =>
    switch(x) {
        | NotSpecial =>
            Json.Encode.object_
                ([( "type", Json.Encode.string("NotSpecial") ),])
        | OkayIGuess(arg0) =>
            Json.Encode.object_
                ([( "type", Json.Encode.string("OkayIGuess") ),
                    ( "arg0", encodeMonstrosity(arg0) ),])

        | Ridiculous(arg0,arg1,arg2) =>
            Json.Encode.object_
                ([( "type", Json.Encode.string("Ridiculous") ),
                    ( "arg0", Json.Encode.int(arg0) ),
                    ( "arg1", Json.Encode.string(arg1) ),
                    ( "arg2", (Json.Encode.list(encodeMonstrosity))(arg2) ),])}
