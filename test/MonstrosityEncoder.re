open MonstrosityType;

let rec encodeMonstrosity  = (x : monstrosity) =>
    switch(x) {
        | NotSpecial =>
            Json.Encode.object_
                ([( "tag", Json.Encode.string("NotSpecial") )])
        | OkayIGuess(arg0) =>
            Json.Encode.object_
                ([( "tag", Json.Encode.string("OkayIGuess") ),
                  ("contents", encodeMonstrosity(arg0))])

        | Ridiculous(arg0,arg1,arg2) =>
            Json.Encode.object_
                ([( "tag", Json.Encode.string("Ridiculous") ),
                  ("contents", Json.Encode.jsonArray([|
                    Json.Encode.int(arg0),
                    Json.Encode.string(arg1),
                    (Json.Encode.list(encodeMonstrosity))(arg2)
                  |]))])}
