open MonstrosityType;

let rec decodeMonstrosity = json =>
    
        json |> (Json.Decode.field("type", Json.Decode.string)
            |> Json.Decode.andThen
            ((x) => switch(x)
            {
                | "NotSpecial" => json =>  NotSpecial
                | "OkayIGuess" => json =>  OkayIGuess(json |> Json.Decode.field("arg0"
                                                                               ,decodeMonstrosity))
                | "Ridiculous" => json =>  Ridiculous(json |> Json.Decode.field ("arg0"
                                                                                ,Json.Decode.int),
                json |> Json.Decode.field ("arg1"
                                          ,Json.Decode.string),
                json |> Json.Decode.field ("arg2"
                                          ,Json.Decode.list(decodeMonstrosity)),)
                | _ => failwith("unknown constructor")}))
