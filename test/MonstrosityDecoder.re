open MonstrosityType;

let rec decodeMonstrosity = json =>
    
        json |> (Json.Decode.field("tag", Json.Decode.string)
            |> Json.Decode.andThen
            ((x) => switch(x)
            {
                | "NotSpecial" => json => NotSpecial
                | "OkayIGuess" => json => OkayIGuess(json |> Json.Decode.field("contents", decodeMonstrosity))
                | "Ridiculous" => json => (((arg0
                                            ,arg1)) => Ridiculous(arg0
                                                                 ,arg1
                                                                 ,arg2))(json |> Json.Decode.tuple3(Json.Decode.int
                                                                                                   ,Json.Decode.string
                                                                                                   ,Json.Decode.list(decodeMonstrosity)))
                | _ => failwith("unknown constructor")}))
