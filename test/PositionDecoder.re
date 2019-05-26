open PositionType;

let rec decodePosition = json =>
    
        json |> (Json.Decode.field("type", Json.Decode.string)
            |> Json.Decode.andThen
            ((x) => switch(x)
            {
                | "Beginning" => json =>  Beginning
                | "Middle" => json =>  Middle
                | "End" => json =>  End
                | _ => failwith("unknown constructor")}))
