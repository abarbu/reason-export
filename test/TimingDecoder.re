open TimingType;

let rec decodeTiming = json =>
    
        json |> (Json.Decode.field("type", Json.Decode.string)
            |> Json.Decode.andThen
            ((x) => switch(x)
            {
                | "Start" => json =>  Start
                | "Continue" => json =>  Continue(json |> Json.Decode.field("arg0"
                                                                           ,Json.Decode.float))
                | "Stop" => json =>  Stop
                | _ => failwith("unknown constructor")}))
