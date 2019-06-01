open TimingType;

let rec decodeTiming = json =>
    
        json |> (Json.Decode.field("tag", Json.Decode.string)
            |> Json.Decode.andThen
            ((x) => switch(x)
            {
                | "Start" => json => Start
                | "Continue" => json => Continue(json |> Json.Decode.field("contents", Json.Decode.float))
                | "Continue2" => json => (((arg0
                                           ,arg1)) => Continue2(arg0
                                                               ,arg1))(json |> Json.Decode.tuple2(Json.Decode.float
                                                                                                 ,Json.Decode.float))
                | "Continue3" => json => Continue3({c3a : json |> Json.Decode.field ("c3a"
                                                                                    ,Json.Decode.float),
                                                  c3b : json |> Json.Decode.field ("c3b"
                                                                                  ,Json.Decode.float),})
                | "Stop" => json => Stop
                | _ => failwith("unknown constructor")}))
