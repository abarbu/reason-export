open ShadowingType;

let rec decodeShadowing = json =>
    {
        prop : json |> Json.Decode.field ("prop"
                                         ,Json.Decode.tuple2(Json.Decode.tuple2(Json.Decode.int
                                                                               ,Json.Decode.int)
                                                            ,Json.Decode.tuple2(Json.Decode.string
                                                                               ,Json.Decode.string))),}
