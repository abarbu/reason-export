open ShadowingType;

let rec encodeShadowing  = (x : shadowing) =>
    Json.Encode.object_
        ([ ( "prop", Json.Encode.tuple2(Json.Encode.tuple2(Json.Encode.int
                                                          ,Json.Encode.int)
                                       ,Json.Encode.tuple2(Json.Encode.string
                                                          ,Json.Encode.string))(x.prop) )
        ])
