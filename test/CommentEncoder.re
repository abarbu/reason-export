open CommentType;

let rec encodeComment  = (x : comment) =>
    Json.Encode.object_
        ([ ( "postId", Json.Encode.int(x.postId) )
        , ( "text", Json.Encode.string(x.text) )
        , ( "mainCategories", Json.Encode.tuple2(Json.Encode.string
                                                ,Json.Encode.string)(x.mainCategories) )
        , ( "published", Json.Encode.bool(x.published) )
        , ( "created", Json.Encode.date(x.created) )
        , ( "tags", ((x) => Json.Encode.list(Json.Encode.tuple2(Json.Encode.string
                                                               ,Json.Encode.int))(Map_651798451719185100.bindings(x)))(x.tags) )
        ])
