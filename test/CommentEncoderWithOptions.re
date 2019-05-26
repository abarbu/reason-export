open CommentTypeWithOptions;

let rec encodeComment  = (x : comment) =>
    Json.Encode.object_
        ([ ( "commentPostId", Json.Encode.int(x.commentPostId) )
        , ( "commentText", Json.Encode.string(x.commentText) )
        , ( "commentMainCategories", Json.Encode.tuple2(Json.Encode.string
                                                       ,Json.Encode.string)(x.commentMainCategories) )
        , ( "commentPublished", Json.Encode.bool(x.commentPublished) )
        , ( "commentCreated", Json.Encode.date(x.commentCreated) )
        , ( "commentTags", ((x) => Json.Encode.list(Json.Encode.tuple2(Json.Encode.string
                                                                      ,Json.Encode.int))(Map_651798451719185100.bindings(x)))(x.commentTags) )
        ])
