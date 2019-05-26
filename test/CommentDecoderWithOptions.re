open CommentTypeWithOptions;

let rec decodeComment = json =>
    {
        commentPostId : json |> Json.Decode.field ("commentPostId"
                                                  ,Json.Decode.int),
        commentText : json |> Json.Decode.field ("commentText"
                                                ,Json.Decode.string),
        commentMainCategories : json |> Json.Decode.field ("commentMainCategories"
                                                          ,Json.Decode.tuple2(Json.Decode.string
                                                                             ,Json.Decode.string)),
        commentPublished : json |> Json.Decode.field ("commentPublished"
                                                     ,Json.Decode.bool),
        commentCreated : json |> Json.Decode.field ("commentCreated"
                                                   ,Json.Decode.date),
        commentTags : json |> Json.Decode.field ("commentTags"
                                                ,Json.Decode.map(l => List.fold_left ((m,(k,v)) => Map_651798451719185100.add(k,v,m)
                                                                                     ,Map_651798451719185100.empty
                                                                                     ,l)
                                                                ,Json.Decode.list(Json.Decode.tuple2(Json.Decode.string,Json.Decode.int)))),}
