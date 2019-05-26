open CommentType;

let rec decodeComment = json =>
    {
        postId : json |> Json.Decode.field ("postId"
                                           ,Json.Decode.int),
        text : json |> Json.Decode.field ("text"
                                         ,Json.Decode.string),
        mainCategories : json |> Json.Decode.field ("mainCategories"
                                                   ,Json.Decode.tuple2(Json.Decode.string
                                                                      ,Json.Decode.string)),
        published : json |> Json.Decode.field ("published"
                                              ,Json.Decode.bool),
        created : json |> Json.Decode.field ("created"
                                            ,Json.Decode.date),
        tags : json |> Json.Decode.field ("tags"
                                         ,Json.Decode.map(l => List.fold_left ((m,(k,v)) => Map_651798451719185100.add(k,v,m)
                                                                              ,Map_651798451719185100.empty
                                                                              ,l)
                                                         ,Json.Decode.list(Json.Decode.tuple2(Json.Decode.string,Json.Decode.int)))),}
