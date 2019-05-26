open PostTypeWithOptions;
open CommentDecoder;

let rec decodePost = json =>
    {
        postId : json |> Json.Decode.field ("postId"
                                           ,Json.Decode.int),
        postName : json |> Json.Decode.field ("postName"
                                             ,Json.Decode.string),
        postAge : json |> Json.Decode.field ("postAge"
                                            ,Json.Decode.optional(Json.Decode.float)),
        postComments : json |> Json.Decode.field ("postComments"
                                                 ,Json.Decode.list(decodeComment)),
        postPromoted : json |> Json.Decode.field ("postPromoted"
                                                 ,Json.Decode.optional(decodeComment)),
        postAuthor : json |> Json.Decode.field ("postAuthor"
                                               ,Json.Decode.optional(Json.Decode.string)),}
