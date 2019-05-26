open PostType;
open CommentDecoder;

let rec decodePost = json =>
    {
        id : json |> Json.Decode.field ("id"
                                       ,Json.Decode.int),
        name : json |> Json.Decode.field ("name"
                                         ,Json.Decode.string),
        age : json |> Json.Decode.field ("age"
                                        ,Json.Decode.optional(Json.Decode.float)),
        comments : json |> Json.Decode.field ("comments"
                                             ,Json.Decode.list(decodeComment)),
        promoted : json |> Json.Decode.field ("promoted"
                                             ,Json.Decode.optional(decodeComment)),
        author : json |> Json.Decode.field ("author"
                                           ,Json.Decode.optional(Json.Decode.string)),}
