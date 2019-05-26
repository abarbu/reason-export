open PostTypeWithOptions;
open CommentEncoder;

let rec encodePost  = (x : post) =>
    Json.Encode.object_
        ([ ( "postId", Json.Encode.int(x.postId) )
        , ( "postName", Json.Encode.string(x.postName) )
        , ( "postAge", (Json.Encode.nullable(Json.Encode.float))(x.postAge) )
        , ( "postComments", (Json.Encode.list(encodeComment))(x.postComments) )
        , ( "postPromoted", (Json.Encode.nullable(encodeComment))(x.postPromoted) )
        , ( "postAuthor", (Json.Encode.nullable(Json.Encode.string))(x.postAuthor) )
        ])
