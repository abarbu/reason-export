open PostType;
open CommentEncoder;

let rec encodePost  = (x : post) =>
    Json.Encode.object_
        ([ ( "id", Json.Encode.int(x.id) )
        , ( "name", Json.Encode.string(x.name) )
        , ( "age", (Json.Encode.nullable(Json.Encode.float))(x.age) )
        , ( "comments", (Json.Encode.list(encodeComment))(x.comments) )
        , ( "promoted", (Json.Encode.nullable(encodeComment))(x.promoted) )
        , ( "author", (Json.Encode.nullable(Json.Encode.string))(x.author) )
        ])
