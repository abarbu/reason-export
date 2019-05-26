open CommentType;

type post =
    { postId : int
    , postName : string
    , postAge : option (float)
    , postComments : list (comment)
    , postPromoted : option (comment)
    , postAuthor : option (string)
    }
