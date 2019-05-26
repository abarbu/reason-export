open CommentType

type post =
    { id : int
    , name : string
    , age : option (float)
    , comments : list (comment)
    , promoted : option (comment)
    , author : option (string)
    }
