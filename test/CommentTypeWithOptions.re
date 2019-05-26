module Map_651798451719185100 = Map.Make({ type t = string; let compare = compare });

type comment =
    { commentPostId : int
    , commentText : string
    , commentMainCategories : (string, string)
    , commentPublished : bool
    , commentCreated : Js.Date.t
    , commentTags : Map_651798451719185100.t (int)
    }
