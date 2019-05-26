module Map_651798451719185100 = Map.Make({ type t = string; let compare = compare });

type comment =
    { postId : int
    , text : string
    , mainCategories : (string, string)
    , published : bool
    , created : Js.Date.t
    , tags : Map_651798451719185100.t (int)
    }
