open PositionType;
module Map_651798451719185100 = Map.Make({ type t = string; let compare = compare });

type favoritePlaces =
    { positionsByUser : Map_651798451719185100.t (list (position))
    }
