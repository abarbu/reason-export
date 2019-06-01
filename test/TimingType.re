type timing =
    | Start
    | Continue(float)
    | Continue2(float, float)
    | Continue3 { c3a : float
                , c3b : float
                }
    | Stop
