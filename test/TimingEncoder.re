open TimingType;

let rec encodeTiming  = (x : timing) =>
    switch(x) {
        | Start =>
            Json.Encode.object_
                ([( "tag", Json.Encode.string("Start") )])

        | Continue(arg0) =>
            Json.Encode.object_
                ([( "tag", Json.Encode.string("Continue") ),
                  ("contents", Json.Encode.float(arg0))])
        | Continue2(arg0,arg1) =>
            Json.Encode.object_
                ([( "tag", Json.Encode.string("Continue2") ),
                  ("contents", Json.Encode.jsonArray([|
                    Json.Encode.float(arg0),
                    Json.Encode.float(arg1)
                  |]))])

        | Continue3({c3a,c3b}) =>
            Json.Encode.object_
                ([( "tag", "Continue3" ),
                  ("c3a", Json.Encode.float(c3a)),
                  ("c3b", Json.Encode.float(c3b))])

        | Stop =>
            Json.Encode.object_
                ([( "tag", Json.Encode.string("Stop") )])}
