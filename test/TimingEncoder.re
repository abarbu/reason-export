open TimingType;

let rec encodeTiming  = (x : timing) =>
    switch(x) {
        | Start =>
            Json.Encode.object_
                ([( "type", Json.Encode.string("Start") ),])
        | Continue(arg0) =>
            Json.Encode.object_
                ([( "type", Json.Encode.string("Continue") ),
                    ( "arg0", Json.Encode.float(arg0) ),])

        | Stop =>
            Json.Encode.object_
                ([( "type", Json.Encode.string("Stop") ),])}