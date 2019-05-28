open TwoArgType;

let rec encodeTwoArg  = (x : twoArg) =>
    switch(x) {
        | TwoArg(arg0,arg1) =>
          Json.Encode.jsonArray([|
            Json.Encode.string(arg0),
            Json.Encode.float(arg1)
          |])}
