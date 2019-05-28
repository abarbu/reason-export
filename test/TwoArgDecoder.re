open TwoArgType;

let rec decodeTwoArg = json =>
    (((arg0,arg1)) => TwoArg(arg0
                            ,arg1))(json |> Json.Decode.tuple2(Json.Decode.string
                                                              ,Json.Decode.float))
