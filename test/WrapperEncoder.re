open WrapperType;

let rec encodeWrapper  = (x : wrapper) =>
    switch(x) {
        | Wrapper(y0) => Json.Encode.int(y0)}
