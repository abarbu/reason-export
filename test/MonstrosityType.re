type monstrosity =
    | NotSpecial
    | OkayIGuess(monstrosity)
    | Ridiculous(int, string, (list (monstrosity)))
