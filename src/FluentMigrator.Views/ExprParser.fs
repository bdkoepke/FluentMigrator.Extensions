namespace FluentMigrator

module ExprParser =
    open FParsec
    open FluentMigrator.Views.Expr

    let private ws = spaces

    let private token (parser: Parser<'a, unit>) : Parser<'a, unit> = parser .>> ws

    let private isIdentifierStart c =
        System.Char.IsLetter c || c = '_' || c = '$'

    let private isIdentifierChar c =
        System.Char.IsLetterOrDigit c || c = '_' || c = '$'

    let private plainIdentifier =
        (satisfy isIdentifierStart .>>. manyChars (satisfy isIdentifierChar))
        |>> fun (head, tail) -> string head + tail

    let private quotedIdentifier =
        between (pchar '"') (pchar '"') (manyChars (noneOf "\"" <|> (pstring "\"\"" >>% '"')))
        |>> fun name -> "\"" + name.Replace("\"", "\"\"") + "\""

    let private identifier = token (quotedIdentifier <|> plainIdentifier)

    let private keyword (word: string) =
        token (attempt (pstringCI word .>> notFollowedBy (satisfy isIdentifierChar)))

    let private nameParts =
        pipe2 identifier (many (attempt (pchar '.' >>. ws >>. identifier))) (fun head tail -> head :: tail)

    let private numberLiteral =
        let digits = many1Chars (satisfy System.Char.IsDigit)

        token (
            pipe2 digits (opt (pchar '.' >>. digits)) (fun whole fraction ->
                match fraction with
                | None -> Int(int64 whole)
                | Some decimals ->
                    Decimal(
                        System.Decimal.Parse(whole + "." + decimals, System.Globalization.CultureInfo.InvariantCulture)
                    ))
        )

    let private stringLiteral =
        let escapedQuote = pstring "''" >>% '\''

        token (between (pchar '\'') (pchar '\'') (manyChars (noneOf "'" <|> escapedQuote)))
        |>> String

    let private tableStar =
        attempt (
            pipe2 nameParts (pchar '.' >>. ws >>. pchar '*' .>> ws) (fun parts _ -> Star(Some(String.concat "." parts)))
        )

    let private pExpr, pExprRef = createParserForwardedToRef<Expr, unit>()

    let private functionArgs =
        between (token (pchar '(')) (token (pchar ')')) (sepBy pExpr (token (pchar ',')))

    let private direction = (keyword "ASC" >>% Asc) <|> (keyword "DESC" >>% Desc)

    let private orderItem =
        pipe2 pExpr (opt direction) (fun expr direction -> expr, defaultArg direction Asc)

    let private windowSpec =
        between
            (token (pchar '('))
            (token (pchar ')'))
            (pipe2
                (opt (keyword "PARTITION" >>. keyword "BY" >>. sepBy1 pExpr (token (pchar ','))))
                (opt (keyword "ORDER" >>. keyword "BY" >>. sepBy1 orderItem (token (pchar ','))))
                (fun partitionBy orderBy ->
                    {
                        PartitionBy = defaultArg partitionBy []
                        OrderBy = defaultArg orderBy []
                    }))

    let private functionCall =
        attempt (
            pipe3 nameParts functionArgs (opt (keyword "OVER" >>. windowSpec)) (fun name args window ->
                let name = String.concat "." name
                Function(name, args, window))
        )

    let private caseExpression =
        pipe3
            (keyword "CASE"
             >>. many1 (
                 pipe2 (keyword "WHEN" >>. pExpr) (keyword "THEN" >>. pExpr) (fun condition result -> condition, result)
             ))
            (opt (keyword "ELSE" >>. pExpr))
            (keyword "END")
            (fun cases elseExpr _ -> Case(cases, elseExpr))

    let private columnExpr =
        nameParts
        |>> function
            | [ name ] -> Column(None, name)
            | parts ->
                let table = parts |> List.take (parts.Length - 1) |> String.concat "."
                Column(Some table, List.last parts)

    let private postfixOperator =
        choice
            [
                attempt (
                    keyword "IS" >>. keyword "NOT" >>. keyword "NULL"
                    >>% fun expr -> Unary("IS NOT NULL", expr)
                )
                attempt (keyword "IS" >>. keyword "NULL" >>% fun expr -> Unary("IS NULL", expr))
            ]

    let private term =
        choice
            [
                between (token (pchar '(')) (token (pchar ')')) pExpr
                caseExpression
                keyword "NULL" >>% Null
                keyword "TRUE" >>% Bool true
                keyword "FALSE" >>% Bool false
                numberLiteral
                stringLiteral
                attempt tableStar
                token (pchar '*') >>% Star None
                attempt functionCall
                columnExpr
            ]

    let private primary =
        term .>>. many postfixOperator
        |>> fun (expr, operators) -> operators |> List.fold (fun expr op -> op expr) expr

    let private unary, unaryRef = createParserForwardedToRef<Expr, unit>()

    unaryRef.Value <-
        choice
            [
                attempt (keyword "NOT" >>. unary |>> fun expr -> Unary("NOT", expr))
                attempt (token (pchar '-') >>. unary |>> fun expr -> Unary("-", expr))
                primary
            ]

    let private multiplicativeOperator =
        choice
            [
                token (pchar '*') >>% fun left right -> Binary(left, "*", right)
                token (pchar '/') >>% fun left right -> Binary(left, "/", right)
                token (pchar '%') >>% fun left right -> Binary(left, "%", right)
            ]

    let private additiveOperator =
        choice
            [
                attempt (token (pstring "||")) >>% fun left right -> Binary(left, "||", right)
                token (pchar '+') >>% fun left right -> Binary(left, "+", right)
                token (pchar '-') >>% fun left right -> Binary(left, "-", right)
            ]

    let private comparisonOperator =
        choice
            [
                attempt (token (pstring "<>")) >>% fun left right -> Binary(left, "<>", right)
                attempt (token (pstring "!=")) >>% fun left right -> Binary(left, "!=", right)
                attempt (token (pstring "<=")) >>% fun left right -> Binary(left, "<=", right)
                attempt (token (pstring ">=")) >>% fun left right -> Binary(left, ">=", right)
                token (pstring "=") >>% fun left right -> Binary(left, "=", right)
                token (pstring "<") >>% fun left right -> Binary(left, "<", right)
                token (pstring ">") >>% fun left right -> Binary(left, ">", right)
                keyword "LIKE" >>% fun left right -> Binary(left, "LIKE", right)
            ]

    let private andOperator =
        keyword "AND" >>% fun left right -> Binary(left, "AND", right)

    let private orOperator =
        keyword "OR" >>% fun left right -> Binary(left, "OR", right)

    let private multiplicative = chainl1 unary multiplicativeOperator
    let private additive = chainl1 multiplicative additiveOperator
    let private comparison = chainl1 additive comparisonOperator
    let private conjunction = chainl1 comparison andOperator
    let private disjunction = chainl1 conjunction orOperator

    pExprRef.Value <- disjunction .>> ws

    let tryParse (text: string) =
        match run (ws >>. pExpr .>> eof) text with
        | Success(expr, _, _) -> Result.Ok expr
        | Failure(message, _, _) -> Result.Error message

    let parse (text: string) =
        match tryParse text with
        | Result.Ok expr -> expr
        | Result.Error message -> failwith $"Invalid SQL expression '{text}': {message}"
