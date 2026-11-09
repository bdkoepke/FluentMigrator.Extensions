namespace FluentMigrator

open FluentMigrator.Builders
open FluentMigrator.Builders.Create
open FluentMigrator.Builders.Delete
open FluentMigrator.Expressions
open FluentMigrator.Infrastructure

module Views =
    type Column =
        | Json of columnName: string * jsonPath: string
        | JsonCast of columnName: string * jsonPath: string * castType: string

    type Definition =
        {
            Name: string
            SourceTable: string
            SourceColumn: string
            Columns: Column list
        }

    let private create name sourceTable =
        {
            Name = name
            SourceTable = sourceTable
            SourceColumn = "value"
            Columns = []
        }

    let toSql (view: Definition) =
        let colHeaders =
            view.Columns
            |> List.map (function
                | Json(name, _)
                | JsonCast(name, _, _) -> $"    {name}")
            |> String.concat ",\n"

        let extractExpr (path: string) =
            let cleanPath =
                if path.StartsWith("$.") then path.Substring(2)
                elif path.StartsWith("$") then path.Substring(1)
                else path

            let (segments: string array) = cleanPath.Split('.')

            if segments.Length = 1 then
                $"{view.SourceColumn}::json->>'{segments[0]}'"
            else
                let segs = segments |> Array.map (fun s -> $"'{s}'") |> String.concat ", "
                $"json_extract_path_text({view.SourceColumn}::json, {segs})"

        let selectExpressions =
            view.Columns
            |> List.map (function
                | Json(_, path) -> $"    {extractExpr path}"
                | JsonCast(_, path, castType) -> $"    CAST({extractExpr path} AS {castType})")
            |> String.concat ",\n"

        $"CREATE VIEW {view.Name} (\n{colHeaders}\n) AS\nSELECT\n{selectExpressions}\nFROM {view.SourceTable};"

    let dropSql viewName = $"DROP VIEW IF EXISTS {viewName};"

    type Builder(context: IMigrationContext, name: string, sourceTable: string) as this =
        let state = ref (create name sourceTable)
        let expression = ExecuteSqlStatementExpression()

        do
            context.Expressions.Add(expression)
            this.UpdateSql()

        member private this.UpdateSql() =
            expression.SqlStatement <-
                toSql
                    { state.Value with
                        Columns = List.rev state.Value.Columns
                    }

        member this.AddColumn(col: Column) =
            state.Value <-
                { state.Value with
                    Columns = col :: state.Value.Columns
                }

            this.UpdateSql()

        member this.ReplaceLastColumn(col: Column) =
            match state.Value.Columns with
            | _ :: tail ->
                state.Value <-
                    { state.Value with
                        Columns = col :: tail
                    }
            | [] -> state.Value <- { state.Value with Columns = [ col ] }

            this.UpdateSql()

        member this.WithSourceColumn(sourceCol: string) =
            state.Value <-
                { state.Value with
                    SourceColumn = sourceCol
                }

            this.UpdateSql()
            this

        member this.WithColumn(columnName: string) = ColumnSyntax(this, columnName)

    and ColumnSyntax(builder: Builder, columnName: string) =
        member _.AsJson(jsonPath: string) =
            ColumnAsTypeSyntax(builder, columnName, jsonPath)

    and ColumnAsTypeSyntax(builder: Builder, columnName: string, jsonPath: string) as this =
        do builder.AddColumn(Json(columnName, jsonPath))

        member _.AsType(castType: string) =
            builder.ReplaceLastColumn(JsonCast(columnName, jsonPath, castType))
            this

        member _.WithColumn(columnName: string) = builder.WithColumn(columnName)

        member _.WithSourceColumn(sourceCol: string) =
            builder.WithSourceColumn(sourceCol) |> ignore
            this

    module Expr =
        type Direction =
            | Asc
            | Desc

        type Expr =
            | Column of table: string option * name: string
            | Star of table: string option
            | Int of int64
            | Decimal of decimal
            | String of string
            | Bool of bool
            | Null
            | Raw of string
            | Binary of left: Expr * op: string * right: Expr
            | Unary of op: string * expr: Expr
            | Function of name: string * args: Expr list * window: WindowSpec option
            | Case of cases: (Expr * Expr) list * elseExpr: Expr option

        and WindowSpec =
            {
                PartitionBy: Expr list
                OrderBy: (Expr * Direction) list
            }

        let rec toSql (expr: Expr) =
            match expr with
            | Column(None, name) -> name
            | Column(Some table, name) -> $"{table}.{name}"
            | Star None -> "*"
            | Star(Some table) -> $"{table}.*"
            | Int value -> value.ToString()
            | Decimal value -> value.ToString(System.Globalization.CultureInfo.InvariantCulture)
            | String value -> "'" + value.Replace("'", "''") + "'"
            | Bool true -> "TRUE"
            | Bool false -> "FALSE"
            | Null -> "NULL"
            | Raw sql -> sql
            | Binary(left, op, right) -> $"({toSql left} {op} {toSql right})"
            | Unary("NOT", expr) -> $"(NOT {toSql expr})"
            | Unary("-", expr) -> $"(-{toSql expr})"
            | Unary(op, expr) -> $"({toSql expr} {op})"
            | Function(name, args, None) ->
                let argSql = args |> List.map toSql |> String.concat ", "
                $"{name}({argSql})"
            | Function(name, args, Some window) ->
                let argSql = args |> List.map toSql |> String.concat ", "

                let partitionSql =
                    if window.PartitionBy.IsEmpty then
                        ""
                    else
                        "PARTITION BY " + (window.PartitionBy |> List.map toSql |> String.concat ", ")

                let orderSql =
                    if window.OrderBy.IsEmpty then
                        ""
                    else
                        "ORDER BY "
                        + (window.OrderBy
                           |> List.map (fun (expr, direction) ->
                               let dir =
                                   match direction with
                                   | Asc -> "ASC"
                                   | Desc -> "DESC"

                               $"{toSql expr} {dir}")
                           |> String.concat ", ")

                let spec =
                    [ partitionSql; orderSql ]
                    |> List.filter (fun sql -> not (System.String.IsNullOrWhiteSpace sql))
                    |> String.concat " "

                $"{name}({argSql}) OVER ({spec})"
            | Case(cases, elseExpr) ->
                let caseSql =
                    cases
                    |> List.map (fun (condition, result) -> $"    WHEN {toSql condition} THEN {toSql result}")
                    |> String.concat "\n"

                let elseSql =
                    match elseExpr with
                    | Some expr -> $"\n    ELSE {toSql expr}"
                    | None -> ""

                $"CASE\n{caseSql}{elseSql}\nEND"

        let col name = Column(None, name)
        let colOf table name = Column(Some table, name)
        let star = Star None
        let starOf table = Star(Some table)
        let int value = Int(int64 value)
        let decimal value = Decimal value
        let str value = String value
        let bool value = Bool value
        let nullValue = Null
        let raw sql = Raw sql

        let binary left op right = Binary(left, op, right)
        let add left right = Binary(left, "+", right)
        let subtract left right = Binary(left, "-", right)
        let multiply left right = Binary(left, "*", right)
        let divide left right = Binary(left, "/", right)
        let eq left right = Binary(left, "=", right)
        let neq left right = Binary(left, "<>", right)
        let lt left right = Binary(left, "<", right)
        let lte left right = Binary(left, "<=", right)
        let gt left right = Binary(left, ">", right)
        let gte left right = Binary(left, ">=", right)
        let andAlso left right = Binary(left, "AND", right)
        let orElse left right = Binary(left, "OR", right)
        let isNull expr = Unary("IS NULL", expr)
        let isNotNull expr = Unary("IS NOT NULL", expr)

        let func name args = Function(name, args, None)
        let coalesce args = func "coalesce" args
        let sum expr = func "sum" [ expr ]
        let count expr = func "count" [ expr ]
        let countStar = func "count" [ star ]
        let min expr = func "min" [ expr ]
        let max expr = func "max" [ expr ]
        let abs expr = func "abs" [ expr ]

        let dateDiff part start finish =
            func "date_diff" [ str part; start; finish ]

        let over expr partitionBy orderBy =
            match expr with
            | Function(name, args, None) ->
                Function(
                    name,
                    args,
                    Some
                        {
                            PartitionBy = partitionBy
                            OrderBy = orderBy
                        }
                )
            | _ -> failwith "OVER can only be applied to a function call"

        let rowNumber partitionBy orderBy =
            over (func "row_number" []) partitionBy orderBy

        let denseRank partitionBy orderBy =
            over (func "dense_rank" []) partitionBy orderBy

        let lag expr partitionBy orderBy =
            over (func "lag" [ expr ]) partitionBy orderBy

        let caseOf cases elseExpr = Case(cases, elseExpr)

    let private indentSql (sql: string) =
        sql.Split('\n') |> Array.map (fun line -> "    " + line) |> String.concat "\n"

    type private RelationalState =
        {
            Ctes: (string * string) list
            Source: string option
            Joins: (string * string * Expr.Expr) list
            Columns: (string * Expr.Expr) list
            WhereClauses: Expr.Expr list
            GroupBy: Expr.Expr list
            Unions: string list
        }

    type RelationalBuilder(context: IMigrationContext option, viewName: string option, replace: bool) =
        let expression =
            match context with
            | Some _ -> Some(ExecuteSqlStatementExpression())
            | None -> None

        let state =
            ref
                {
                    Ctes = []
                    Source = None
                    Joins = []
                    Columns = []
                    WhereClauses = []
                    GroupBy = []
                    Unions = []
                }

        do
            match context, expression with
            | Some ctx, Some expr -> ctx.Expressions.Add(expr)
            | _ -> ()

        member private this.SelectSql(includeAliases: bool) =
            let current = state.Value

            let selectExpressions =
                current.Columns
                |> List.map (fun (name, expr) ->
                    let sql = Expr.toSql expr
                    let sql = if includeAliases then $"{sql} AS {name}" else sql
                    $"    {sql}")
                |> String.concat ",\n"

            let cteSql =
                current.Ctes
                |> List.map (fun (name, sql) -> $"{name} AS (\n{indentSql sql}\n)")
                |> String.concat ",\n"

            let withSql = if current.Ctes.IsEmpty then "" else $"WITH {cteSql}\n"

            let clauses =
                [
                    yield $"SELECT\n{selectExpressions}"

                    for sourceSql in Option.toList current.Source do
                        yield $"FROM {sourceSql}"

                    yield!
                        (current.Joins
                         |> List.map (fun (joinType, table, condition) ->
                             $"{joinType} {table} ON {Expr.toSql condition}"))

                    if not current.WhereClauses.IsEmpty then
                        yield
                            "WHERE "
                            + (current.WhereClauses |> List.map Expr.toSql |> String.concat " AND ")

                    if not current.GroupBy.IsEmpty then
                        yield "GROUP BY " + (current.GroupBy |> List.map Expr.toSql |> String.concat ", ")
                ]

            let baseSql = String.concat "\n" clauses

            let selectSql =
                if current.Unions.IsEmpty then
                    baseSql
                else
                    (baseSql :: current.Unions) |> String.concat "\nUNION\n"

            $"{withSql}{selectSql}"

        member private this.UpdateSql() =
            match expression, viewName with
            | Some expr, Some name ->
                let current = state.Value
                let verb = if replace then "CREATE OR REPLACE VIEW" else "CREATE VIEW"

                let colHeaders =
                    current.Columns
                    |> List.map (fun (name, _) -> $"    {name}")
                    |> String.concat ",\n"

                let selectSql = this.SelectSql(false)

                expr.SqlStatement <- $"{verb} {name} (\n{colHeaders}\n) AS\n{selectSql};"
            | _ -> ()

        member this.ToSql(includeAliases: bool) = this.SelectSql(includeAliases)

        member this.With(name: string, sql: string) =
            state.Value <-
                { state.Value with
                    Ctes = state.Value.Ctes @ [ name, sql ]
                }

            this.UpdateSql()
            this

        member this.With(name: string, build: RelationalBuilder -> RelationalBuilder) =
            let cte = RelationalBuilder(None, None, false)
            let built = build cte

            state.Value <-
                { state.Value with
                    Ctes = state.Value.Ctes @ [ name, built.ToSql(true) ]
                }

            this.UpdateSql()
            this

        member this.From(table: string, alias: string) =
            state.Value <-
                { state.Value with
                    Source = Some $"{table} {alias}"
                }

            this.UpdateSql()
            this

        member this.From(sourceSql: string) =
            state.Value <-
                { state.Value with
                    Source = Some sourceSql
                }

            this.UpdateSql()
            this

        member this.Join(table: string, alias: string, condition: Expr.Expr) =
            state.Value <-
                { state.Value with
                    Joins = state.Value.Joins @ [ "JOIN", $"{table} {alias}", condition ]
                }

            this.UpdateSql()
            this

        member this.Join(tableAndAlias: string, condition: string) =
            state.Value <-
                { state.Value with
                    Joins = state.Value.Joins @ [ "JOIN", tableAndAlias, Expr.raw condition ]
                }

            this.UpdateSql()
            this

        member this.LeftJoin(table: string, alias: string, condition: Expr.Expr) =
            state.Value <-
                { state.Value with
                    Joins = state.Value.Joins @ [ "LEFT JOIN", $"{table} {alias}", condition ]
                }

            this.UpdateSql()
            this

        member this.LeftJoin(tableAndAlias: string, condition: string) =
            state.Value <-
                { state.Value with
                    Joins = state.Value.Joins @ [ "LEFT JOIN", tableAndAlias, Expr.raw condition ]
                }

            this.UpdateSql()
            this

        member this.WithColumn(columnName: string) =
            RelationalColumnSyntax(this, columnName)

        member this.Where(condition: Expr.Expr) =
            state.Value <-
                { state.Value with
                    WhereClauses = state.Value.WhereClauses @ [ condition ]
                }

            this.UpdateSql()
            this

        member this.Where(condition: string) = this.Where(Expr.raw condition)

        member this.GroupBy(expressions: Expr.Expr list) =
            state.Value <-
                { state.Value with
                    GroupBy = state.Value.GroupBy @ expressions
                }

            this.UpdateSql()
            this

        member this.GroupBy(expressions: string) = this.GroupBy([ Expr.raw expressions ])

        member this.Union(build: RelationalBuilder -> RelationalBuilder) =
            let other = RelationalBuilder(None, None, false)
            let built = build other

            state.Value <-
                { state.Value with
                    Unions = state.Value.Unions @ [ built.ToSql(false) ]
                }

            this.UpdateSql()
            this

        member internal this.AddColumn(columnName: string, expr: Expr.Expr) =
            state.Value <-
                { state.Value with
                    Columns = state.Value.Columns @ [ columnName, expr ]
                }

            this.UpdateSql()

    and RelationalColumnSyntax(builder: RelationalBuilder, columnName: string) =
        member _.As(expr: Expr.Expr) =
            builder.AddColumn(columnName, expr)
            builder

        member _.As(sql: string) =
            builder.AddColumn(columnName, Expr.raw sql)
            builder

    type RawViewBuilder(context: IMigrationContext, viewName: string, replace: bool) =
        member _.As(selectSql: string) =
            let expression = ExecuteSqlStatementExpression()
            context.Expressions.Add(expression)
            let verb = if replace then "CREATE OR REPLACE VIEW" else "CREATE VIEW"

            expression.SqlStatement <- $"{verb} {viewName} AS {selectSql};"

        member _.From(sourceSql: string) =
            RelationalBuilder(Some context, Some viewName, replace).From(sourceSql)

        member _.From(table: string, alias: string) =
            RelationalBuilder(Some context, Some viewName, replace).From(table, alias)

        member _.With(name: string, sql: string) =
            RelationalBuilder(Some context, Some viewName, replace).With(name, sql)

        member _.With(name: string, build: RelationalBuilder -> RelationalBuilder) =
            RelationalBuilder(Some context, Some viewName, replace).With(name, build)

    type CreateRoot(context: IMigrationContext, viewName: string) =
        member _.OnTable(sourceTable: string) = Builder(context, viewName, sourceTable)

        member _.As(selectSql: string) =
            RawViewBuilder(context, viewName, false).As(selectSql)

        member _.From(sourceSql: string) =
            RawViewBuilder(context, viewName, false).From(sourceSql)

        member _.From(table: string, alias: string) =
            RawViewBuilder(context, viewName, false).From(table, alias)

        member _.With(name: string, sql: string) =
            RawViewBuilder(context, viewName, false).With(name, sql)

        member _.With(name: string, build: RelationalBuilder -> RelationalBuilder) =
            RawViewBuilder(context, viewName, false).With(name, build)

        member _.OrReplace() = RawViewBuilder(context, viewName, true)

    type DeleteViewExpressionBuilder(expression: ExecuteSqlStatementExpression, viewName: string) as this =
        inherit ExpressionBuilderBase<ExecuteSqlStatementExpression>(expression)

        let schema: string option ref = ref None

        do this.UpdateSql()

        member private this.UpdateSql() =
            let ifExistsClause = "IF EXISTS "

            let qualifiedName =
                match schema.Value with
                | Some s when not (System.String.IsNullOrWhiteSpace s) -> $"{s}.{viewName}"
                | _ -> viewName

            this.Expression.SqlStatement <- $"DROP VIEW {ifExistsClause}{qualifiedName};"

        interface IFluentSyntax with
            member this.GetType() = base.GetType()
            member this.GetHashCode() = base.GetHashCode()
            member this.ToString() = base.ToString()
            member this.Equals(other) = base.Equals(other)

        interface IInSchemaSyntax with
            member this.InSchema(schemaName: string) =
                schema.Value <- Some schemaName
                this.UpdateSql()

        interface IIfExistsOrInSchemaSyntax with
            member this.IfExists() =
                this.UpdateSql()
                this :> IInSchemaSyntax

    let private getContext (root: obj) : IMigrationContext =
        let bindingFlags =
            System.Reflection.BindingFlags.NonPublic
            ||| System.Reflection.BindingFlags.Instance

        let field = root.GetType().GetField("_context", bindingFlags)

        if isNull field then
            failwith "Unable to extract IMigrationContext from expression root"
        else
            field.GetValue(root) :?> IMigrationContext

    type ICreateExpressionRoot with
        member this.View(viewName: string) =
            let context = getContext this
            CreateRoot(context, viewName)

    type IDeleteExpressionRoot with
        member this.View(viewName: string) : IIfExistsOrInSchemaSyntax =
            let context = getContext this
            let expression = ExecuteSqlStatementExpression()
            context.Expressions.Add(expression)
            DeleteViewExpressionBuilder(expression, viewName) :> IIfExistsOrInSchemaSyntax
