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
        let mutable sourceColumn = "value"
        let mutable columns: Column list = []
        let expression = ExecuteSqlStatementExpression()

        do
            context.Expressions.Add(expression)
            this.UpdateSql()

        member private this.UpdateSql() =
            let def =
                {
                    Name = name
                    SourceTable = sourceTable
                    SourceColumn = sourceColumn
                    Columns = List.rev columns
                }

            expression.SqlStatement <- toSql def

        member this.AddColumn(col: Column) =
            columns <- col :: columns
            this.UpdateSql()

        member this.ReplaceLastColumn(col: Column) =
            match columns with
            | _ :: tail ->
                columns <- col :: tail
                this.UpdateSql()
            | [] ->
                columns <- [ col ]
                this.UpdateSql()

        member this.WithSourceColumn(sourceCol: string) =
            sourceColumn <- sourceCol
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

    type CreateRoot(context: IMigrationContext, viewName: string) =
        member _.OnTable(sourceTable: string) = Builder(context, viewName, sourceTable)

    type DeleteViewExpressionBuilder(expression: ExecuteSqlStatementExpression, viewName: string) as this =
        inherit ExpressionBuilderBase<ExecuteSqlStatementExpression>(expression)

        let mutable ifExists = true
        let mutable schema: string option = None

        do this.UpdateSql()

        member private this.UpdateSql() =
            let ifExistsClause = if ifExists then "IF EXISTS " else ""

            let qualifiedName =
                match schema with
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
                schema <- Some schemaName
                this.UpdateSql()

        interface IIfExistsOrInSchemaSyntax with
            member this.IfExists() =
                ifExists <- true
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
