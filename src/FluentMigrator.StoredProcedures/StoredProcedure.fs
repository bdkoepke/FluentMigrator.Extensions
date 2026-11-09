namespace FluentMigrator

open FluentMigrator.Builders
open FluentMigrator.Builders.Create
open FluentMigrator.Builders.Delete
open FluentMigrator.Expressions
open FluentMigrator.Infrastructure

module StoredProcedures =
    type ParameterDirection =
        | In
        | Out
        | InOut
        | Variadic

    type Parameter =
        {
            Name: string
            DataType: string
            Direction: ParameterDirection
            DefaultValue: string option
        }

    type Definition =
        {
            Name: string
            Schema: string option
            OrReplace: bool
            Parameters: Parameter list
            Language: string
            Body: string
        }

    let private create name =
        {
            Name = name
            Schema = None
            OrReplace = false
            Parameters = []
            Language = "plpgsql"
            Body = ""
        }

    let toSql (def: Definition) =
        let orReplaceClause = if def.OrReplace then "OR REPLACE " else ""

        let qualifiedName =
            match def.Schema with
            | Some s when not (System.String.IsNullOrWhiteSpace s) -> $"{s}.{def.Name}"
            | _ -> def.Name

        let formatParam (p: Parameter) =
            let dirStr =
                match p.Direction with
                | In -> "IN "
                | Out -> "OUT "
                | InOut -> "INOUT "
                | Variadic -> "VARIADIC "

            let defaultStr =
                match p.DefaultValue with
                | Some v when not (System.String.IsNullOrWhiteSpace v) -> $" DEFAULT {v}"
                | _ -> ""

            $"    {dirStr}{p.Name} {p.DataType}{defaultStr}"

        let paramsBlock =
            if def.Parameters.IsEmpty then
                "()"
            else
                let formatted = def.Parameters |> List.map formatParam |> String.concat ",\n"
                $"(\n{formatted}\n)"

        let body =
            if System.String.IsNullOrWhiteSpace def.Body then
                ""
            else
                def.Body

        $"CREATE {orReplaceClause}PROCEDURE {qualifiedName} {paramsBlock}\nLANGUAGE {def.Language}\nAS $$\n{body}\n$$;"

    type DropOptions =
        {
            Name: string
            Schema: string option
            IfExists: bool
            ParameterTypes: string list
            Cascade: bool
        }

    let dropSql (options: DropOptions) =
        let ifExistsClause = if options.IfExists then "IF EXISTS " else ""

        let qualifiedName =
            match options.Schema with
            | Some s when not (System.String.IsNullOrWhiteSpace s) -> $"{s}.{options.Name}"
            | _ -> options.Name

        let typesBlock =
            if options.ParameterTypes.IsEmpty then
                ""
            else
                let typesStr = options.ParameterTypes |> String.concat ", "
                $"({typesStr})"

        let cascadeClause = if options.Cascade then " CASCADE" else ""

        $"DROP PROCEDURE {ifExistsClause}{qualifiedName}{typesBlock}{cascadeClause};"

    type Builder(context: IMigrationContext, name: string) as this =
        inherit ExpressionBuilderBase<ExecuteSqlStatementExpression>(ExecuteSqlStatementExpression())

        let mutable schema: string option = None
        let mutable orReplace = false
        let mutable parameters: Parameter list = []
        let mutable language = "plpgsql"
        let mutable body = ""

        do
            context.Expressions.Add(this.Expression)
            this.UpdateSql()

        member private this.UpdateSql() =
            let def =
                {
                    Name = name
                    Schema = schema
                    OrReplace = orReplace
                    Parameters = List.rev parameters
                    Language = language
                    Body = body
                }

            this.Expression.SqlStatement <- toSql def

        member this.InSchema(schemaName: string) =
            schema <- Some schemaName
            this.UpdateSql()
            this

        member this.OrReplace() =
            orReplace <- true
            this.UpdateSql()
            this

        member this.WithParameter(paramName: string, dataType: string, ?direction: ParameterDirection) =
            let dir = defaultArg direction In

            let p =
                {
                    Name = paramName
                    DataType = dataType
                    Direction = dir
                    DefaultValue = None
                }

            parameters <- p :: parameters
            this.UpdateSql()
            this

        member this.WithInOutParameter(paramName: string, dataType: string) =
            this.WithParameter(paramName, dataType, InOut)

        member this.WithOutParameter(paramName: string, dataType: string) =
            this.WithParameter(paramName, dataType, Out)

        member this.WithDefaultValue(defaultValueExpr: string) =
            match parameters with
            | p :: tail ->
                parameters <-
                    { p with
                        DefaultValue = Some defaultValueExpr
                    }
                    :: tail

                this.UpdateSql()
            | [] -> ()

            this

        member this.Language(lang: string) =
            language <- lang
            this.UpdateSql()
            this

        member this.AsBody(sqlBody: string) =
            body <- sqlBody
            this.UpdateSql()
            this

        interface IFluentSyntax with
            member this.GetType() = base.GetType()
            member this.GetHashCode() = base.GetHashCode()
            member this.ToString() = base.ToString()
            member this.Equals(other) = base.Equals(other)

    type CreateRoot(context: IMigrationContext, name: string) =
        inherit Builder(context, name)

    type DeleteProcedureExpressionBuilder(expression: ExecuteSqlStatementExpression, procName: string) as this =
        inherit ExpressionBuilderBase<ExecuteSqlStatementExpression>(expression)

        let mutable ifExists = true
        let mutable schema: string option = None
        let mutable parameterTypes: string list = []
        let mutable cascade = false

        do this.UpdateSql()

        member private this.UpdateSql() =
            let options =
                {
                    Name = procName
                    Schema = schema
                    IfExists = ifExists
                    ParameterTypes = parameterTypes
                    Cascade = cascade
                }

            this.Expression.SqlStatement <- dropSql options

        member this.IfExists() =
            ifExists <- true
            this.UpdateSql()
            this

        member this.InSchema(schemaName: string) =
            schema <- Some schemaName
            this.UpdateSql()
            this

        member this.WithParameterTypes(types: string list) =
            parameterTypes <- types
            this.UpdateSql()
            this

        member this.Cascade() =
            cascade <- true
            this.UpdateSql()
            this

        member this.Restrict() =
            cascade <- false
            this.UpdateSql()
            this

        interface IFluentSyntax with
            member this.GetType() = base.GetType()
            member this.GetHashCode() = base.GetHashCode()
            member this.ToString() = base.ToString()
            member this.Equals(other) = base.Equals(other)

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
        member this.StoredProcedure(name: string) =
            let context = getContext this
            CreateRoot(context, name)

    type IDeleteExpressionRoot with
        member this.StoredProcedure(name: string) : DeleteProcedureExpressionBuilder =
            let context = getContext this
            let expression = ExecuteSqlStatementExpression()
            context.Expressions.Add(expression)
            DeleteProcedureExpressionBuilder(expression, name)
