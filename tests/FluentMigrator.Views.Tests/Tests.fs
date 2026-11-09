namespace FluentMigrator.Views.Tests

open System
open Xunit
open FluentMigrator
open FluentMigrator.Builders
open FluentMigrator.Builders.Create
open FluentMigrator.Builders.Delete
open FluentMigrator.Expressions
open FluentMigrator.Infrastructure
open FluentMigrator.Views

module Helpers =
    let context () =
        MigrationContext(
            Unchecked.defaultof<IQuerySchema>,
            Unchecked.defaultof<IServiceProvider>,
            Unchecked.defaultof<string>
        )
        :> IMigrationContext

    let statements (ctx: IMigrationContext) =
        ctx.Expressions
        |> Seq.map (fun expression -> (expression :?> ExecuteSqlStatementExpression).SqlStatement)
        |> Seq.toList

    let lastStatement ctx = statements ctx |> List.last

module Sql =
    [<Fact>]
    let ``toSql generates a simple JSON view`` () =
        let view =
            {
                Name = "active_users"
                SourceTable = "users"
                SourceColumn = "value"
                Columns = [ Json("id", "$.id") ]
            }

        let expected =
            String.concat
                "\n"
                [
                    "CREATE VIEW active_users ("
                    "    id"
                    ") AS"
                    "SELECT"
                    "    value::json->>'id'"
                    "FROM users;"
                ]

        Assert.Equal(expected, toSql view)

    [<Fact>]
    let ``toSql generates nested paths and casts in column order`` () =
        let view =
            {
                Name = "user_summary"
                SourceTable = "users"
                SourceColumn = "document"
                Columns =
                    [
                        Json("id", "$.id")
                        Json("city", "$.address.city")
                        JsonCast("age", "$.age", "integer")
                    ]
            }

        let expected =
            String.concat
                "\n"
                [
                    "CREATE VIEW user_summary ("
                    "    id,"
                    "    city,"
                    "    age"
                    ") AS"
                    "SELECT"
                    "    document::json->>'id',"
                    "    json_extract_path_text(document::json, 'address', 'city'),"
                    "    CAST(document::json->>'age' AS integer)"
                    "FROM users;"
                ]

        Assert.Equal(expected, toSql view)

    [<Fact>]
    let ``toSql accepts paths without a JSON root prefix`` () =
        let view =
            {
                Name = "names"
                SourceTable = "people"
                SourceColumn = "payload"
                Columns = [ Json("name", "name") ]
            }

        Assert.Contains("payload::json->>'name'", toSql view)
        Assert.Contains("FROM people;", toSql view)

    [<Fact>]
    let ``dropSql drops a view when it exists`` () =
        Assert.Equal("DROP VIEW IF EXISTS active_users;", dropSql "active_users")

module Builder =
    [<Fact>]
    let ``builder records a single expression and preserves column order`` () =
        let ctx = Helpers.context ()
        let builder = Builder(ctx, "active_users", "users")

        builder.WithColumn("id").AsJson("$.id").WithColumn("city").AsJson("$.address.city").AsType("text")
        |> ignore

        let expected =
            String.concat
                "\n"
                [
                    "CREATE VIEW active_users ("
                    "    id,"
                    "    city"
                    ") AS"
                    "SELECT"
                    "    value::json->>'id',"
                    "    CAST(json_extract_path_text(value::json, 'address', 'city') AS text)"
                    "FROM users;"
                ]

        Assert.Equal(1, Helpers.statements ctx |> List.length)
        Assert.Equal(expected, Helpers.lastStatement ctx)

    [<Fact>]
    let ``builder changes the source column used for extraction`` () =
        let ctx = Helpers.context ()
        let builder = Builder(ctx, "user_names", "users")

        builder.WithColumn("name").AsJson("$.name") |> ignore
        builder.WithSourceColumn("payload") |> ignore

        Assert.Contains("payload::json->>'name'", Helpers.lastStatement ctx)

    [<Fact>]
    let ``builder can change the source column through the column syntax`` () =
        let ctx = Helpers.context ()
        let builder = Builder(ctx, "user_names", "users")

        builder.WithColumn("name").AsJson("$.name").WithSourceColumn("payload")
        |> ignore

        Assert.Contains("payload::json->>'name'", Helpers.lastStatement ctx)

    [<Fact>]
    let ``create root starts a builder for a source table`` () =
        let ctx = Helpers.context ()
        let builder = CreateRoot(ctx, "user_names").OnTable("users")

        builder.WithColumn("name").AsJson("$.name") |> ignore

        Assert.Contains("CREATE VIEW user_names (", Helpers.lastStatement ctx)
        Assert.Contains("FROM users;", Helpers.lastStatement ctx)

    [<Fact>]
    let ``create extension is available from the create expression root`` () =
        let ctx = Helpers.context ()
        let builder = CreateExpressionRoot(ctx).View("user_names").OnTable("users")

        builder.WithColumn("name").AsJson("$.name") |> ignore

        Assert.Equal(1, Helpers.statements ctx |> List.length)
        Assert.Contains("CREATE VIEW user_names (", Helpers.lastStatement ctx)

module Delete =
    [<Fact>]
    let ``delete builder drops a view if it exists by default`` () =
        let ctx = Helpers.context ()
        let expression = ExecuteSqlStatementExpression()
        ctx.Expressions.Add(expression)

        DeleteViewExpressionBuilder(expression, "active_users") |> ignore

        Assert.Equal("DROP VIEW IF EXISTS active_users;", expression.SqlStatement)

    [<Fact>]
    let ``delete builder supports a schema`` () =
        let ctx = Helpers.context ()
        let expression = ExecuteSqlStatementExpression()
        ctx.Expressions.Add(expression)

        let builder = DeleteViewExpressionBuilder(expression, "active_users")
        (builder :> IInSchemaSyntax).InSchema("reporting")

        Assert.Equal("DROP VIEW IF EXISTS reporting.active_users;", expression.SqlStatement)

    [<Fact>]
    let ``delete extension configures if-exists and schema`` () =
        let ctx = Helpers.context ()
        let deletion = DeleteExpressionRoot(ctx).View("active_users")

        deletion.IfExists().InSchema("reporting")

        Assert.Equal(1, Helpers.statements ctx |> List.length)
        Assert.Equal("DROP VIEW IF EXISTS reporting.active_users;", Helpers.lastStatement ctx)
