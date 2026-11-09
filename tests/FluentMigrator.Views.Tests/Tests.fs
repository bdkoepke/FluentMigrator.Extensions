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

    [<Fact>]
    let ``create root builds a raw SQL view`` () =
        let ctx = Helpers.context ()
        CreateRoot(ctx, "active_users").As("SELECT 1 AS id")
        Assert.Equal("CREATE VIEW active_users AS SELECT 1 AS id;", Helpers.lastStatement ctx)

    [<Fact>]
    let ``create root builds a replaceable raw SQL view`` () =
        let ctx = Helpers.context ()
        CreateRoot(ctx, "active_users").OrReplace().As("SELECT 1 AS id")

        Assert.Equal("CREATE OR REPLACE VIEW active_users AS SELECT 1 AS id;", Helpers.lastStatement ctx)

    [<Fact>]
    let ``create extension supports raw SQL views`` () =
        let ctx = Helpers.context ()
        CreateExpressionRoot(ctx).View("active_users").As("SELECT 1 AS id")

        Assert.Equal(1, Helpers.statements ctx |> List.length)
        Assert.Equal("CREATE VIEW active_users AS SELECT 1 AS id;", Helpers.lastStatement ctx)

    [<Fact>]
    let ``relational builder emits joins, columns, where and group by`` () =
        let ctx = Helpers.context ()

        CreateRoot(ctx, "active_users")
            .From("users u")
            .Join("accounts a", "a.user_id = u.id")
            .LeftJoin("profiles p", "p.user_id = u.id")
            .WithColumn("id")
            .As("u.id")
            .WithColumn("email")
            .As("u.email")
            .Where("u.active = true")
            .GroupBy("u.id, u.email")
        |> ignore

        let expected =
            String.concat
                "\n"
                [
                    "CREATE VIEW active_users ("
                    "    id,"
                    "    email"
                    ") AS"
                    "SELECT"
                    "    u.id,"
                    "    u.email"
                    "FROM users u"
                    "JOIN accounts a ON a.user_id = u.id"
                    "LEFT JOIN profiles p ON p.user_id = u.id"
                    "WHERE u.active = true"
                    "GROUP BY u.id, u.email;"
                ]

        Assert.Equal(expected, Helpers.lastStatement ctx)

    [<Fact>]
    let ``relational builder emits CTEs`` () =
        let ctx = Helpers.context ()

        CreateRoot(ctx, "recent_users")
            .With("recent", "SELECT * FROM users WHERE active = true")
            .From("recent r")
            .WithColumn("id")
            .As("r.id")
        |> ignore

        let expected =
            String.concat
                "\n"
                [
                    "CREATE VIEW recent_users ("
                    "    id"
                    ") AS"
                    "WITH recent AS ("
                    "    SELECT * FROM users WHERE active = true"
                    ")"
                    "SELECT"
                    "    r.id"
                    "FROM recent r;"
                ]

        Assert.Equal(expected, Helpers.lastStatement ctx)

    [<Fact>]
    let ``relational builder supports replace`` () =
        let ctx = Helpers.context ()

        CreateRoot(ctx, "active_users").OrReplace().From("users u").WithColumn("id").As("u.id")
        |> ignore

        Assert.StartsWith("CREATE OR REPLACE VIEW active_users", Helpers.lastStatement ctx)

    [<Fact>]
    let ``relational builder supports expression DSL and structured CTE`` () =
        let ctx = Helpers.context ()

        CreateRoot(ctx, "active_users")
            .With(
                "recent",
                fun cte ->
                    cte
                        .From("users", "u")
                        .WithColumn("id")
                        .As(Expr.colOf "u" "id")
                        .WithColumn("email")
                        .As(Expr.colOf "u" "email")
                        .Where(Expr.eq (Expr.colOf "u" "active") (Expr.bool true))
            )
            .From("recent", "r")
            .WithColumn("id")
            .As(Expr.colOf "r" "id")
            .WithColumn("email")
            .As(Expr.colOf "r" "email")
        |> ignore

        let expected =
            String.concat
                "\n"
                [
                    "CREATE VIEW active_users ("
                    "    id,"
                    "    email"
                    ") AS"
                    "WITH recent AS ("
                    "    SELECT"
                    "        u.id AS id,"
                    "        u.email AS email"
                    "    FROM users u"
                    "    WHERE (u.active = TRUE)"
                    ")"
                    "SELECT"
                    "    r.id,"
                    "    r.email"
                    "FROM recent r;"
                ]

        Assert.Equal(expected, Helpers.lastStatement ctx)

    [<Fact>]
    let ``relational builder supports union`` () =
        let ctx = Helpers.context ()

        CreateRoot(ctx, "keys")
            .From("previous_positions")
            .WithColumn("holding_snapshot_id")
            .As(Expr.col "holding_snapshot_id")
            .WithColumn("security_id")
            .As(Expr.col "security_id")
            .Union(fun other ->
                other
                    .From("current_positions")
                    .WithColumn("holding_snapshot_id")
                    .As(Expr.col "holding_snapshot_id")
                    .WithColumn("security_id")
                    .As(Expr.col "security_id"))
        |> ignore

        let expected =
            String.concat
                "\n"
                [
                    "CREATE VIEW keys ("
                    "    holding_snapshot_id,"
                    "    security_id"
                    ") AS"
                    "SELECT"
                    "    holding_snapshot_id,"
                    "    security_id"
                    "FROM previous_positions"
                    "UNION"
                    "SELECT"
                    "    holding_snapshot_id,"
                    "    security_id"
                    "FROM current_positions;"
                ]

        Assert.Equal(expected, Helpers.lastStatement ctx)

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

module ExprParserTests =
    [<Fact>]
    let ``parses operator precedence`` () =
        let expr = ExprParser.parse "a + b * 2"
        Assert.Equal("(a + (b * 2))", Expr.toSql expr)

    [<Fact>]
    let ``parses comparisons and boolean operators`` () =
        let expr = ExprParser.parse "u.age >= 18 AND u.active = TRUE"
        Assert.Equal("((u.age >= 18) AND (u.active = TRUE))", Expr.toSql expr)

    [<Fact>]
    let ``parses function calls and postfix predicates`` () =
        let expr = ExprParser.parse "coalesce(u.nickname, u.email) IS NOT NULL"
        Assert.Equal("(coalesce(u.nickname, u.email) IS NOT NULL)", Expr.toSql expr)

    [<Fact>]
    let ``parses prefix operators`` () =
        let expr = ExprParser.parse "NOT u.active AND -u.balance > 0"

        Assert.Equal("((NOT u.active) AND ((-u.balance) > 0))", Expr.toSql expr)

    [<Fact>]
    let ``parses quoted identifiers and escaped strings`` () =
        let expr = ExprParser.parse "\"user\".\"display name\" = 'O''Brien'"
        Assert.Equal("(\"user\".\"display name\" = 'O''Brien')", Expr.toSql expr)

    [<Fact>]
    let ``parses searched case expressions`` () =
        let expr = ExprParser.parse "CASE WHEN a > 0 THEN 'positive' ELSE 'other' END"
        Assert.Equal("CASE\n    WHEN (a > 0) THEN 'positive'\n    ELSE 'other'\nEND", Expr.toSql expr)

    [<Fact>]
    let ``parses window functions`` () =
        let expr =
            ExprParser.parse "row_number() OVER (PARTITION BY u.account_id ORDER BY u.created_at DESC)"

        Assert.Equal("row_number() OVER (PARTITION BY u.account_id ORDER BY u.created_at DESC)", Expr.toSql expr)

    [<Fact>]
    let ``returns an error for invalid input`` () =
        Assert.True(Result.isError (ExprParser.tryParse "a +"))
