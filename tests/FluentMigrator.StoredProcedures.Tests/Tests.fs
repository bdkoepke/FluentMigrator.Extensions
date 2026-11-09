namespace FluentMigrator.StoredProcedures.Tests

open System
open Xunit
open FluentMigrator
open FluentMigrator.Builders.Create
open FluentMigrator.Builders.Delete
open FluentMigrator.Expressions
open FluentMigrator.Infrastructure
open FluentMigrator.StoredProcedures

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
    let ``toSql generates a procedure without parameters`` () =
        let definition =
            {
                Name = "refresh_data"
                Schema = None
                OrReplace = false
                Parameters = []
                Language = "plpgsql"
                Body = ""
            }

        let expected =
            String.concat "\n" [ "CREATE PROCEDURE refresh_data ()"; "LANGUAGE plpgsql"; "AS $$"; ""; "$$;" ]

        Assert.Equal(expected, toSql definition)

    [<Fact>]
    let ``toSql generates a qualified procedure with parameters and a body`` () =
        let definition =
            {
                Name = "upsert_user"
                Schema = Some "public"
                OrReplace = true
                Parameters =
                    [
                        {
                            Name = "user_id"
                            DataType = "integer"
                            Direction = In
                            DefaultValue = None
                        }
                        {
                            Name = "display_name"
                            DataType = "text"
                            Direction = In
                            DefaultValue = Some "'anonymous'"
                        }
                        {
                            Name = "created_at"
                            DataType = "timestamp"
                            Direction = Out
                            DefaultValue = None
                        }
                    ]
                Language = "plpgsql"
                Body = "BEGIN\n    NULL;\nEND"
            }

        let expected =
            String.concat
                "\n"
                [
                    "CREATE OR REPLACE PROCEDURE public.upsert_user ("
                    "    IN user_id integer,"
                    "    IN display_name text DEFAULT 'anonymous',"
                    "    OUT created_at timestamp"
                    ")"
                    "LANGUAGE plpgsql"
                    "AS $$"
                    "BEGIN"
                    "    NULL;"
                    "END"
                    "$$;"
                ]

        Assert.Equal(expected, toSql definition)

    [<Fact>]
    let ``toSql supports INOUT and VARIADIC parameters`` () =
        let definition =
            {
                Name = "calculate"
                Schema = None
                OrReplace = false
                Parameters =
                    [
                        {
                            Name = "running_total"
                            DataType = "numeric"
                            Direction = InOut
                            DefaultValue = None
                        }
                        {
                            Name = "values"
                            DataType = "integer[]"
                            Direction = Variadic
                            DefaultValue = None
                        }
                    ]
                Language = "sql"
                Body = "SELECT 1;"
            }

        Assert.Contains("    INOUT running_total numeric,", toSql definition)
        Assert.Contains("    VARIADIC values integer[]", toSql definition)
        Assert.Contains("LANGUAGE sql", toSql definition)

    [<Fact>]
    let ``toSql ignores blank schemas and blank default values`` () =
        let definition =
            {
                Name = "do_work"
                Schema = Some "   "
                OrReplace = false
                Parameters =
                    [
                        {
                            Name = "value"
                            DataType = "text"
                            Direction = In
                            DefaultValue = Some ""
                        }
                    ]
                Language = "plpgsql"
                Body = ""
            }

        let sql = toSql definition

        Assert.StartsWith("CREATE PROCEDURE do_work (", sql)
        Assert.DoesNotContain("DEFAULT", sql)

    [<Fact>]
    let ``dropSql generates a basic drop statement`` () =
        let options =
            {
                Name = "refresh_data"
                Schema = None
                IfExists = false
                ParameterTypes = []
                Cascade = false
            }

        Assert.Equal("DROP PROCEDURE refresh_data;", dropSql options)

    [<Fact>]
    let ``dropSql includes schema, parameter types and cascade`` () =
        let options =
            {
                Name = "upsert_user"
                Schema = Some "public"
                IfExists = true
                ParameterTypes = [ "integer"; "text" ]
                Cascade = true
            }

        Assert.Equal("DROP PROCEDURE IF EXISTS public.upsert_user(integer, text) CASCADE;", dropSql options)

    [<Fact>]
    let ``dropSql ignores a blank schema and restrict omits cascade`` () =
        let options =
            {
                Name = "upsert_user"
                Schema = Some " "
                IfExists = true
                ParameterTypes = [ "integer" ]
                Cascade = false
            }

        Assert.Equal("DROP PROCEDURE IF EXISTS upsert_user(integer);", dropSql options)

module Builder =
    [<Fact>]
    let ``builder records a single expression and updates parameters in order`` () =
        let ctx = Helpers.context ()
        let builder = Builder(ctx, "upsert_user")

        builder
            .InSchema("public")
            .OrReplace()
            .WithParameter("user_id", "integer")
            .WithDefaultValue("1")
            .WithOutParameter("result", "text")
            .AsBody("BEGIN\n    NULL;\nEND")
        |> ignore

        let expected =
            String.concat
                "\n"
                [
                    "CREATE OR REPLACE PROCEDURE public.upsert_user ("
                    "    IN user_id integer DEFAULT 1,"
                    "    OUT result text"
                    ")"
                    "LANGUAGE plpgsql"
                    "AS $$"
                    "BEGIN"
                    "    NULL;"
                    "END"
                    "$$;"
                ]

        Assert.Equal(1, Helpers.statements ctx |> List.length)
        Assert.Equal(expected, Helpers.lastStatement ctx)

    [<Fact>]
    let ``builder supports INOUT parameters, defaults and a custom language`` () =
        let ctx = Helpers.context ()
        let builder = Builder(ctx, "calculate")

        builder
            .WithInOutParameter("running_total", "numeric")
            .WithDefaultValue("0")
            .Language("sql")
            .AsBody("SELECT running_total;")
        |> ignore

        let sql = Helpers.lastStatement ctx

        Assert.Contains("    INOUT running_total numeric DEFAULT 0", sql)
        Assert.Contains("LANGUAGE sql", sql)
        Assert.Contains("SELECT running_total;", sql)

    [<Fact>]
    let ``create root starts a procedure builder`` () =
        let ctx = Helpers.context ()
        let builder = CreateRoot(ctx, "refresh_data")

        builder.AsBody("SELECT 1;") |> ignore

        Assert.Contains("CREATE PROCEDURE refresh_data ()", Helpers.lastStatement ctx)
        Assert.Contains("SELECT 1;", Helpers.lastStatement ctx)

    [<Fact>]
    let ``create extension is available from the create expression root`` () =
        let ctx = Helpers.context ()
        let builder = CreateExpressionRoot(ctx).StoredProcedure("refresh_data")

        builder.AsBody("SELECT 1;") |> ignore

        Assert.Equal(1, Helpers.statements ctx |> List.length)
        Assert.Contains("CREATE PROCEDURE refresh_data ()", Helpers.lastStatement ctx)

module Delete =
    [<Fact>]
    let ``delete builder drops a procedure if it exists by default`` () =
        let ctx = Helpers.context ()
        let expression = ExecuteSqlStatementExpression()
        ctx.Expressions.Add(expression)

        DeleteProcedureExpressionBuilder(expression, "refresh_data") |> ignore

        Assert.Equal("DROP PROCEDURE IF EXISTS refresh_data;", expression.SqlStatement)

    [<Fact>]
    let ``delete builder supports schema, parameter types and cascade`` () =
        let ctx = Helpers.context ()
        let expression = ExecuteSqlStatementExpression()
        ctx.Expressions.Add(expression)

        let builder = DeleteProcedureExpressionBuilder(expression, "upsert_user")

        builder.InSchema("public").WithParameterTypes([ "integer"; "text" ]).Cascade()
        |> ignore

        Assert.Equal("DROP PROCEDURE IF EXISTS public.upsert_user(integer, text) CASCADE;", expression.SqlStatement)

    [<Fact>]
    let ``delete builder restrict removes cascade`` () =
        let ctx = Helpers.context ()
        let expression = ExecuteSqlStatementExpression()
        ctx.Expressions.Add(expression)

        let builder = DeleteProcedureExpressionBuilder(expression, "upsert_user")

        builder.Cascade().Restrict() |> ignore

        Assert.Equal("DROP PROCEDURE IF EXISTS upsert_user;", expression.SqlStatement)

    [<Fact>]
    let ``delete extension configures the full drop statement`` () =
        let ctx = Helpers.context ()
        let deletion = DeleteExpressionRoot(ctx).StoredProcedure("upsert_user")

        deletion.InSchema("public").WithParameterTypes([ "integer"; "text" ]).Cascade()
        |> ignore

        Assert.Equal(1, Helpers.statements ctx |> List.length)

        Assert.Equal("DROP PROCEDURE IF EXISTS public.upsert_user(integer, text) CASCADE;", Helpers.lastStatement ctx)
