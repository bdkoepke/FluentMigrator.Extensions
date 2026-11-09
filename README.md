# FluentMigrator.Extensions

A collection of [FluentMigrator](https://fluentmigrator.github.io/) extensions for
PostgreSQL-specific database objects, written in F#.

The solution currently provides two packages:

| Package | Description |
| --- | --- |
| `FluentMigrator.Views` | Fluent builders for creating, replacing, and dropping PostgreSQL views, including JSON views, relational views, CTEs, unions, and an SQL expression DSL/parser. |
| `FluentMigrator.StoredProcedures` | Fluent builders for creating, replacing, and dropping PostgreSQL stored procedures with typed, directional parameters. |

Both packages extend the standard `Create` and `Delete` expression roots, so they
can be used directly inside FluentMigrator migrations.

## Requirements

- .NET SDK targeting `net10.0`
- F# (the libraries are F# projects)
- FluentMigrator 8.x
- PostgreSQL (the generated SQL is PostgreSQL-flavored)

## Installation

Add the package(s) you need to your migration project:

```bash
dotnet add package FluentMigrator.Views
dotnet add package FluentMigrator.StoredProcedures
```

To build the packages from source:

```bash
dotnet pack -c Release
```

## Views

Open the `FluentMigrator.Views` module and use `Create.View` / `Delete.View`.

### JSON views

The `OnTable` builder extracts values from a JSON column (default source column:
`value`) using PostgreSQL JSON operators and functions.

```fsharp
open FluentMigrator
open FluentMigrator.Views

type CreateActiveUsersView() =
    inherit Migration()

    override this.Up() =
        this.Create
            .View("active_users")
            .OnTable("users")
            .WithColumn("id").AsJson("$.id")
            .WithColumn("email").AsJson("$.email")
            .WithColumn("age").AsJson("$.age").AsType("integer")
        |> ignore

    override this.Down() =
        this.Delete.View("active_users").IfExists()
```

This produces:

```sql
CREATE VIEW active_users (
    id,
    email,
    age
) AS
SELECT
    value::json->>'id',
    value::json->>'email',
    CAST(value::json->>'age' AS integer)
FROM users;
```

- `AsJson "$.address.city"` uses `json_extract_path_text` for nested paths.
- `AsType "integer"` wraps the extracted text in `CAST(... AS integer)`.
- `WithSourceColumn "payload"` changes the JSON column being read.
- Paths may be written with or without the `$` root prefix.

### Raw SQL views

```fsharp
this.Create.View("active_users").As("SELECT 1 AS id")
this.Create.View("active_users").OrReplace().As("SELECT 1 AS id")
```

### Relational views

The relational builder supports `From`, `Join`/`LeftJoin`, `Where`, `GroupBy`,
CTEs, and `Union`. Conditions and columns can be supplied as raw SQL strings or
as `Expr` values.

```fsharp
this.Create
    .View("active_users")
    .From("users u")
    .Join("accounts a", "a.user_id = u.id")
    .LeftJoin("profiles p", "p.user_id = u.id")
    .WithColumn("id").As("u.id")
    .WithColumn("email").As("u.email")
    .Where("u.active = true")
    .GroupBy("u.id, u.email")
|> ignore
```

Structured CTE:

```fsharp
this.Create
    .View("active_users")
    .With(
        "recent",
        fun cte ->
            cte
                .From("users", "u")
                .WithColumn("id").As(Expr.colOf "u" "id")
                .WithColumn("email").As(Expr.colOf "u" "email")
                .Where(Expr.eq (Expr.colOf "u" "active") (Expr.bool true))
    )
    .From("recent", "r")
    .WithColumn("id").As(Expr.colOf "r" "id")
    .WithColumn("email").As(Expr.colOf "r" "email")
|> ignore
```

Union:

```fsharp
this.Create
    .View("keys")
    .From("previous_positions")
    .WithColumn("security_id").As(Expr.col "security_id")
    .Union(fun other ->
        other
            .From("current_positions")
            .WithColumn("security_id").As(Expr.col "security_id"))
|> ignore
```

### SQL expression DSL and parser

The `FluentMigrator.Views.Expr` module provides typed SQL expressions:

```fsharp
Expr.eq (Expr.colOf "u" "active") (Expr.bool true)
Expr.andAlso (Expr.gte (Expr.colOf "u" "age") (Expr.int 18)) (Expr.isNotNull (Expr.col "email"))
Expr.coalesce [ Expr.col "nickname"; Expr.col "email" ]
Expr.rowNumber [ Expr.col "account_id" ] [ Expr.colOf "u" "created_at", Expr.Desc ]
```

The module also includes `FluentMigrator.ExprParser`, which parses SQL-like
expressions into the same `Expr` tree:

```fsharp
let expr = ExprParser.parse "u.age >= 18 AND coalesce(u.nickname, u.email) IS NOT NULL"
let sql = Expr.toSql expr

match ExprParser.tryParse "a +" with
| Ok _ -> ()
| Error message -> printfn "Invalid expression: %s" message
```

Supported syntax includes identifiers (plain and quoted), string/numeric/boolean
literals, `NULL`, function calls, `CASE`, window functions (`OVER`),
arithmetic and comparison operators, `LIKE`, `IS [NOT] NULL`, `NOT`, `AND`,
and `OR`.

### Dropping views

```fsharp
this.Delete.View("active_users").IfExists()
this.Delete.View("active_users").InSchema("reporting")
```

`DROP VIEW IF EXISTS` is used by the delete builder.

## Stored procedures

Open the `FluentMigrator.StoredProcedures` module and use
`Create.StoredProcedure` / `Delete.StoredProcedure`.

### Creating procedures

```fsharp
open FluentMigrator
open FluentMigrator.StoredProcedures

type CreateUpsertUserProcedure() =
    inherit Migration()

    override this.Up() =
        this.Create
            .StoredProcedure("upsert_user")
            .InSchema("public")
            .OrReplace()
            .WithParameter("user_id", "integer")
            .WithParameter("display_name", "text")
            .WithDefaultValue("'anonymous'")
            .WithOutParameter("created_at", "timestamp")
            .Language("plpgsql")
            .AsBody("""
BEGIN
    INSERT INTO users (id, name) VALUES (user_id, display_name);
    SELECT now() INTO created_at;
END
""")
        |> ignore

    override this.Down() =
        this.Delete
            .StoredProcedure("upsert_user")
            .InSchema("public")
            .WithParameterTypes([ "integer"; "text"; "timestamp" ])
            .Cascade()
        |> ignore
```

Parameter builders:

- `WithParameter(name, dataType)` – `IN` parameter (default)
- `WithOutParameter(name, dataType)` – `OUT` parameter
- `WithInOutParameter(name, dataType)` – `INOUT` parameter
- `WithParameter(name, dataType, Variadic)` – `VARIADIC` parameter
- `WithDefaultValue sql` – attaches a default expression to the most recently
  added parameter

The generated statement has the form:

```sql
CREATE OR REPLACE PROCEDURE public.upsert_user (
    IN user_id integer,
    IN display_name text DEFAULT 'anonymous',
    OUT created_at timestamp
)
LANGUAGE plpgsql
AS $$
...
$$;
```

Blank schema names and blank default values are ignored.

### Dropping procedures

```fsharp
this.Delete
    .StoredProcedure("upsert_user")
    .InSchema("public")
    .WithParameterTypes([ "integer"; "text" ])
    .Cascade()
|> ignore
```

`IfExists()` is enabled by default. Use `Restrict()` to remove `CASCADE`.

## Project structure

```
src/
  FluentMigrator.Views/              JSON, raw, and relational view builders + expression parser
  FluentMigrator.StoredProcedures/   Stored procedure builders
tests/
  FluentMigrator.Views.Tests/        xUnit tests for views and the expression parser
  FluentMigrator.StoredProcedures.Tests/ xUnit tests for stored procedures
```

## Building and testing

```bash
dotnet restore
dotnet build
dotnet test
```

Formatting uses [Fantomas](https://github.com/fsprojects/fantomas) via a local
tool manifest:

```bash
dotnet tool restore
dotnet fantomas .
```

## Notes

- Generated SQL is specific to PostgreSQL. The views package uses `json` casts
  and `json_extract_path_text`; the stored procedures package uses
  PostgreSQL's `CREATE PROCEDURE` / `DROP PROCEDURE` syntax.
- The builders add a single `ExecuteSqlStatementExpression` to the migration
  context and update its SQL as the fluent chain is built.
