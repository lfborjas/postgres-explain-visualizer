# postgres-explain-visualizer

pev2, with a backend

## Development

There's a `shell.nix`, enter a `nix-shell` or put `use nix` in an `.envrc` file to develop.

## Deployment

See the `heroku.yml` github action; you can set a repository secret on your own fork to deploy to your own Heroku instance. You'll need Postgres provisioned.

### Migrations

You can send the `--migrate` option to the generated binary to run migrations. The migration runner is idempotent, but there's no rollback, so... always going forward, 'cuz we can't find reverse!

Since migrations for this particular app are a rare occurence, I just ran them manually after deploy, like so:

``` sh
> heroku run ash -a pgexplain
Running ash on ⬢ pgexplain... up, run.9955 (Free)
~ $ bin/pev-exe --migrate
Migrating...
Initializing schema
NOTICE:  relation "schema_migrations" already exists, skipping
Execute:	20220519174700_extensions_and_date_procs.sql
NOTICE:  trigger "plan_insert" for relation "plan" does not exist, skipping
NOTICE:  trigger "plan_update" for relation "plan" does not exist, skipping
Execute:	20220519174800_add_plan_table.sql
All migrations ran.
```

If you deploy your fork to Heroku, you can do the same, or update the code or docker derivation to run them, maybe?
