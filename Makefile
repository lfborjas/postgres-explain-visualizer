serve:
	DATABASE_URL=postgresql://localhost/pev_dev?user=$(whoami) DEPLOY_ENV=Development PORT=3333 cabal run
migrate:
	DATABASE_URL=postgresql://localhost/pev_dev?user=$(whoami) DEPLOY_ENV=Development PORT=3333 cabal run pev-exe -- --migrate
format:
	fourmolu -o -XImportQualifiedPost --mode inplace $(git ls-files '*.hs')
