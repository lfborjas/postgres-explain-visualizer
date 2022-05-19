serve:
	DATABASE_URL=postgresql://localhost/pev_dev?user=$(whoami) DEPLOY_ENV=Development PORT=3333 cabal run
