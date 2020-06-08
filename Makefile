app.js: index.js package.json src/*.elm
	yarn build

.PHONY: start
start:
	yarn start

.PHONY: format
format:
	yarn format
