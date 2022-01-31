.DEFAULT_GOAL := help

.PHONY: dev
dev: ## Start development.
	make build
	docker run -it -p 8080:8080 -e HSPG_ORIGIN="localhost:8080" -e HSPG_PUBLIC_URL="http://localhost:8080" visortelle/haskell-playground-sandbox:base &
	cd widget && make dev

.PHONY: build
build: ## Build.
	cd ./widget && make build
	cd ./sandbox-base && make build
	./build-sandbox/build.sh base-sandbox

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
