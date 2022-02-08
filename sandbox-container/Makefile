.DEFAULT_GOAL := help

.PHONY: dev
dev: ## Run container for development
	make build-skip-ui
	./dev.sh

.PHONY: build-builder
build-builder: ## Build builder docker contianer
	docker build -t visortelle/haskell-playground-sandbox-builder:main -f Dockerfile.builder .

.PHONY: build-skip-ui
build-skip-ui: ## Build docker image skipping building UI
	docker build -t visortelle/haskell-playground-sandbox:main .

.PHONY: build
build: ## Build docker image
	./build.sh

.PHONY: publish
publish: ## Publish docker image to registry
	docker push visortelle/haskell-playground-sandbox:main

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
