.DEFAULT_GOAL := help

.PHONY: dev
dev: ## Run container for development
	docker run -it -p 8080:8080 visortelle/haskell-playground-sandbox-base:main

.PHONY: build
build: ## Build docker image
	docker build -t visortelle/haskell-playground-sandbox-base:main .

.PHONY: publish
publish: ## Publish docker image to registry
	docker push visortelle/haskell-playground-sandbox-base:main

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
