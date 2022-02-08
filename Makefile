.DEFAULT_GOAL := help

.PHONY: dev
dev: ## Start development.
	cd sandbox-container && make dev

.PHONY: build
build: ## Build.
	cd ./sandbox-container && make build
	# ./sandbox-builder/build.sh base-sandbox

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
