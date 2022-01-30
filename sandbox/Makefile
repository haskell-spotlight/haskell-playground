.DEFAULT_GOAL := help

# .PHONY: dev
# dev: ## Run container for development
# 	docker run -it -p 8000:8000 -p 8001:8001 haskell-playground-sandbox:latest

.PHONY: dev
dev: ## Run container for development
	docker run -it -p 8000:8000 -p 8001:8001 haskell-playground-sandbox:latest


.PHONY: build
build: ## Build docker container
	docker build -t haskell-playground-sandbox:latest .

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
