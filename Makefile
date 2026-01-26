.PHONY: help install build check test test-file document clean

help: ## Show this help message
	@echo "Rye Development Commands"
	@echo ""
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}'

install: ## Install package dependencies
	R -e "devtools::install_deps()"

build: ## Build the package
	R -e "devtools::build()"

check: build ## Check the package (includes tests)
	R -e "devtools::check()"

document: ## Generate roxygen2 documentation
	R -e "devtools::document()"

coverage:
	R -e "covr::package_coverage()"

test: ## Run tests
	R -e "devtools::test()"

test-file: ## Run a single test file (usage: make test-file FILE=test-parser)
	@if [ -z "$(FILE)" ]; then \
		echo "Error: FILE parameter required. Usage: make test-file FILE=test-parser"; \
		exit 1; \
	fi
	R -e "testthat::test_file('tests/testthat/$(FILE).R')"

clean: ## Remove build artifacts
	rm -f rye_*.tar.gz
	rm -rf rye.Rcheck
