.PHONY: help
help: ## Show this help message
	@echo "Rye Development Commands"
	@echo ""
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}'

.PHONY: install
install: ## Install package
	R -q -e "devtools::install()"

.PHONY: build
build: ## Build the package
	R -q -e "devtools::build()"

.PHONY: check
check: build ## Check the package (includes tests)
	R -q -e 'devtools::check(args="--as-cran")'

.PHONY: document
document: ## Generate roxygen, README, and pkgdown docs
	R -q -e "devtools::document()"
	R -q -e "rmarkdown::render('README.Rmd')"
	R -q -e "devtools::build_vignettes()"
	R -q -e "pkgdown::build_site()"

.PHONY: coverage
coverage:
	R -q -e "covr::package_coverage()"

.PHONY: lint
lint:
	R -q -e "lintr::lint_dir(path='.')"

.PHONY: test
test: ## Run tests
	R -q -e "testthat::set_max_fails(Inf); devtools::test()"

.PHONY: test-file
test-file: ## Run a single test file (usage: make test-file FILE=test-parser)
	@if [ -z "$(FILE)" ]; then \
		echo "Error: FILE parameter required. Usage: make test-file FILE=test-parser"; \
		exit 1; \
	fi
	R -q -e "testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/$(FILE).R')"

.PHONY: clean
clean: ## Remove build artifacts
	rm -f rye_*.tar.gz
	rm -rf rye.Rcheck
	rm -rf site/
