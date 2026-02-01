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
	R -q -e "devtools::load_all(); testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/$(FILE).R')"

.PHONY: test-native
test-native: ## Run a single native test file (usage: make test-native FILE=test-equality-types)
	@if [ -z "$(FILE)" ]; then \
		echo "Error: FILE parameter required. Usage: make test-native FILE=test-equality-types"; \
		exit 1; \
	fi
	R -q -e "devtools::load_all(); source('tests/testthat/helper-native.R'); engine <- RyeEngine\$$new(); env <- engine\$$env\$$env; for (m in c('control', 'binding', 'strings', 'math', 'predicates', 'list', 'higher-order', 'sequences')) { exprs <- engine\$$read(sprintf('(load \"%s\")', m)); engine\$$eval_in_env(exprs[[1]], env) }; run_native_test_file('tests/native/$(FILE).rye', engine, env)"

.PHONY: bench
bench: ## Run all benchmarks
	R -q -e "source('benchmarks/run-all-benchmarks.R')"

.PHONY: bench-component
bench-component: ## Run single component benchmark (usage: make bench-component COMPONENT=tokenizer)
	@if [ -z "$(COMPONENT)" ]; then \
		echo "Error: COMPONENT parameter required. Options: tokenizer, parser, macro, eval, stdlib, e2e"; \
		exit 1; \
	fi
	R -q -e "source('benchmarks/bench-$(COMPONENT).R')"

.PHONY: profile
profile: ## Generate profiling reports
	R -q -e "source('benchmarks/run-all-profiles.R')"

.PHONY: profile-component
profile-component: ## Profile single component (usage: make profile-component COMPONENT=tokenizer)
	@if [ -z "$(COMPONENT)" ]; then \
		echo "Error: COMPONENT parameter required. Options: tokenizer, parser, macro, eval"; \
		exit 1; \
	fi
	R -q -e "source('benchmarks/profile-$(COMPONENT).R')"

.PHONY: bench-compare
bench-compare: ## Compare benchmark results (usage: make bench-compare OLD=baseline.rds NEW=optimized.rds)
	@if [ -z "$(OLD)" ] || [ -z "$(NEW)" ]; then \
		echo "Error: OLD and NEW parameters required"; \
		exit 1; \
	fi
	R -q -e "source('benchmarks/compare-results.R'); compare_benchmarks('$(OLD)', '$(NEW)')"

.PHONY: clean
clean: ## Remove build artifacts
	rm -f rye_*.tar.gz
	rm -rf rye.Rcheck
	rm -rf site/ doc/ Meta/

.PHONY: cran-prep
cran-prep: ## Prepare docs and run CRAN check
	R -q -e "source('tools/cran_prep.R')"

.PHONY: cran-check
cran-check: ## Run CRAN check
	R -q -e "source('tools/cran_check.R')"

.PHONY: cran-comments
cran-comments: ## Generate cran-comments and CRAN-SUBMISSION
	R -q -e "source('tools/cran_comments.R')"

.PHONY: cran-clean
cran-clean: ## Remove CRAN check artifacts
	rm -rf rye.Rcheck
	rm -f tools/cran_check_summary.txt

.PHONY: cran
cran: ## Run full CRAN prep/check/comments
	make cran-prep
	make cran-comments
