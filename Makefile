.PHONY: help
help: ## Show this help message
	@echo "Rye Development Commands"
	@echo ""
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}'

# Stdlib load order cache (phony: stdlib deps not in makefile, run promiscuously).
.PHONY: stdlib-cache
stdlib-cache: ## Ensure inst/rye/load-order.rds is up to date (run before build/install)
	Rscript tools/build-stdlib-order.R

.PHONY: install
install: stdlib-cache ## Install package
	R -q -e "devtools::install()"

.PHONY: build
build: stdlib-cache ## Build the package
	R -q -e "devtools::build()"

.PHONY: check
check: build ## Check the package (includes tests)
	R -q -e 'devtools::check(args="--as-cran")'

.PHONY: devdoc
devdoc:
	R -q -e "devtools::document()"

.PHONY: readme
readme:
	R -q -e "rmarkdown::render('README.Rmd')"

.PHONY: vignettes
vignettes:
	R -q -e "devtools::build_vignettes()"

.PHONY: site
site:
	@tmp=$$(mktemp -d) && \
	rsync -a --delete \
	  --exclude 'AGENTS.md' \
	  --exclude 'CLAUDE.md' \
	  --exclude '.git/' \
	  ./ $$tmp/ && \
	Rscript -e "pkgdown::build_site(pkg='$$tmp')" && \
	rm -rf $$tmp

## Generate roxygen, README, and pkgdown docs
.PHONY: document
document: devdoc readme vignettes site

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
	R -q -e "devtools::load_all(); source('tests/testthat/helper-native.R'); engine <- RyeEngine\$$new(); env <- engine\$$env\$$env; run_native_test_file('tests/native/$(FILE).rye', engine, env)"

.PHONY: bench
bench: ## Run all benchmarks
	R -q -e "devtools::load_all(); source('benchmarks/run-all-benchmarks.R')"

.PHONY: bench-component
bench-component: ## Run single component benchmark (usage: make bench-component COMPONENT=tokenizer)
	@if [ -z "$(COMPONENT)" ]; then \
		echo "Error: COMPONENT parameter required. Options: tokenizer, parser, macro, eval, stdlib, e2e"; \
		exit 1; \
	fi
	R -q -e "devtools::load_all(); source('benchmarks/bench-$(COMPONENT).R')"

.PHONY: profile
profile: ## Generate profiling reports
	R -q -e "devtools::load_all(); source('benchmarks/run-all-profiles.R')"

.PHONY: profile-component
profile-component: ## Profile single component (usage: make profile-component COMPONENT=tokenizer)
	@if [ -z "$(COMPONENT)" ]; then \
		echo "Error: COMPONENT parameter required. Options: tokenizer, parser, macro, eval"; \
		exit 1; \
	fi
	R -q -e "devtools::load_all(); source('benchmarks/profile-$(COMPONENT).R')"

.PHONY: bench-compare
bench-compare: ## Compare benchmark results (usage: make bench-compare OLD=baseline.rds NEW=optimized.rds)
	@if [ -z "$(OLD)" ] || [ -z "$(NEW)" ]; then \
		echo "Error: OLD and NEW parameters required"; \
		exit 1; \
	fi
	R -q -e "source('benchmarks/compare-results.R'); compare_benchmarks('$(OLD)', '$(NEW)')"

.PHONY: clean
clean: ## Remove build artifacts and all make document output
	rm -f rye_*.tar.gz
	rm -rf rye.Rcheck
	rm -rf site/ doc/ Meta/
	rm -f README.knit.md
	rm -f vignettes/*.html vignettes/*.R vignettes/*.knit.md
	find . -type d -name ".rye_cache" -exec rm -rf {} +
	rm -f *.log
	rm -f benchmarks/profiles/*.html
	rm -rf benchmarks/profiles/*_files
	# rm -f README.md  # version-controlled
	# rm -rf man/  # version-controlled

.PHONY: cran-prep
cran-prep: stdlib-cache ## Prepare docs and run CRAN check
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

.PHONY: stdlib-order
stdlib-order: ## Print stdlib load order (topological sort)
	R -q -e "devtools::load_all(); rye:::rye_stdlib_print_order()"

# this is too crude to use, has too many false positives
# maybe we can improve it in the future
# .PHONY: stdlib-lint-check
# stdlib-lint-check: ## Print stdlib load order and crude undeclared dep check
# 	R -q -e "devtools::load_all(); rye:::rye_stdlib_print_order_and_check_undeclared()"

.PHONY: cran
cran: ## Run full CRAN prep/check/comments
	make cran-prep
	make cran-comments
