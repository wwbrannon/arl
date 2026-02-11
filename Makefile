SHELL := /bin/bash

#
## Build/install targets
#

# Keep inst/rye/load-order.rds up to date (must run before build/install)
.PHONY: stdlib-order
stdlib-order: ## help: Build stdlib load order cache
	Rscript tools/build-stdlib-order.R

.PHONY: install
install: clean-cache stdlib-order ## help: Install the package
	R -q -e "devtools::install()"

.PHONY: build
build: clean-cache stdlib-order ## help: Build the package tarball
	R -q -e "devtools::build()"

#
## Documentation targets
#

.PHONY: stdlib-docs
stdlib-docs: ## help: Generate stdlib reference vignettes from .rye source
	Rscript tools/docs/generate-stdlib-docs.R

.PHONY: devdoc
devdoc: ## help: Generate roxygen documentation
	R -q -e "devtools::document()"

.PHONY: readme
readme: ## help: Render README from README.Rmd
	R -q -e "rmarkdown::render('README.Rmd')"

.PHONY: vignettes
vignettes: ## help: Build vignettes
	R -q -e "devtools::build_vignettes()"

.PHONY: site
site: ## help: Build pkgdown site
	@tmp=$$(mktemp -d) && \
	rsync -a --delete \
	  --exclude 'AGENTS.md' \
	  --exclude 'CLAUDE.md' \
	  --exclude '.git/' \
	  ./ $$tmp/ && \
	Rscript -e "pkgdown::build_site(pkg='$$tmp')" && \
	rm -rf $$tmp

.PHONY: document
document: stdlib-docs devdoc readme vignettes site ## help: Generate all documentation (roxygen, README, vignettes, site)

#
## QA targets
#

.PHONY: check
check: build ## help: Check the package (includes tests)
	R -q -e 'devtools::check(args="--as-cran")'

#
## Coverage targets
#

.PHONY: coverage
coverage: coverage-all ## help: Run complete coverage analysis (R + Rye)

.PHONY: coverage-all
coverage-all: coverage-r coverage-rye coverage-combined ## help: Run all coverage and generate combined report

.PHONY: coverage-r
coverage-r: clean-cache ## help: Run R code coverage only
	@echo "Running R code coverage..."
	@mkdir -p coverage/r
	@R -q -e "\
	  cov <- covr::package_coverage(type='all', quiet=FALSE); \
	  pct <- covr::percent_coverage(cov); \
	  writeLines(c( \
	    paste0('rye Coverage: ', sprintf('%.2f%%', pct)), \
	    '', \
	    capture.output(print(cov), type = 'message') \
	  ), 'coverage/r/summary.txt'); \
	  covr::to_cobertura(cov, filename='coverage/r/coverage.xml'); \
	  message(sprintf('R coverage: %.1f%%', pct)); \
	  if (requireNamespace('DT', quietly=TRUE) && requireNamespace('htmltools', quietly=TRUE)) { \
	    covr::report(cov, file='coverage/r/index.html'); \
	    message('R coverage report: coverage/r/index.html'); \
	  } else { \
	    message('Skipping HTML report (install DT and htmltools packages for HTML output)'); \
	  } \
	"

.PHONY: coverage-rye
coverage-rye: clean-cache ## help: Run Rye code coverage only
	@echo "Running Rye code coverage..."
	@mkdir -p coverage/rye
	@R -q -e "\
	  source('tools/coverage/rye-coverage.R'); \
	  tracker <- rye_coverage_report( \
	    output = c('console', 'html', 'json'), \
	    html_file = 'coverage/rye/index.html', \
	    json_file = 'coverage/rye/coverage.json' \
	  ); \
	" | tee coverage/rye/summary.txt

.PHONY: coverage-combined
coverage-combined: ## help: Generate combined coverage summary
	@echo "Generating combined coverage report..."
	@mkdir -p coverage/combined
	@R -q -e "source('tools/coverage/coverage-combine.R'); generate_combined_report()"

.PHONY: coverage-report
coverage-report: ## help: Open coverage reports in browser
	@echo "Opening coverage reports..."
	@if [ -f coverage/combined/index.html ]; then \
		open coverage/combined/index.html; \
	elif [ -f coverage/r/index.html ]; then \
		open coverage/r/index.html; \
		[ -f coverage/rye/index.html ] && open coverage/rye/index.html; \
	else \
		echo "No coverage reports found. Run 'make coverage' first."; \
		exit 1; \
	fi

.PHONY: coverage-upload
coverage-upload: ## help: Upload coverage to codecov (for CI)
	@echo "Uploading coverage to Codecov..."
	@if [ -z "$(CODECOV_TOKEN)" ]; then \
		echo "Warning: CODECOV_TOKEN not set"; \
	fi
	@if [ -f coverage/r/coverage.xml ]; then \
		bash <(curl -s https://codecov.io/bash) \
			-f coverage/r/coverage.xml \
			-F r-code \
			-n "R Coverage" || true; \
	fi
	@if [ -f coverage/rye/coverage.json ]; then \
		bash <(curl -s https://codecov.io/bash) \
			-f coverage/rye/coverage.json \
			-F rye-code \
			-n "Rye Coverage" || true; \
	fi

.PHONY: coverage-clean
coverage-clean: ## help: Remove coverage output files
	@echo "Cleaning coverage outputs..."
	@rm -rf coverage
	@echo "Coverage outputs cleaned."

.PHONY: lint
lint: ## help: Run linter checks
	R -q -e "lintr::lint_dir(path='.')"

.PHONY: test
test: clean-cache ## help: Run tests
	R -q -e "testthat::set_max_fails(Inf); devtools::test()"

.PHONY: test-file
test-file: clean-cache ## help: Run a single test file (usage: make test-file FILE=test-parser)
	@if [ -z "$(FILE)" ]; then \
		echo "Error: FILE parameter required. Usage: make test-file FILE=test-parser"; \
		exit 1; \
	fi
	R -q -e "devtools::load_all(); testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/$(FILE).R')"

.PHONY: test-native
test-native: clean-cache ## help: Run a single native test file (usage: make test-native FILE=test-equality-types)
	@if [ -z "$(FILE)" ]; then \
		echo "Error: FILE parameter required. Usage: make test-native FILE=test-equality-types"; \
		exit 1; \
	fi
	R -q -e "devtools::load_all(); source('tests/testthat/helper-native.R'); engine <- Engine\$$new(); env <- engine\$$env\$$env; run_native_test_file('tests/native/$(FILE).rye', engine, env)"

.PHONY: bench
bench: clean-cache ## help: Run all benchmarks
	R -q -e "devtools::load_all(); source('benchmarks/run-all-benchmarks.R')"

.PHONY: bench-component
bench-component: clean-cache ## help: Run single component benchmark (usage: make bench-component COMPONENT=tokenizer)
	@if [ -z "$(COMPONENT)" ]; then \
		echo "Error: COMPONENT parameter required. Options: tokenizer, parser, macro, eval, stdlib, e2e"; \
		exit 1; \
	fi
	R -q -e "devtools::load_all(); source('benchmarks/bench-$(COMPONENT).R')"

.PHONY: profile
profile: clean-cache ## help: Generate profiling reports
	R -q -e "devtools::load_all(); source('benchmarks/run-all-profiles.R')"

.PHONY: profile-component
profile-component: clean-cache ## help: Profile single component (usage: make profile-component COMPONENT=tokenizer)
	@if [ -z "$(COMPONENT)" ]; then \
		echo "Error: COMPONENT parameter required. Options: tokenizer, parser, macro, eval"; \
		exit 1; \
	fi
	R -q -e "devtools::load_all(); source('benchmarks/profile-$(COMPONENT).R')"

.PHONY: bench-compare
bench-compare: ## help: Compare benchmark results (usage: make bench-compare OLD=baseline.rds NEW=optimized.rds)
	@if [ -z "$(OLD)" ] || [ -z "$(NEW)" ]; then \
		echo "Error: OLD and NEW parameters required"; \
		exit 1; \
	fi
	R -q -e "source('benchmarks/compare-results.R'); compare_benchmarks('$(OLD)', '$(NEW)')"

.PHONY: stdlib-show-order
stdlib-show-order: ## help: Print stdlib load order (topological sort)
	R -q -e "devtools::load_all(); rye:::stdlib_print_order()"

#
## CRAN submission helper targets
#

.PHONY: cran
cran: ## help: Run full CRAN prep/check/comments
	make cran-prep
	make cran-comments

.PHONY: cran-prep
cran-prep: stdlib-order ## help: Prepare docs and run CRAN check
	R -q -e "source('tools/cran/cran_prep.R')"

.PHONY: cran-check
cran-check: ## help: Run CRAN check
	R -q -e "source('tools/cran/cran_check.R')"

.PHONY: cran-comments
cran-comments: ## help: Generate cran-comments and CRAN-SUBMISSION
	R -q -e "source('tools/cran/cran_comments.R')"

.PHONY: cran-clean
cran-clean: ## help: Remove CRAN check artifacts
	rm -rf rye.Rcheck
	rm -f tools/cran/cran_check_summary.txt

#
## Cleanup
#

.PHONY: clean-cache
clean-cache: ## help: Remove .rye_cache directories (auto-runs before dev targets)
	@find . -type d -name ".rye_cache" -exec rm -rf {} + 2>/dev/null || true

.PHONY: clean
clean: coverage-clean clean-cache ## help: Remove build artifacts and all make document output
	rm -f rye_*.tar.gz
	rm -rf rye.Rcheck
	rm -rf site/ doc/ Meta/
	rm -f README.knit.md
	rm -f vignettes/*.html vignettes/*.R vignettes/*.knit.md
	rm -f *.log
	rm -f benchmarks/profiles/*.html
	rm -rf benchmarks/profiles/*_files
	# rm -f README.md  # version-controlled
	# rm -rf man/  # version-controlled

#
## Help
#

.PHONY: help
help: ## help: Show this help message
	@echo "Rye Development Commands"
	@echo ""
	@grep -E '^[a-zA-Z_-]+:.*?## help: .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## help: "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}'
