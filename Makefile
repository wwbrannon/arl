SHELL := /bin/bash

#
## Build/install targets
#

# Keep inst/arl/load-order.txt up to date (must run before build/install)
.PHONY: stdlib-order
stdlib-order: ## help: Build stdlib load order cache (inst/arl/load-order.txt)
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
stdlib-docs: clean-cache stdlib-order ## help: Generate stdlib reference vignettes from .arl source
	R -q -e "devtools::load_all(); source('tools/docs/generate-stdlib-docs.R')"

.PHONY: devdoc
devdoc: clean-cache stdlib-order ## help: Generate roxygen documentation
	R -q -e "devtools::document()"

.PHONY: readme
readme: clean-cache stdlib-order ## help: Render README from README.Rmd
	R -q -e "devtools::load_all(); rmarkdown::render('README.Rmd')"

.PHONY: vignettes
vignettes: clean-cache stdlib-order stdlib-docs ## help: Build vignettes
	R -q -e "devtools::build_vignettes()"

.PHONY: site
site: clean-cache stdlib-order ## help: Build pkgdown site
	@tmp=$$(mktemp -d) && \
	rsync -a --delete \
	  --exclude 'AGENTS.md' \
	  --exclude 'CLAUDE.md' \
	  --exclude '.git/' \
	  ./ $$tmp/ && \
	Rscript -e "pkgdown::build_site(pkg='$$tmp')" && \
	rm -rf site && \
	mv $$tmp/site site && \
	rm -rf $$tmp

.PHONY: document
document: devdoc readme vignettes site ## help: Generate all documentation

#
## Test running, lint, R CMD check
#

.PHONY: check
check: build ## help: Check the package (includes tests)
	R -q -e 'devtools::check(args="--as-cran", check_dir=".")'

.PHONY: lint
lint: clean-cache stdlib-order ## help: Run linter checks
	R -q -e "devtools::load_all(); lintr::lint_dir(path='.')"

.PHONY: test
test: clean-cache stdlib-order ## help: Run tests
	R -q -e "testthat::set_max_fails(Inf); devtools::test()"

.PHONY: test-file
test-file: clean-cache stdlib-order ## help: Run a single test file (usage: make test-file FILE=test-parser)
	@if [ -z "$(FILE)" ]; then \
		echo "Error: FILE parameter required. Usage: make test-file FILE=test-parser"; \
		exit 1; \
	fi
	R -q -e "devtools::load_all(); testthat::set_max_fails(Inf); testthat::test_file('tests/testthat/$(FILE).R')"

.PHONY: test-native
test-native: clean-cache stdlib-order ## help: Run a single native test file (usage: make test-native FILE=test-equality-types)
	@if [ -z "$(FILE)" ]; then \
		echo "Error: FILE parameter required. Usage: make test-native FILE=test-equality-types"; \
		exit 1; \
	fi
	R -q -e "devtools::load_all(); source('tests/testthat/helper-native.R'); engine <- Engine\$$new(); env <- engine\$$env\$$env; run_native_test_file('tests/native/$(FILE).arl', engine, env)"

#
## Coverage targets
#

.PHONY: coverage
coverage: coverage-r coverage-arl coverage-combined ## help: Run complete coverage analysis (R + Arl)

.PHONY: coverage-r
coverage-r: clean-cache stdlib-order ## help: Run R code coverage only
	Rscript tools/coverage/r-coverage.R

.PHONY: coverage-arl
coverage-arl: clean-cache stdlib-order ## help: Run Arl code coverage only
	Rscript tools/coverage/arl-coverage.R

.PHONY: coverage-combined
coverage-combined: ## help: Generate combined coverage summary
	Rscript tools/coverage/coverage-combine.R

.PHONY: coverage-report
coverage-report: ## help: Open coverage reports in browser
	@OPENER=; \
	if command -v open >/dev/null 2>&1; then OPENER=open; \
	elif command -v xdg-open >/dev/null 2>&1; then OPENER=xdg-open; \
	else echo "No 'open' or 'xdg-open' found"; exit 1; \
	fi; \
	if [ -f coverage/combined/index.html ]; then \
		$$OPENER coverage/combined/index.html; \
	elif [ -f coverage/r/index.html ]; then \
		$$OPENER coverage/r/index.html; \
		[ -f coverage/arl/index.html ] && $$OPENER coverage/arl/index.html; \
	else \
		echo "No coverage reports found. Run 'make coverage' first."; \
		exit 1; \
	fi

#
## Benchmarking and profiling
#

.PHONY: bench
bench: clean-cache stdlib-order ## help: Run all benchmarks
	R -q -e "devtools::load_all(); source('benchmarks/run-all-benchmarks.R')"

.PHONY: bench-component
bench-component: clean-cache stdlib-order ## help: Run single component benchmark (usage: make bench-component COMPONENT=tokenizer)
	@if [ -z "$(COMPONENT)" ]; then \
		echo "Error: COMPONENT parameter required. Options: tokenizer, parser, macro, eval, stdlib, e2e"; \
		exit 1; \
	fi
	R -q -e "devtools::load_all(); source('benchmarks/bench-$(COMPONENT).R')"

.PHONY: profile
profile: clean-cache stdlib-order ## help: Generate profiling reports
	R -q -e "devtools::load_all(); source('benchmarks/run-all-profiles.R')"

.PHONY: profile-component
profile-component: clean-cache stdlib-order ## help: Profile single component (usage: make profile-component COMPONENT=tokenizer)
	@if [ -z "$(COMPONENT)" ]; then \
		echo "Error: COMPONENT parameter required. Options: tokenizer, parser, macro, eval"; \
		exit 1; \
	fi
	R -q -e "devtools::load_all(); source('benchmarks/profile-$(COMPONENT).R')"

.PHONY: bench-compare
bench-compare: ## help: Compare benchmark results (usage: make bench-compare OLD=baseline.csv NEW=optimized.csv)
	@if [ -z "$(OLD)" ] || [ -z "$(NEW)" ]; then \
		echo "Error: OLD and NEW parameters required"; \
		exit 1; \
	fi
	R -q -e "source('benchmarks/compare-results.R'); compare_benchmarks('$(OLD)', '$(NEW)')"

#
## CRAN submission helper targets
#

.PHONY: cran
cran: devdoc readme vignettes cran-comments ## help: Run full CRAN prep/check/comments

.PHONY: cran-comments
cran-comments: check ## help: Generate cran-comments and CRAN-SUBMISSION
	Rscript tools/cran/cran_comments.R

#
## Cleanup
#

.PHONY: clean-cache
clean-cache: ## help: Remove .arl_cache directories (auto-runs before dev targets)
	find . -type d -name ".arl_cache" -exec rm -rf {} + 2>/dev/null || true

.PHONY: clean-coverage
clean-coverage: ## help: Remove coverage output files
	rm -rf coverage

.PHONY: clean-bench-profile
clean-bench-profile: ## help: Remove temporary benchmark / profile results objects
	rm -rf benchmarks/profiles/
	rm -rf benchmarks/results/

.PHONY: clean-cran
clean-cran: ## help: Remove CRAN check artifacts
	rm -rf arl.Rcheck

.PHONY: clean
clean: clean-cache clean-coverage clean-bench-profile clean-cran ## help: Remove build artifacts and all make document output
	rm -f arl_*.tar.gz
	rm -rf site/ doc/ Meta/
	rm -f README.knit.md
	rm -f vignettes/*.html vignettes/*.R vignettes/*.knit.md
	rm -f *.log
	# rm -f README.md  # version-controlled
	# rm -rf man/  # version-controlled

#
## Help
#

.PHONY: help
help: ## help: Show this help message
	@echo "Arl Development Commands"
	@echo ""
	@grep -E '^[a-zA-Z_-]+:.*?## help: .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## help: "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}'
