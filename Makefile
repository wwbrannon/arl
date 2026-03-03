SHELL := /bin/bash

#
## Build/install targets
#

.PHONY: clean-cache
clean-cache: ## help: Remove module cache under R_user_dir (auto-runs before dev targets)
	Rscript -e 'unlink(file.path(tools::R_user_dir("arl", "cache"), "modules"), recursive = TRUE)'

# Keep inst/arl/load-order.txt up to date (must run before build/install)
.PHONY: stdlib-order
stdlib-order: ## help: Build stdlib load order cache (inst/arl/load-order.txt)
	Rscript tools/build-stdlib-order.R

.PHONY: bench-data
bench-data: ## help: Check out benchmark data from gh-pages branch
	@mkdir -p benchmarks/results
	@git show gh-pages:dev/bench/data.js > benchmarks/results/data.js 2>/dev/null \
		|| echo "Warning: could not fetch benchmark data from gh-pages branch"

.PHONY: roxygen
roxygen: clean-cache stdlib-order ## help: Generate man/ pages and NAMESPACE from roxygen2
	R -q -e "devtools::document()"

.PHONY: lang-docs
lang-docs: clean-cache stdlib-order ## help: Generate stdlib reference vignettes from .arl source
	R -q -e "devtools::load_all(); source('tools/docs/generate-lang-docs.R')"

.PHONY: readme
readme: clean-cache stdlib-order ## help: Render README from README.Rmd
	R -q -e "devtools::load_all(); rmarkdown::render('README.Rmd')"

.PHONY: vignettes
vignettes: lang-docs bench-data ## help: Build vignettes
	R -q -e "devtools::build_vignettes()"

.PHONY: document
document: roxygen lang-docs readme vignettes ## help: Generate all documentation

.PHONY: install
install: clean-cache stdlib-order ## help: Install the package
	R -q -e "devtools::install()"

.PHONY: build
build: roxygen lang-docs ## help: Build the package tarball
	R -q -e "devtools::build(path='.')"

#
## Test running, lint, R CMD check
#

.PHONY: check
check: build ## help: Check the package (includes tests)
	R -q -e 'p <- read.dcf("DESCRIPTION"); tb <- sprintf("%s_%s.tar.gz", p[1,"Package"], p[1,"Version"]); devtools::check_built(tb, args=c("--as-cran","--run-donttest"), check_dir=".")'

# Extra env vars beyond what --as-cran already sets, to match CRAN's
# incoming-check configuration more closely. See "R Internals" manual,
# section 8 (Tools) for the full list of _R_CHECK_ variables.
# --as-cran already turns on ~40 variables including temp-dir checks,
# URL validation, bashisms, orphaned deps, etc. The variables below are
# the ones CRAN uses that --as-cran does NOT set.
CRAN_CHECK_ENV := \
	_R_CHECK_FORCE_SUGGESTS_=FALSE \
	_R_CHECK_LENGTH_1_CONDITION_=abort,verbose \
	_R_CHECK_LENGTH_1_LOGIC2_=abort,verbose \
	_R_CHECK_EXCESSIVE_IMPORTS_=20

.PHONY: check-cran
check-cran: build ## help: Check the package replicating CRAN's incoming checks
	$(CRAN_CHECK_ENV) R -q -e 'p <- read.dcf("DESCRIPTION"); tb <- sprintf("%s_%s.tar.gz", p[1,"Package"], p[1,"Version"]); devtools::check_built(tb, args=c("--as-cran","--run-donttest"), check_dir=".")'

.PHONY: lint
lint: clean-cache stdlib-order ## help: Run linter checks
	R -q -e "devtools::load_all(); lintr::lint_dir(path='.')"
	shellcheck inst/bin/posix/arl

.PHONY: test
test: clean-cache stdlib-order lang-docs ## help: Run tests
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

.PHONY: coverage-test-file
coverage-test-file: clean-cache stdlib-order ## help: Run a single test file with Arl coverage instrumentation (usage: make coverage-test-file FILE=test-parser)
	@if [ -z "$(FILE)" ]; then \
		echo "Error: FILE parameter required. Usage: make coverage-test-file FILE=test-parser"; \
		exit 1; \
	fi
	Rscript tools/coverage/run-test-file.R "tests/testthat/$(FILE).R"

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
		echo "Error: COMPONENT parameter required. Options: tokenizer, parser, macro, compile, r-eval, stdlib, e2e"; \
		exit 1; \
	fi
	R -q -e "devtools::load_all(); source('benchmarks/bench-$(COMPONENT).R')"

.PHONY: profile
profile: clean-cache stdlib-order ## help: Generate profiling reports
	R -q -e "devtools::load_all(); source('benchmarks/run-all-profiles.R')"

.PHONY: profile-component
profile-component: clean-cache stdlib-order ## help: Profile single component (usage: make profile-component COMPONENT=tokenizer)
	@if [ -z "$(COMPONENT)" ]; then \
		echo "Error: COMPONENT parameter required. Options: tokenizer, parser, macro, compile, r-eval"; \
		exit 1; \
	fi
	R -q -e "devtools::load_all(); source('benchmarks/profile-$(COMPONENT).R')"

.PHONY: bench-publish
bench-publish: ## help: Publish benchmark results to gh-pages branch (run after make bench)
	@if [ ! -f benchmarks/results/benchmark-results.json ]; then \
		echo "Error: benchmarks/results/benchmark-results.json not found. Run 'make bench' first."; \
		exit 1; \
	fi
	@benchmarks/publish-results.sh

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
cran: check-cran ## help: Run full CRAN prep/check/comments
	Rscript tools/cran/comments.R
	@echo "You should also make check-winbuilder and check-macbuilder targets"

.PHONY: check-winbuilder
check-winbuilder: ## help: Submit to win-builder (devel + release)
	R -q -e "devtools::check_win_devel()"
	R -q -e "devtools::check_win_release()"

.PHONY: check-macbuilder
check-macbuilder: ## help: Submit to mac-builder (release)
	R -q -e "devtools::check_mac_release()"

#
## Pkgdown site
#

.PHONY: site
site: clean-cache stdlib-order lang-docs bench-data ## help: Build pkgdown site
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

#
## Cleanup
#

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
	rm -f vignettes/articles/*.html vignettes/articles/*.knit.md
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
