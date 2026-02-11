## CRAN submission checklist (automated)

Run the full sequence:

```
make cran
```

Or run each step:

```
make cran-prep
make cran-check
make cran-comments
```

## What each target does

- `cran-prep`: runs `devtools::document()`, renders `README.Rmd`, builds vignettes,
  then performs `R CMD check --as-cran`.
- `cran-check`: runs `R CMD check --as-cran` only.
- `cran-comments`: generates `cran-comments.md` and `CRAN-SUBMISSION`.
- `cran-clean`: removes `rye.Rcheck` and `tools/cran/cran_check_summary.txt`.

## Review before submission

1. Open `cran-comments.md` and remove/replace any placeholder note text.
2. Confirm `R CMD check` summary shows 0 errors, 0 warnings, 0 notes.
3. Ensure `NEWS.md` is current for the submitted version.
4. Verify licensing (`DESCRIPTION` uses `MIT + file LICENSE`,
   `LICENSE` has YEAR and COPYRIGHT HOLDER, `LICENSE.md` has full text).
