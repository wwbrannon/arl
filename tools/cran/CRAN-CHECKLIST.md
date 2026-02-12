## CRAN submission checklist (automated)

Run the full sequence:

```
make cran
```

## What `make cran` does

Runs `devdoc`, `readme`, `vignettes`, and `cran-comments`. All prerequisite
targets (cache clearing, stdlib load-order rebuild, `devtools::load_all()`) are
handled automatically. The `cran-comments` target depends on `check` (builds
tarball + `R CMD check --as-cran`), reads the status from
`arl.Rcheck/00check.log`, and generates `cran-comments.md` and
`CRAN-SUBMISSION`.

To remove the `arl.Rcheck` directory when no longer needed, run `make
cran-clean` or `make clean`.

## Review before submission

1. Open `cran-comments.md` and remove/replace any placeholder note text.
2. Confirm `R CMD check` summary shows 0 errors, 0 warnings, 0 notes.
3. Ensure `NEWS.md` is current for the submitted version.
4. Verify licensing (`DESCRIPTION` uses `MIT + file LICENSE`,
   `LICENSE` has YEAR and COPYRIGHT HOLDER, `LICENSE.md` has full text).
