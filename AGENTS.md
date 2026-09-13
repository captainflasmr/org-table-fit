# AGENTS.md

## Repo shape
- This repo is a single Emacs Lisp package, not a multi-package app.
- The implementation lives in `org-table-fit.el`; the regression suite lives in `test/org-table-fit-test.el`.
- `org-table-fit.el` declares `Package-Requires: ((emacs "29.1"))` and depends on `org` at runtime.
- `org-table-fit.elc` is a generated compiled artifact; edit the `.el` source, not the bytecode file.

## Verification command
- Use the repo’s real test entrypoint:
  - `emacs -Q --batch -L . -l test/org-table-fit-test.el`
- This is the source of truth for correctness in this repo; there is no separate `npm test`, `make`, or lint target.
- The suite exits nonzero if any test fails; the current pass condition is `0 failure(s)`.

## Architecture and workflow
- The main public commands are `org-table-fit-window` and `org-table-fit-unwrap`, both implemented in `org-table-fit.el`.
- `org-table-fit-window` fits the table at point to the current window/body width, optionally using a numeric width argument.
- If the table was already wrapped by this package, it merges continuation rows before refitting; this is a key correctness path.
- `org-table-fit-unwrap` restores original single-line rows and clears the package’s internal text properties.
- Formula cells such as `=1+2` and verbatim Org spans such as `=C-x C-s=` are intentionally preserved and must not be broken incorrectly.

## When changing code
- If you alter wrapping logic, row-merging behavior, width allocation, or table rendering, update or extend `test/org-table-fit-test.el` to cover the behavior.
- Prefer the existing helper functions and conventions in `org-table-fit.el` over introducing new APIs or new test harnesses.
- Keep behavior scoped to Org tables at point; avoid broad Emacs global side effects.
