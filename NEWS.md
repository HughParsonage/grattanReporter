
* Checks now skips the second argument of chapter range cross-references
* Spell checks draw on a larger number of words via hunspell's add to dictionary (requires TeXCheckR 0.6.0)
* `biber` checks are now more lenient by using avoiding the validation flag

# grattanReporter 0.25.2
* Fix false pass from `check_CenturyFootnote` when multiple auxiliary files are legitimately produced (say from `\include`).
* New function `word2tex` for conversion from docx.
* Sentence-ending periods are not checked within `\hl`.
* Ghostscript-dependent functions now demand only what is required by `embedFonts`, rather than a specific environmental variable.

# grattanReporter 0.25.0
* Attorney-General's Department: basic validation of entries (#75)
* New function `compress_FrontPage` for crisper, smaller title pages.
* New function `fix_labels` to fix prefixes of labels.

# grattanReporter 0.24.0
* Permit reordering of recommended citation

# grattanReporter 0.23.1
* Use a more precise version of `inputs_of` to handle custom inputs like `\includenextfigure`

# grattanReporter 0.23.0

* Added a `NEWS.md` file to track changes to the package.
* `validate_bibliography()` now writes MD5 sum to travis/grattanReporter/md5 for speedier subsequent validation
* Added NEM capacity markets paper for validation


