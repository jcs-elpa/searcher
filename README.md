[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/searcher-badge.svg)](https://melpa.org/#/searcher)
[![MELPA Stable](https://stable.melpa.org/packages/searcher-badge.svg)](https://stable.melpa.org/#/searcher)
[![CI](https://github.com/jcs-elpa/searcher/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/searcher/actions/workflows/test.yml)

# searcher
> Searcher in pure elisp

Similar to `ag`, `rg`, `grep`, etc. But written in pure elisp.

## Searching Algorithm

You can change the search type by tweaking the variable `searcher-search-type`
to one of the following value.

* `regex` - Use normal regular expression by default.
* `regex-fuzzy` - Fuzzy regular expression from [ivy--regex-fuzzy](https://github.com/abo-abo/swiper/blob/b65e401c22ec56a008b00f651cd9536caf593d43/ivy.el#L2906).
* `flx` - Fuzzy searching the same as `regex-fuzzy` and uses [flx](https://github.com/lewang/flx)
to score the candidates.
(You would need to install [flx](https://github.com/lewang/flx))

If you think the fuzzy is too fuzzy and not perform accuracy. Try to raise/lower
`searcher-flx-threshold` value depends on the scoring you want.

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
