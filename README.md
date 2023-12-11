[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/searcher-badge.svg)](https://melpa.org/#/searcher)
[![MELPA Stable](https://stable.melpa.org/packages/searcher-badge.svg)](https://stable.melpa.org/#/searcher)

# searcher
> Searcher in pure elisp

[![CI](https://github.com/jcs-elpa/searcher/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/searcher/actions/workflows/test.yml)

Similar to `ag`, `rg`, `grep`, etc. But written in pure elisp.

## 💡 Searching Algorithm

You can change the search type by tweaking the variable `searcher-search-type`
to one of the following value.

* `regex` - Use normal regular expression by default.
* `regex-fuzzy` - Fuzzy regular expression from [ivy--regex-fuzzy](https://github.com/abo-abo/swiper/blob/b65e401c22ec56a008b00f651cd9536caf593d43/ivy.el#L2906).
* `flx` - Fuzzy searching the same as `regex-fuzzy` and uses [flx](https://github.com/lewang/flx)
to score the candidates.
(You would need to install [flx](https://github.com/lewang/flx))

If you think the fuzzy is too fuzzy and not perform accuracy. Try to raise/lower
`searcher-flx-threshold` value depends on the scoring you want.

## 🛠️ Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!

### 🔬 Development

To run the test locally, you will need the following tools:

- [Eask](https://emacs-eask.github.io/)
- [Make](https://www.gnu.org/software/make/) (optional)

Install all dependencies and development dependencies:

```sh
$ eask install-deps --dev
```

To test the package's installation:

```sh
$ eask package
$ eask install
```

To test compilation:

```sh
$ eask compile
```

**🪧 The following steps are optional, but we recommend you follow these lint results!**

The built-in `checkdoc` linter:

```sh
$ eask lint checkdoc
```

The standard `package` linter:

```sh
$ eask lint package
```

*📝 P.S. For more information, find the Eask manual at https://emacs-eask.github.io/.*

## ⚜️ License

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

See [`LICENSE`](./LICENSE.txt) for details.
