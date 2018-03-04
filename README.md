# gotest-ui-mode - ergonomic display for your go test results in Emacs

With the introduction of `go test -json` in go 1.10, it's finally
possible to make reasonable front-ends for running go tests, and emacs
is right here at the vanguard! This is, at heart, a mode for running
`go test`, and it does a bunch of things better than just running them
on the console. `gotest-ui-mode`:

* Shows the overall test status - `pass` or `fail` for the entire
  suite is displayed on the first line.

* Shows you the failing test cases at the very top, so you can start
  debugging any test failures even while your test suite is running.

* Has all tests' output available, folded away - and failing test
  cases' output is unfurled as soon as they fail.

* Deals with compiler errors when you invoke the project-wide test
  suite: packages that fail to compile appear just like regular
  compiler errors do, not buried in the middle of test output.

All that with link-ified `file.go:line` references, so you can quickly
jump to the cause of failures.

## Installation

This package isn't yet available on melpa, but you can install it via
[`straight`](https://github.com/raxod502/straight.el) quite easily:

``` elisp
(straight-use-package
 `(gotest-ui-mode :type git :repo "https://github.com/antifuchs/gotest-ui-mode.git"))
```

## Usage

`gotest-ui-mode` is strongly inspired by `gotest.el`, and so it
exports the following interactive functions for running tests:

* `gotest-ui-current-test` - runs the test function under `(point)`

* `gotest-ui-current-file` - runs all test functions in the current
  file, or in the "test file next door" - if you are in `foo.go` and
  there's a `foo_test.go`, it'll run all the test functions in that
  file.

* `gotest-ui-current-project` - runs all tests in the current project.

Each of these functions pops up a buffer with the test status. Inside
that buffer, you can do the following things:

* Hitting `TAB` on top of a test or package entry folds / unfolds its
  output.

* Clicking `file.go:NNN` links takes you to that file/line location.

## Get involved!

I really hope you find this emacs mode useful! If you do, I'd love if
you contribute back, in whichever way you can - be it docs, code, or
bug reports! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for
details!
