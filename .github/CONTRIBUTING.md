# Contributing to Stack Builders Tutorials

First off, thanks for taking the time to contribute! :tada::+1:

The following is a set of guidelines for contributing to Stack Builders Tutorials, which are
hosted in the [Stack Builders Organization](https://github.com/stackbuilders) on
GitHub.
These are mostly just guidelines, not rules, use your best judgment and feel free
to propose changes to this document in a pull request.

## Contributing Guide

1. [Fork it](https://github.com/stackbuilders/tutorials/fork)
1. Create your tutorial branch (`git checkout -b my-new-tutorial`)
1. Add your tutorial changes following the [tutorial guide](#tutorial-creation-guide)
1. Commit your changes (`git commit -am 'Add some tutorial'`)
1. Push to the branch (`git push origin my-new-tutorial`)
1. Create a new Pull Request

## Tutorial Creation Guide

1. Create a new directory for your tutorial under `tutorials/<language>/`
1. Inside `tutorials/<language>/<your-tutorial-dir>` create a directory
named `code` and place your tutorial's code in there
1. Add a `tutorial.md` file under `tutorials/<language>/<your-tutorial-dir>`
with the steps required for accomplishing your tutorial. Use
[this tutorial](../tutorials/haskell/csv-encoding-decoding/tutorial.md) as a guide.
