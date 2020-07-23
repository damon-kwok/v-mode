# Contributing to v-mode

Hi there! Thanks for your interest in contributing to `v-mode`.

Please note, that by submitting changes to `v-mode`, you are agreeing that
it can be licensed under our license as seen in `v-mode.el`. Furthermore,
you are testifying that you own the copyright to the submitted changes and
indemnify `v-mode` from any copyright claim that might result from your not
being the authorized copyright holder.

## Code formatting basics

* 2 Spaces not tabs for indentation
* No lines over 80 columns

## How to submit a pull request

If you have improvements to Ponylang-mode, please open a pull request against
this repo. Based on the state of your particular PR, we might request changing
to comply with formatting or the content of the change itself.

Be sure to keep your PR to a single change. If you are working on multiple
changes, make sure they are each on their own branch and that before creating a
new branch that you are on the master branch (others multiple patterns might
end up in your pull request). Each PR should be for a single logical change. We
request that you create a good commit messages as laid out in
['How to Write a Git Commit Message'](http://chris.beams.io/posts/git-commit/).

If your PR is for a single logical change (which is should be) but spans
multiple commits, we'll ask you to squash them into a single commit before we
merge. Steve Klabnik wrote a handy guide for that:
[How to squash commits in a GitHub pull request](http://blog.steveklabnik.com/posts/2012-11-08-how-to-squash-commits-in-a-github-pull-request).

Note that changes that are meaningful to end users of the mode are kept in our
[CHANGELOG](CHANGELOG.md). Please make sure you update it accordingly with when
submitting your PR.
