# Emacs Starter Kit

This is a Literate Programming fork of the original Emacs Starter Kit

*Note that the original maintainers of the Emacs Starter Kit are no
longer maintaining it, as it has been their experience that emacs
users are better off developing their own init setup rather than
attempting to use (and understand) a pre-packaged solution.*

This author mostly agrees with that, but there are two important
reasons for this project to exist:

1. I use it myself. I can clone the project to a new .emacs.d
   directory on a new machine, and automatically do all the setup and
   package installation to get the environment I'm used to.

2. It's useful to beginners to see how other's emacs setup is done, so
   they can learn (and steal code) for their own setup. With the
   literate programming made possible by .org files, this can be a
   valuable teaching tool.

*So I encourage you to use it in that spirit, rather than installing
it and blindly accepting all the setup it does. For one thing, unless
you program in the same languages I do, many of the packages may not
be useful to you.*

This should provide a saner set of defaults than you get normally with
Emacs. It's intended for beginners, but provides a good elisp
initialization structure for all levels of users.

The main advantages of this Emacs Starter Kit are

- better default settings
- easy loading of many useful libraries and configurations
- "literate" customization embedded in Org-mode files
- an organizational directory structure
- git provides for version control, backup, and sharing
- easy installation with no need to compile external packages

The latest version is on
[github](http://github.com/flanagan/emacs24-starter-kit).  The branch
[lloyd-custom](http://github.com/flanagan/emacs24-starter-kit/tree/lloyd-custom)
is most up to date, but also the most specific to a single user.

For more information including use and installation instructions see
[the original wiki](http://eschulte.github.com/emacs24-starter-kit/)
for the upstream repository.

Previously there was an attempt to test loading these scripts to
[Travis CI](https://travis-ci.org "Online Continuous Integration Service").
This was constantly failing due to problems with the third-party packages
being loaded.

TODO: Develop tests that are less dependent on loading external
packages. Substitute our own "stub" packages for testing?

> Note that this document is intended to conform to the
> [CommonMark specification](http://spec.commonmark.org/
> "A formal Markdown specification").

