This is a Literate Programming fork of the original Emacs Starter Kit

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

Previously there was an attempt to test loading these scripts to 
https://travis-ci.org. This was constantly failing due to problems
with the third-party packages being loaded.

TODO: Develop tests that are less dependent on loading external
packages. Substitute our own "stub" packages for testing?

The latest version is at http://github.com/flanagan/emacs24-starter-kit.
The branch lloyd-custom is most up to date, but also the most
specific to a single user.

For more information including use and installation instructions see
http://eschulte.github.com/emacs24-starter-kit/
