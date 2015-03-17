The purpose of `opamcheck` is to automate the task of checking
experimental/development versions of the OCaml compilers on a bunch of
OPAM packages.

To this end, we run opam in a loop, trying to install all the
(available) packages one after the other.

In order to get deterministic behavior, `opam` is isolated from the
network by a sandbox, composed of:
- a clone of `opam-repository`
- a special wrapper around `curl` that caches all download results


File hierarchy
--------------

opamcheck          this is the main directory, everything is under it
                   OPAMCHECKDIR points to this directory
  bin              the opamcheck scripts, should be in PATH
    opamcheck      main script
    opamcheck-opam   `opam` wrapper
    opamcheck-curl   `curl` wrapper
    opamcheck-reinit auxiliary script
    opamcheck-env    helper to set environment variables
    opamcompare      work in progress
  camlp4           a directory with the camlp4 sources
    4.02.1a        the default camlp4 version (will be copied to x as needed)
    x              camlp4 for OCaml version x
  control          create files here to control the script
    stop-x         stop running version x
    suspend-x      suspend the run, resume when the file is removed
  curl-cache       contains all the files ever downloaded by opamcheck-curl
  dot_opam         .opam directories for each version
    x
  etc              config files
    all-packages     list of packages to install
    exclude-packages list of packages to exclude
  install          the OCaml and camlp4 versions will be installed here
    x
  log              log files
    opam           log files for opam runs
    opamcheck      log files for opamcheck runs
  ocaml            a directory that contains various version of the OCaml sources
  opam             a clone (and build) of the `opam` sources
  opam-repository  a clone of the opam repository
  tmp              temp files
  


INSTALL
=======
- Clone the opam sources into `opam`. Configure and compile, but do
  not install
- Clone the opam repository into `opam-repository`
- Check out some version of OCaml in a sub-directory of `ocaml`
    the name of this sub-directory will be the version name for this
    version of OCaml
- Clone the `camlp4` git repo at a reasonable version into camlp4/default
   git clone https://github.com/ocaml/camlp4.git default
   (cd default; git checkout 0dd57037fbe6918281625e2022a675c801ff0b91)
