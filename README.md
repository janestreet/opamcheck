The purpose of `opamcheck` is to automate the task of checking
experimental/development versions of the OCaml compilers on a bunch of
OPAM packages.

To this end, we run `opam` in a loop, trying to install all the
(available) packages one after the other.

In order to get deterministic behavior, `opam` is isolated from the
network by a sandbox, composed of:
- a clone of `opam-repository`
- a special wrapper around `curl` that caches all download results


File hierarchy
--------------

* `opamcheck`          this is the main directory, everything is under it
                       `OPAMCHECKDIR` points to this directory
  * `bin`              the `opamcheck` scripts, should be in your `PATH`
    * `opamcheck`      main script
    * `opamcheck-opam`   `opam` wrapper
    * `opamcheck-curl`   `curl` wrapper
    * `opamcheck-reinit` auxiliary script
    * `opamcheck-env`    helper to set environment variables
    * `opamcompare`      work in progress
  * `control`          create files here to control the script
    * `stop-`*x*         stop running version *x*
    * `suspend-`*x*      suspend the run, resume when the file is removed
  * `curl-cache`       contains all the files ever downloaded by opamcheck-curl
  * `dot-opam`         `.opam` directories for each version
    * *x*
  * `etc`              config files
    * `all-packages`     list of packages to install
    * `exclude-packages` list of packages to exclude
  * `install`          the OCaml and `camlp4` versions will be installed here
    * *x*
  * `log`              log files
    * `opam`           log files for `opam` runs
    * `opamcheck`      log files for `opamcheck` runs
  * `ocaml`            a directory that contains various version of the OCaml sources
  * `opam`             a clone (and build) of the `opam` sources
  * `opam-repository`  a clone of the OPAM repository
  * `tmp`              temp files
  


INSTALL
=======
- Clone the [opam sources](https://github.com/ocaml/opam) into `opam`. Configure (no need to set the prefix)
  and compile, but do not install:
```
   git clone https://github.com/ocaml/opam
   (cd opam && ./configure && make lib-ext && make)
```
- Clone the [opam repository](https://github.com/ocaml/opam-repository) into `opam-repository`:
```
   git clone https://github.com/ocaml/opam-repository
```
- Check out some version of [OCaml](https://github.com/ocaml/ocaml.git) in a sub-directory of `ocaml`
    the name of this sub-directory will be the version name for this
    version of OCaml:
```
   git clone https://github.com/ocaml/ocaml.git ocaml/trunk
```
