The purpose of `opamcheck` is to automate the task of checking
experimental/development versions of the OCaml compilers on a bunch of
OPAM packages.

To this end, we run `opam` in a loop, trying to install all the
(available) packages one after the other.

In order to get deterministic behavior, `opam` is isolated from the
network by a sandbox, composed of:
- a clone of `opam-repository`
- a special wrapper around `curl` that caches all download results


This new version is still under construction. Its driver is an OCaml program
instead of a bunch of bash and awk scripts.
