alphagov-backend.hs
===================

Drop-in replacements for [GOV.UK][] backend services, written in
Haskell using [servant][].

This is my firebreak project for end-of-Q1 2018

[GOV.UK]: https://www.gov.uk
[servant]: http://haskell-servant.readthedocs.io


Outline
-------

For a service `$SERVICE`:

- `$SERVICE/api`
  is a library defining its interface
- `$SERVICE/server`
  is an executable providing the server
- `$SERVICE/test`
  is an executable intended to be run against the server

The server and test executables are called `$SERVICE` and
`$SERVICE-test` respectively.

Build everything with `stack`:

    $ stack build

Run a server or testsuite with `stack`:

    $ stack exec $SERVICE &
    $ stack exec $SERVICE-test

Or directly:

    $ ./.stack-work/install/x86_64-linux/lts-11.15/8.2.2/bin/$SERVICE &
    $ ./.stack-work/install/x86_64-linux/lts-11.15/8.2.2/bin/$SERVICE-test


Running in the VM
-----------------

I'll figure this out when I get to it.
