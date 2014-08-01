yesod-fay
=========

Utilities for using the [Fay](https://github.com/faylang/fay/wiki) Haskell-to-JS compiler with [Yesod](http://www.yesodweb.com).

For an example of a Yesod application with Fay integrated, have a look at the `sample/` directory in this repository, or alternatively use the experimental Postgres-Fay (`pf`) application template (select it when running `yesod init` with recent versions of Yesod).


Usage with cabal sandboxes
--------------------------

You are highly encouraged to create a cabal sandbox for your Yesod app. This feature is available in cabal 1.18+.

Use the following command to set an environment variable (this is a workaround, needed until Fay integrates with [haskell-packages](http://hackage.haskell.org/package/haskell-packages), only possible after cabal 1.20 is released). 

    export HASKELL_PACKAGE_SANDBOX=`echo .cabal-sandbox/*-packages.conf.d/`

Make sure to run this from the root of the Yesod project.
