yesod-fay
=========

Utilities for using the Fay Haskell-to-JS compiler with Yesod.

For an example of a Yesod application with Fay integrated, have a look at the experimental Postgres-Fay (`pf`) application template. You can select it when running `yesod init` with recent versions of Yesod.


Usage with cabal sandboxes
--------------------------

You are highly encouraged to create a cabal sandbox for your Yesod app. This feature is available in cabal 1.18+.

Use the following command to set an environment variable; this is a workaround that is needed until cabal 1.20 is out. 

    export HASKELL_PACKAGE_SANDBOX=`echo .cabal-sandbox/*-packages.conf.d/`

Make sure to run this from the root of the Yesod project.
