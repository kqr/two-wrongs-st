NOTES
=====

1. Poop out an atom feed


Possible development: store images and static content with posts, serve shit
from the same domain!


Test running funcitons
----------------------

First-time installation

    $ git clone git@github.com:kqr/two-wrongs-st.git
    $ cd two-wrongs-st
    $ stack setup
    $ stack build
    $ cd example
    $ stack ghci
    Prelude> :load Main
    *Main> :main
    *Main> -- At this point you should have a freshly generated site in
    *Main> --     ./example/site

Repeat from `stack ghci` for any future runs.

If code has changed, run `:reload` inside GHCi to get new version of code.

If the code has changed a lot, repeat from `stack build` to ensure a fresh build.
