NOTES
=====

1. Read in blog articles from directories ./published and ./drafts
2. Filename has form "2015-08-20-dont-fear-the-bomb.txt" where the first part
   is the published date, and second part is the slug.
3. Put into list of posts
4. Check for slug collisions!!
5. Poop out HTML for everything into ./site

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
