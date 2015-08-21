NOTES
=====

1. Read in blog articles from directories ./published and ./drafts
2. Filename has form "2015-08-20-dont-fear-the-bomb.txt" where the first part
   is the published date, and second part is the slug.
3. Put into list of posts
4. Poop out HTML for everything into ./site

Possible development: store images and static content with posts, serve shit
from the same domain!


Test running funcitons
----------------------

First-time installation

    $ git clone git@github.com:kqr/two-wrongs-st.git
    $ cd two-wrongs-st
    $ stack setup && stack build
    $ cd example
    $ stack ghci
    Prelude> :load Main
    *Main> :main
    [Post {title = "The First Post of the Blog!", slug = Slug "the-first-post-of-the-blog", datestamp = 1998-04-28, content = "\nHello! This is the first post of the blog. It's nice to get started.\n", author = (), tags = []},Post {title = "Valid Slug Test", slug = Slug "valid-slug-test", datestamp = 1999-05-04, content = "\nWell this is the second post\n", author = (), tags = []}]

Repeat from `stack ghci` for any future runs.

If code has changed, run `:reload` inside GHCi to get new version of code.
