YAMLKeysDiff
============

Command line tools to generate a fast and human readable diff::


    $ cat a.yml
    a: hello
    b: world
    d:
        a: a
        b: b
        A: A
    $ cat b.yml
    a: world
    c: hello
    d:
        a: a
        b: b
        c: c
    $ yamlkeysdiff a.yml b.yml
    > b
    < c
    > d:A
    < d:c
    $ yamlkeysdiff "a.yml#d" "b.yml#d"
    > A
    < c

Installation
------------

You can install it from hackage::

    $ cabal update
    $ cabal install yamlkeysdiff

Development
------------

::

    $ cabal sandbox init  # Always use sandboxes (like python virtual env)
    $ cabal install --only-dependencies  # Install the package dependencies
    # if this fails, try:
    # cabal install --only-dependencies --force-reinstalls
    $ cabal configure
    $ cabal build
    $ ./dist/build/yamlkeysdiff/yamlkeysdiff

You can also do that with::

    $ make all


TODO
----

* Add tests
* Don't compare the subkeys when the parent key is missing
* Write a generic diff function which can diff many files
