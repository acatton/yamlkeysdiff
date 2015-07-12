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

Installation
------------

::

    $ cabal sandbox init  # Always use sandboxes (like python virtual env)
    $ cabal install --only-dependencies  # Install the package dependencies
    $ cabal configure
    $ cabal build
    $ ./dist/build/yamlkeysdiff/yamlkeysdiff


TODO
----

* Use getopt
* Add support for unified diff (``--uniform`` in ``man diff``)
* Write a generic diff function which can diff many files
