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

This is not packaged yet in order to set that up, please::

    $ cabal sandbox init
    $ cabal install yaml
    $ cabal exec ghc yamlkeysdiff.hs

And you got it!


TODO
----

* Package it
* Use getopt
* Add support for unified diff (``--uniform`` in ``man diff``)
* Write a generic diff function which can diff many files
