There are two versions of the paper, full and IFL 2016
submission. Both versions are produced from the same source.

To produce the full version:

    make full && make

To produce IFL 2016 version:

    make ifl && make

Which version is produced depends  on the contents of
`toggle.tex`, which is not in the repository, but copied by
`make` from either `toggle-ifl.tex` or `toggle-full.tex`.
