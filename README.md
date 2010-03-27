Different
---------

This little code project implements longest common subsequence in Erlang, it
is the foundation of finding the optimal diff between two files.

    $ cat > a
    monkey
    cat
    dog
    bicycle
    horse
    cow
    pig
    $ grep -v bicycle < a > b

That will create two files, the other without the 'bicycle' line in it.

    $ erl
    1> Patch = differlent:unified_files("a", "b", 2).
    2> file:write_file("patch", Patch).
    3>

That will create an iolist and write it to a file. It is the patch presented as a unified diff format.

    $ cat patch
    --- a
    +++ b
    @@ -2,5 +2,4 @@
     cat
     dog
    -bicycle
     horse
     cow
    $ diff -U 2 a b
    --- a   2010-03-27 19:03:09.483175230 +0100
    +++ b   2010-03-27 19:03:19.099174920 +0100
    @@ -2,5 +2,4 @@
     cat
     dog
    -bicycle
     horse
     cow

As you can see, it produces (nearly) the same output as unix diff presents a unified diff. I used two lines of context.


