optimize=''
ccflags='-DDEBUGGING -A cpu,mathchip -W0,-opt,2'

cat <<'EOF'
Some tests may fail unless you use 'chacl -B'.  Also, op/stat
test 2 may fail occasionally because Apollo doesn't guarantee
that mtime will be equal to ctime on a newly created unmodified
file.  Finally, the sleep test will sometimes fail.  See the
sleep(3) man page to learn why.

And a note on ccflags:

    Lastly, while -A cpu,mathchip generates optimal code for your DN3500
    running sr10.3, be aware that you should be using -A cpu,mathlib_sr10
    if your perl must also run on any machines running sr10.0, sr10.1, or
    sr10.2.  The -A cpu,mathchip option generates code that doesn't work on
    pre-sr10.3 nodes.  See the cc(1) man page for more details.
						-- Steve Vinoski

EOF
