cc='/bin/cc'
test -f $cc || cc='/usr/ccs/bin/cc'
ldflags='-L/usr/ucblib'
mansrc='/usr/share/man/man1'
ccflags='-I/usr/include -I/usr/ucbinclude'
libswanted=`echo $libswanted | sed 's/ ucb/ c ucb/'`
