# !/bin/csh
#
#	@(#)newversion.csh	2.2 (Berkeley) 83/02/06
#	update the version number of your argument makefile, etc.
#
if ( $#argv != 2 ) then
    echo "usage: /bin/csh newversion.csh whoami directory"
    exit(1)
endif
set whoami = $1
set dir = $2
set makefile = ${whoami}makefile
sccs edit $makefile
set oldversion = `grep "VERSION = " $makefile | sed "s/VERSION = //"`
cp $makefile /tmp/$$; awk -f newversion.awk < /tmp/$$ > $makefile ; rm /tmp/$$
set newversion = `grep "VERSION = " $makefile | sed "s/VERSION = //"`
echo version $oldversion becomes $newversion automagically.
sccs delta $makefile << EOF
version $oldversion becomes $newversion automagically.
EOF
rm -f $dir/$makefile
sccs get -p $makefile > $dir/$makefile
mv $dir/${whoami}${oldversion}strings $dir/${whoami}${newversion}strings
