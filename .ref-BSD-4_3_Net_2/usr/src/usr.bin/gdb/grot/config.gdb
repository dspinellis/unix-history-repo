#!/bin/sh

#
# Shell script to do machine-dependent things in
# preparation for compiling gdb.
#
# Usage: config.gdb machine [operating-system]
#
# If config.gdb succeeds, it leaves its status in config.status.
# If config.gdb fails after disturbing the status quo, 
# 	config.status is removed.
#
# Note: if making something gives errors like "unable to make target
# 1-dep.c", this means that the compiler has put in a builtin define
# and there is a "-U" missing from makedefine in this file.
#

progname=$0

case $# in 
1)
	machine=$1
	os=""
	;;
2)
	machine=$1
	os=$2
	;;
*)
	echo "Usage: $progname machine [operating-system]"
	echo "Available machine types:"
	echo m-*.h | sed 's/m-//g' | sed 's/\.h//g'
	if [ -r config.status ]
	then
		cat config.status
	fi
	exit 1
	;;
esac

# cannonicalize the machine name.
case $machine in
news)
	case $os in
	os3|newsos3)
		$machine=newsos3
		$os=""
		;;
	esac
	;;
sun2)
	case $os in
	os4|sunos4)
		machine=sun2os4
		os=""
	;;
	os2|sunos2)
		machine=sun2os2
		os=""
		;;
	esac
	;;
sun3)
	case $os in
	os4|sunos4)
		machine=sun3os4
		os=""
		;;
	os3|sunos3)
		machine=sun3os3
		os=""
		;;
	*)
# Arguably, the default should be sun3os4, but in that case we'd want
# to change the list of machine types given by "config.gdb" so it
# doesn't list "sun3 sun3os4".
		machine=sun3os3
		os=""
		;;
	esac
	;;
sparc|sun4)
	case $os in
	os4|sunos4)
		machine=sun4os4
		os=""
		;;
	*)
# Arguably, the default should be sun4os4, but in that case we'd want
# to change the list of machine types given by "config.gdb" so it
# doesn't list "sun4 sun4os4".
		machine=sun4os3
		os=""
		;;
	esac
	;;
# GCC accepts sequent-i386 or symmetry, so be consistent.
sequent-i386)
	machine=symmetry
	os=""
	;;
esac

paramfile=m-${machine}${os}.h
pinsnfile=${machine}${os}-pinsn.c
opcodefile=${machine}${os}-opcode.h
# Set up a define for purposes of editing the makefile.
makedefine=
if [ -r ${machine}${os}-dep.c ]
then
	depfile=${machine}${os}-dep.c
else
	depfile=default-dep.c
fi

#
# Special cases.
# If a file is not needed, set the file name to something.
# It must be linked to something, or else make will try to make it.
# /dev/null will not work because on systems without symbolic links,
# it will complain that /dev/null is on a different file system.
#
case $machine in
altos)
	makedefine="-DM_REGEX=regex.o -DM_SYSV -DM_BSD_NM"
	pinsnfile=m68k-pinsn.c
	opcodefile=m68k-opcode.h
	;;
altosgas)
	echo "Use of the coff encapsulation features require the GNU binutils utilities"
	echo "To be ahead of their System V counterparts in your path."
	makedefine="-DM_REGEX=regex.o -DM_SYSV -DM_BSD_NM"
	pinsnfile=m68k-pinsn.c
	depfile=altos-dep.c
	opcodefile=m68k-opcode.h
	;;
pyramid)
	echo
	echo "Note that GDB on Pyramids only works with GCC."
	echo
	;;
vax)
	echo
# The following types of /bin/cc failures have been observed:
# 1.  Something in readline.c which I have never seen
# 2.  ``"values.c", line 816: compiler error: schain botch''
	echo "/bin/cc has been known to fail on VAXen running BSD4.3"
	echo "If this occurs, use gcc "
	echo " (but see comments in Makefile.dist about compiling with gcc)."
	echo
	pinsnfile=vax-pinsn.c
	opcodefile=vax-opcode.h
	;;
hp9k320)
# The headers in the directory hp-include override system headers
# and tell GDB to use BSD executable file format (hence -Ihp-include)
	makedefine="-DM_SYSV -DM_BSD_NM -DM_REGEX=regex.o 
		-DM_ALLOCA=alloca.o -DM_CFLAGS=-Ihp-include"
# The following is true because gcc uses a different .o file format
# than the native HPUX compiler
	echo
	echo "If you compile GDB with GCC on HPUX, you must make sure"
	echo "that the \`nm' used in \`munch' is GNU nm"
	echo
	pinsnfile=m68k-pinsn.c
	opcodefile=m68k-opcode.h
	;;
hp300bsd)
	pinsnfile=m68k-pinsn.c
	opcodefile=m68k-opcode.h
	;;
isi)
# ISI running bsd4.2
	pinsnfile=m68k-pinsn.c
	opcodefile=m68k-opcode.h
	;;
i386)
	makedefine="-DM_CLIBS=-lPW -DM_SYSV -DM_REGEX=regex.o"
# The following is a lie, but a necessary one.  See comment at beginning
# of this file about unneeded files.
	opcodefile=m-i386.h
	;;
i386gas)
	makedefine="-DM_CLIBS=-lPW -DM_SYSV -DM_REGEX=regex.o"
	echo
	echo "Use of the coff encapsulation features requires the GNU binary utilities"
	echo "to be ahead of their System V counterparts in your path."
	echo
	pinsnfile=i386-pinsn.c
	depfile=i386-dep.c
# The following is a lie, but a necessary one.  See comment at beginning
# of this file about unneeded files.
	opcodefile=m-i386.h
	;;
# These names are short and cryptic due to the @#$#!@#$@! System V
# 14 character file name limit.
i386-sv32)
	makedefine="-DM_CLIBS=-lPW -DM_SYSV -DM_REGEX=regex.o"
	pinsnfile=i386-pinsn.c
	depfile=i386-dep.c
# The following is a lie, but a necessary one.  See comment at beginning
# of this file about unneeded files.
	opcodefile=m-i386.h
	;;
i386g-sv32)
	makedefine="-DM_CLIBS=-lPW -DM_SYSV -DM_REGEX=regex.o"
	echo
	echo "Use of the coff encapsulation features requires the GNU binary utilities"
	echo "to be ahead of their System V counterparts in your path."
	echo
	pinsnfile=i386-pinsn.c
	depfile=i386-dep.c
# The following is a lie, but a necessary one.  See comment at beginning
# of this file about unneeded files.
	opcodefile=m-i386.h
	;;
merlin)
	echo ""
	echo "To install GDB on this machine you must copy /bin/sh"
	echo "to /usr/local/lib/gdb-sh, and make it world readable"
	echo "and writeable.  For example:"
	echo "    cp /bin/sh /usr/local/lib/gdb-sh"
	echo "    chmod ogu+rw /usr/local/lib/gdb-sh"
	echo "If you want to put it somewhere other than /usr/local/lib,"
	echo "edit the definition of SHELL_FILE in m-merlin.h"
	echo ""
	pinsnfile=ns32k-pinsn.c
	opcodefile=ns32k-opcode.h
	;;
news)
	pinsnfile=m68k-pinsn.c
	opcodefile=m68k-opcode.h
	;;
newsos3)
	pinsnfile=m68k-pinsn.c
	opcodefile=m68k-opcode.h
	depfile=news-dep.c
	;;
npl)
	pinsnfile=gld-pinsn.c
	;;
pn)
	pinsnfile=gld-pinsn.c
	;;
sun2)
	depfile=sun3-dep.c
	pinsnfile=m68k-pinsn.c
	opcodefile=m68k-opcode.h
	;;
sun2os2|sun2-os2)
	depfile=default-dep.c
	paramfile=m-sun2.h
	pinsnfile=m68k-pinsn.c
	opcodefile=m68k-opcode.h
	;;	
sun2os4|sun2-os4)
# Compile GDB without shared libraries so that it can be run on itself.
# -Bstatic is the right flag for cc.
# For gcc, -Bstatic is (probably) a no-op, and -g (which is specified by
#  Makefile.dist prevents use of shared libraries).
	makedefine=-DM_CFLAGS=-Bstatic
	echo
	echo "Make sure to compile any program on which you want to run gdb"
	echo " without shared libraries (cc -Bstatic)"
	echo
	paramfile=m-sun2os4.h
	depfile=sun3-dep.c
	pinsnfile=m68k-pinsn.c
	opcodefile=m68k-opcode.h
	;;	
sun3os3)
	paramfile=m-sun3.h
	depfile=sun3-dep.c
	pinsnfile=m68k-pinsn.c
	opcodefile=m68k-opcode.h
	;;
sun3os4|sun3-os4)
# Compile GDB without shared libraries so that it can be run on itself.
	makedefine=-DM_CFLAGS=-Bstatic
	echo
	echo "Make sure to compile any program on which you want to run gdb"
	echo " without shared libraries (cc -Bstatic)"
	echo
	paramfile=m-sun3os4.h
	pinsnfile=m68k-pinsn.c
	opcodefile=m68k-opcode.h
	depfile=sun3-dep.c
	;;	
sun4os4|sun4-os4)
# Compile GDB without shared libraries so that it can be run on itself.
# Undefine sparc to avoid changing sparc-dep.c to 1-dep.c
	makedefine="-DM_CFLAGS=-Bstatic -Usparc"
	echo
	echo "Make sure to compile any program on which you want to run gdb"
	echo " without shared libraries (cc -Bstatic)"
	echo
	paramfile=m-sun4os4.h
	pinsnfile=sparc-pinsn.c
	opcodefile=sparc-opcode.h
	depfile=sparc-dep.c
	;;	
symmetry)
	paramfile=m-symmetry.h
	depfile=symmetry-dep.c
	pinsnfile=i386-pinsn.c
# The following is a lie, but a necessary one.  See comment at beginning
# of this file about unneeded files.
	opcodefile=m-i386.h
	;;
umax)
	pinsnfile=ns32k-pinsn.c
	opcodefile=ns32k-opcode.h
	;;
sparc|sun4|sun4os3|sun4-os3)
	paramfile=m-sparc.h
# Undefine sparc to avoid changing sparc-dep.c to 1-dep.c
	makedefine=-Usparc
	pinsnfile=sparc-pinsn.c
	opcodefile=sparc-opcode.h
	depfile=sparc-dep.c
	;;
convex)
	;;
test)
	paramfile=one
	pinsnfile=three
	opcodefile=four
	;;
*)
	echo "Unknown machine type: \`$machine'"
	echo "Available types:"
	echo m-*.h | sed 's/m-//g' | sed 's/\.h//g'
	exit 1
	;;
esac

files="$paramfile $pinsnfile $opcodefile $depfile"
links="param.h pinsn.c opcode.h dep.c"	

rm -f config.status
while [ -n "$files" ]
do
	# set file to car of files, files to cdr of files
	set $files; file=$1; shift; files=$*
	set $links; link=$1; shift; links=$*

	if [ "$file" != skip ]
	then
		if [ ! -r $file ]
		then
			echo "$progname: cannot create a link \`$link',"
			echo "since the file \`$file' does not exist."
			exit 1
		fi

		echo "Linking \`$link' to \`$file'."
		rm -f $link
		# Make a symlink if possible, otherwise try a hard link
		ln -s $file $link 2>/dev/null || ln $file $link

		if [ ! -r $link ]
		then
			echo "$progname: unable to link \`$link' to \`$file'."
			exit 1
		fi
	fi
done

# edit the makefile
echo "Editing Makefile"
cp Makefile.dist tmp.c
cc -E >Makefile tmp.c $makedefine -DM_MAKEDEFINE="$makedefine"
rm -f tmp.c

echo "GDB is now set up for use with a $machine." \
	| tee config.status
exit 0

