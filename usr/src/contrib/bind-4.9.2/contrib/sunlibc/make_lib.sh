#!/bin/sh

# Set this variable to be the .../bind/contrib/sunlibc directory to where the
#  special libc .o files were copied.
#resolvobjdir=/usr/local/src/bind-4.9.2/contrib/sunlibc
resolvobjdir=

if [ "${resolvobjdir}x" = "x" ]; then
	echo 'Edit the make_lib script to set $resolvobjdir to the proper'
	echo '.../bind/contrib/sunlibc directory.'
	exit 1
fi

mkdir tmp
cd tmp
ar x ../libc_pic.a
rm __.SYMDEF
for f in *.; do
	mv $f ${f}o
done
rm gethostent.o

cp $resolvobjdir/*.o .

cd ..
if [ ! -f lorder-sparc.orig ]
then
        patch < $resolvobjdir/sun-lorder-sparc.patches
fi
if [ ! -f Makefile.orig ]
then
        patch < $resolvobjdir/sun-Makefile.patches
fi

time make libc.so
ls -l
