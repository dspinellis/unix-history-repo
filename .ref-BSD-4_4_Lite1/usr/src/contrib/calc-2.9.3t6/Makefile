#!/bin/make
#
# Copyright (c) 1994 David I. Bell and Landon Curt Noll
# Permission is granted to use, distribute, or modify this source,
# provided that this copyright notice remains intact.
#
# Arbitrary precision calculator.
#
# calculator by David I. Bell
# Makefile by Landon Curt Noll

##############################################################################
#-=-=-=-=-=-=-=-=- You may want to change some values below -=-=-=-=-=-=-=-=-#
##############################################################################

# Determine the type of terminal controls that you want to use
#
#	VARARG value	  meaning
#	------------	  -------
#	(nothing)	  let the makefile guess at what you need
#	-DUSE_TERMIOS	  use struct termios from <termios.h>
#	-DUSE_TERMIO 	  use struct termios from <termio.h>
#	-DUSE_SGTTY    	  use struct sgttyb from <sys/ioctl.h>
#
TERMCONTROL=
#TERMCONTROL= -DUSE_TERMIOS
#TERMCONTROL= -DUSE_TERMIO
#TERMCONTROL= -DUSE_SGTTY

# Determine the type of varargs that you want to use
#
#	VARARG value	  meaning
#	------------	  -------
#	(nothing)	  let the makefile guess at what you need
#	STDARG	    	  you have ANSI C and /usr/include/stdarg.h
#	VARARGS	    	  you have /usr/include/varargs.h
#	SIMULATE_STDARG   use simulated ./stdarg.h
#
# Try defining VARARG to be nothing.  The makefile will look for the
# needed .h files, trying for stdarg.h first.
#
VARARG=
#VARARG= STDARG
#VARARG= VARARGS
#VARARG= SIMULATE_STDARG

# If your system does not have a vsprintf() function, you could be in trouble.
#
#	vsprintf(stream, format, ap)
#
# This function works like sprintf except that the 3rd arg is a va_list
# strarg (or varargs) list.
#
# If you have vsprintf, then define DONT_HAVE_VSPRINTF to be an empty string.
# Some old systems do not have vsprintf().  If you do not have vsprintf()
# then define DONT_HAVE_VSPRINTF to be -DDONT_HAVE_VSPRINTF and hope for
# the best.
#
DONT_HAVE_VSPRINTF=
#DONT_HAVE_VSPRINTF= -DDONT_HAVE_VSPRINTF

# Determine the byte order of your machine
#
#    Big Endian:	Amdahl, 68k, Pyramid, Mips, Sparc, ...
#    Little Endian:	Vax, 32k, Spim (Dec Mips), i386, i486, ...
#
# If in doubt, leave BYTE_ORDER empty.  This makefile will attempt to
# use BYTE_ORDER in <machine/endian.h> or it will attempt to run
# the endian program.  If you get syntax errors when you compile,
# try forcing the value to be BIG_ENDIAN and run the calc regression
# tests. (see the README file)  If the calc regression tests fail, do
# a make clobber and try LITTLE_ENDIAN.  If that fails, ask a wizard
# for help.
#
BYTE_ORDER=
#BYTE_ORDER= BIG_ENDIAN
#BYTE_ORDER= LITTLE_ENDIAN

# Determine the number of bits in a long
#
# If in doubt, leave LONG_BITS empty.  This makefile will run
# the longbits program to determine the length.
#
LONG_BITS=
#LONG_BITS= 32
#LONG_BITS= 64

# Determine whether to use the standard malloc or the alternative one
# included with the calculator.  On some machines, the alternative malloc
# may be faster.  It also can help to debug malloc problems.
#
# Define MALLOC to be -DCALC_MALLOC to use the internal malloc routines.
#
# If in doubt, leave MALLOC empty.
#
MALLOC=
#MALLOC= -DCALC_MALLOC

# where to install binary files
#
#BINDIR= /usr/local/bin
#BINDIR= /usr/bin
BINDIR= /usr/contrib/bin

# where to install the lib/*.cal files
#
# ${TOPDIR} is the directory under which the calc directory will be placed.
# ${LIBDIR} is where the *.cal, bindings and help directory are installed.
# ${HELPDIR} is where the help directory is installed.
#
#TOPDIR= /usr/local/lib
#TOPDIR= /usr/lib
TOPDIR= /usr/libdata

LIBDIR= ${TOPDIR}/calc
HELPDIR= ${LIBDIR}/help

# where man pages are installed
#
# Use MANDIR= to disable installation of the calc man (source) page.
#
MANDIR=
#MANDIR=/usr/local/man/man1
#MANDIR=/usr/man/man1
#MANDIR=/usr/share/man/man1
#MANDIR=/usr/man/u_man/man1
#MANDIR=/usr/contrib/man/man1

# where cat (formatted man) pages are installed
#
# Use CATDIR= to disable installation of the calc cat (formatted) page.
#
CATDIR=
#CATDIR=/usr/local/man/cat1
#CATDIR=/usr/man/cat1
#CATDIR=/usr/share/man/cat1
#CATDIR=/usr/man/u_man/cat1
#CATDIR=/usr/contrib/man/cat1

# extenstion to add on to the calc man page filename
#
# This is ignored if CATDIR is empty.
#
MANEXT= 1
#MANEXT= l

# extenstion to add on to the calc man page filename
#
# This is ignored if CATDIR is empty.
#
#CATEXT= 1
CATEXT= 0
#CATEXT= l

# how to format a man page
#
# We will execute ${NROFF} ${NROFF_ARG} calc.1 to format the calc man page.
#
# This is ignored if CATDIR is empty.
NROFF= nroff
#NROFF= groff
NROFF_ARG = -man
#NROFF_ARG = -mandoc

# If the $CALCPATH environment variable is not defined, then the following
# path will be search for calc lib routines.
#
CALCPATH= .:./lib:~/lib:${LIBDIR}

# If the $CALCRC environment variable is not defined, then the following
# path will be search for calc lib routines.
#
CALCRC= ${LIBDIR}/startup:~/.calcrc

# If the $CALCBINDINGS environment variable is not defined, then the following
# file will be used for the command line and edit history key bindings.
#
CALCBINDINGS= ${LIBDIR}/bindings
#CALCBINDINGS= ${LIBDIR}/altbind

# If $PAGER is not set, use this program to display a help file
#
CALCPAGER= more
#CALCPAGER= pg
#CALCPAGER= cat
#CALCPAGER= less

# Compile/Debug options for ${CC} and ${LD}
#
DEBUG= -O
#DEBUG= -O2
#DEBUG= -O3
#DEBUG= -g
#DEBUG= -gx
#DEBUG= -WM,-g
#DEBUG=

# On systems that have dynamic shared libs, you want want to disable them
# for faster calc startup.
#
NO_SHARED=
#NO_SHARED= -dn

# If you are running an an old BSD system, then you may not have
# the following functions:
#
#	memcpy()	strchr()	memset()
#
# If you do not have these functions, define OLD_BSD to be -DOLD_BSD,
# otherwise define OLD_BSD to be an empty string.
#
# Modern BSD and BSD-like systems have these functions and thus don't
# need OLD_BSD.  If you don't know, try using the empty string and if
# you get complaints, try -DOLD_BSD.
#
OLD_BSD=
#OLD_BSD= -DOLD_BSD

# Some old systems don't know what a uid_t is.  Define UID_T if you get
# an error regarding 'uid_t' when compiling files such as calc.c
#
UID_T=
#UID_T= -DUID_T

# Some systems require one to use ranlib to add a symbol table to
# a *.a library.  Set RANLIB to the utility that performs this action.
# Set RANLIB to : if your system does not need such a utility.
#
RANLIB=ranlib
#RANLIB=:

# Some systems are able to form lint libs.  How it is formed depends
# on your system.  If you do not care about lint, use : as the
# LINTLIB value.
#
#    System type    LINTLIB recomendation
#
#	BSD	    ${LINT} ${LCFLAGS} ${LINTFLAGS} -u -Ccalc
#	SYSV	    ${LINT} ${LCFLAGS} ${LINTFLAGS} -u -o calc
#	disable     :
#
LINTLIB= ${LINT} ${LCFLAGS} ${LINTFLAGS} -u -Ccalc
#LINTLIB= ${LINT} ${LCFLAGS} ${LINTFLAGS} -u -o calc
#LINTLIB= :

# The lint flags vary from system to system.  Some systems have the
# opposite meaning for the flags below.  Other systems change flag
# meaning altogether.
#
#       System    LINTFLAGS recomendation
#
#	SunOs	  -a -h -v -z
#
LINTFLAGS= -a -h -v -z
#LINTFLAGS=

# Select your compiler type
#
# LCFLAGS are flags that both ${CC} and ${LINT} can use
# LDFLAGS are flags that both ${CC} amd ${LD} can use
# CCMAIN are flags for ${CC} when compiling only files with main()
# CCOPT ate flags for ${CC} only
# CFLAGS are the default flags given to ${CC}
#
# If you do not wish to use purify, leave ${PURIFY} commented out.
#
#PURIFY= /u2/purify/purify -logfile=pure.out
#PURIFY= /u2/purify/purify
#
###
#
# common cc
#
LCFLAGS= ${MALLOC} ${OLD_BSD} ${DONT_HAVE_VSPRINTF} ${UID_T}
LDFLAGS= ${DEBUG}
CCMAIN=
CCOPT=
CFLAGS= ${CCOPT} ${LDFLAGS} ${LCFLAGS}
CC= ${PURIFY} cc
#
# for RS6000 cc
#
#LCFLAGS= ${MALLOC} ${OLD_BSD} ${DONT_HAVE_VSPRINTF} ${UID_T}
#LDFLAGS= ${DEBUG}
#CCMAIN=
#CCOPT=
#CFLAGS= ${CCOPT} ${LDFLAGS} ${LCFLAGS} -qlanglvl=ansi
#CC= ${PURIFY} cc
#
# for gcc1
#
#LCFLAGS= ${MALLOC} ${OLD_BSD} ${DONT_HAVE_VSPRINTF} ${UID_T}
#LDFLAGS= ${DEBUG}
#CCMAIN=
#CCOPT= -Wall
#CFLAGS= ${CCOPT} ${LDFLAGS} ${LCFLAGS} -ansi
#CC= ${PURIFY} gcc
#
# for gcc2
#
#LCFLAGS= ${MALLOC} ${OLD_BSD} ${DONT_HAVE_VSPRINTF} ${UID_T}
#LDFLAGS= ${DEBUG}
#CCMAIN= -Wno-return-type
#CCOPT= -Wall -Wno-implicit -Wno-comment
#CFLAGS= ${CCOPT} ${LDFLAGS} ${LCFLAGS} -ansi
#CC= ${PURIFY} gcc

##############################################################################
#-=-=-=-=-=-=-=-=- Be careful if you change something below -=-=-=-=-=-=-=-=-#
##############################################################################

# standard utilities used during make
#
SHELL= /bin/sh
MAKE= make
SED= sed
TEE= tee
LINT= lint
CTAGS= ctags
LD= ${CC}

#
# the source files which are built into a math library
#
LIBSRC = alloc.c comfunc.c commath.c qfunc.c qio.c qmath.c qmod.c qtrans.c \
	zfunc.c zio.c zmath.c zmod.c zmul.c

#
# the object files which are built into a math library
#
LIBOBJS = alloc.o comfunc.o commath.o qfunc.o qio.o qmath.o qmod.o qtrans.o \
	zfunc.o zio.o zmath.o zmod.o zmul.o

#
# the calculator source files
#
CALCSRC = addop.c assocfunc.c calc.c codegen.c config.c const.c	file.c \
	func.c hist.c input.c label.c listfunc.c matfunc.c obj.c opcodes.c \
	string.c symbol.c token.c value.c version.c

#
# we build these .o files for calc
#
CALCOBJS = addop.o assocfunc.o calc.o codegen.o config.o const.o file.o \
	func.o hist.o input.o label.o listfunc.o matfunc.o obj.o opcodes.o \
	string.o symbol.o token.o value.o version.o

# we build these .h files during the make
#
BUILD_H_SRC= args.h config.h endian.h have_malloc.h have_stdlib.h \
	have_string.h longbits.h terminal.h

# these .h files are needed by programs that use libcalc.a
#
LIB_H_SRC= alloc.h args.h cmath.h endian.h have_malloc.h have_stdlib.h \
	have_string.h longbits.h qmath.h stdarg.h zmath.h

# these .h files are neither built, nor required by libcalc.a
#
CALC_H_SRC= calc.h config.h func.h hist.h label.h opcodes.h stdarg.h \
	string.h symbol.h terminal.h token.h value.h

# there are the complete list of .h files
#
H_SRC = ${CALC_H_SRC} ${LIB_H_SRC}

# The code program is not part of the calc distribution, don't worry
# if you do not have it.
#
CODEOBJS= code.o


all: calc calc.1

calc: libcalc.a ${CALCOBJS}
	${LD} ${LDFLAGS} ${CALCOBJS} libcalc.a -o calc ${NO_SHARED}

calc.o: calc.c
	${CC} ${CFLAGS} ${CCMAIN} -c calc.c

hsrc: ${BUILD_H_SRC}

hist.o: hist.c Makefile
	${CC} ${CFLAGS} ${TERMCONTROL} -c hist.c

libcalc.a: ${LIBOBJS} Makefile
	rm -f libcalc.a
	ar qc libcalc.a ${LIBOBJS}
	${RANLIB} libcalc.a

endian: endian.c
	-@rm -f endian.o endian
	${CC} ${CFLAGS} ${CCMAIN} endian.c -o endian ${NO_SHARED}

longbits: longbits.c
	-@rm -f longbits.o longbits
	${CC} ${CFLAGS} ${CCMAIN} longbits.c -o longbits ${NO_SHARED}

config.h: Makefile
	rm -f config.h
	@echo 'forming config.h'
	@echo '/*' > config.h
	@echo ' * DO NOT EDIT -- generated by the Makefile' >> config.h
	@echo ' */' >> config.h
	@echo '' >> config.h
	@echo '/* the default :-separated search path */' >> config.h
	@echo '#ifndef DEFAULTCALCPATH' >> config.h
	@echo '#define DEFAULTCALCPATH "${CALCPATH}"' >> config.h
	@echo '#endif /* DEFAULTCALCPATH */' >> config.h
	@echo '' >> config.h
	@echo '/* the default :-separated startup file list */' >> config.h
	@echo '#ifndef DEFAULTCALCRC' >> config.h
	@echo '#define DEFAULTCALCRC "${CALCRC}"' >> config.h
	@echo '#endif /* DEFAULTCALCRC */' >> config.h
	@echo '' >> config.h
	@echo '/* the default key bindings file */' >> config.h
	@echo '#ifndef DEFAULTCALCBINDINGS' >> config.h
	@echo '#define DEFAULTCALCBINDINGS "${CALCBINDINGS}"' >> config.h
	@echo '#endif /* DEFAULTCALCBINDINGS */' >> config.h
	@echo '' >> config.h
	@echo '/* the location of the help directory */' >> config.h
	@echo '#ifndef HELPDIR' >> config.h
	@echo '#define HELPDIR "${HELPDIR}"' >> config.h
	@echo '#endif /* HELPDIR */' >> config.h
	@echo '' >> config.h
	@echo '/* the default pager to use */' >> config.h
	@echo '#ifndef DEFAULTCALCPAGER' >> config.h
	@echo '#define DEFAULTCALCPAGER "${CALCPAGER}"' >> config.h
	@echo '#endif /* DEFAULTCALCPAGER */' >> config.h
	@echo 'config.h formed'

endian.h: endian Makefile
	rm -f endian.h
	@echo 'forming endian.h'
	@echo '/*' > endian.h
	@echo ' * DO NOT EDIT -- generated by the Makefile' >> endian.h
	@echo ' */' >> endian.h
	@echo '' >> endian.h
	-@if [ X"${BYTE_ORDER}" = X ]; then \
		if [ -f /usr/include/machine/endian.h ]; then \
			echo '#include <machine/endian.h>' >> endian.h; \
		else \
			./endian >> endian.h; \
		fi; \
	else \
	    echo "#define BYTE_ORDER ${BYTE_ORDER}" >> endian.h; \
	fi
	@echo 'endian.h formed'

longbits.h: longbits
	rm -f longbits.h
	@echo 'forming longbits.h'
	@echo '/*' > longbits.h
	@echo ' * DO NOT EDIT -- generated by the Makefile' >> longbits.h
	@echo ' */' >> longbits.h
	@echo '' >> longbits.h
	-@if [ X"${LONG_BITS}" = X ]; then \
		./longbits >> longbits.h; \
	else \
		echo "#define LONG_BITS ${LONG_BITS}" >> longbits.h; \
	fi
	@echo 'longbits.h formed'

have_malloc.h: Makefile
	rm -f have_malloc.h
	@echo 'forming have_malloc.h'
	@echo '/*' > have_malloc.h
	@echo ' * DO NOT EDIT -- generated by the Makefile' >> have_malloc.h
	@echo ' */' >> have_malloc.h
	@echo '' >> have_malloc.h
	@echo '/* do we have /usr/include/malloc.h? */' > have_malloc.h
	-@if [ -f /usr/include/malloc.h ]; then \
		echo '#define HAVE_MALLOC_H  /* yes */' >> have_malloc.h; \
	else \
		echo '#undef HAVE_MALLOC_H   /* no */' >> have_malloc.h; \
	fi
	@echo 'have_malloc.h formed'

have_stdlib.h: Makefile
	rm -f have_stdlib.h
	@echo 'forming have_stdlib.h'
	@echo '/*' > have_stdlib.h
	@echo ' * DO NOT EDIT -- generated by the Makefile' >> have_stdlib.h
	@echo ' */' >> have_stdlib.h
	@echo '' >> have_stdlib.h
	@echo '/* do we have /usr/include/stdlib.h? */' > have_stdlib.h
	-@if [ -f /usr/include/stdlib.h ]; then \
		echo '#define HAVE_STDLIB_H  /* yes */' >> have_stdlib.h; \
	else \
		echo '#undef HAVE_STDLIB_H   /* no */' >> have_stdlib.h; \
	fi
	@echo 'have_stdlib.h formed'

have_string.h: Makefile
	rm -f have_string.h
	@echo 'forming have_string.h'
	@echo '/*' > have_string.h
	@echo ' * DO NOT EDIT -- generated by the Makefile' >> have_string.h
	@echo ' */' >> have_string.h
	@echo '' >> have_string.h
	@echo '/* do we have /usr/include/string.h? */' > have_string.h
	-@if [ -f /usr/include/string.h ]; then \
		echo '#define HAVE_STRING_H  /* yes */' >> have_string.h; \
	else \
		echo '#undef HAVE_STRING_H   /* no */' >> have_string.h; \
	fi
	@echo 'have_string.h formed'

terminal.h: Makefile
	rm -f terminal.h
	@echo 'forming terminal.h'
	@echo '/*' > terminal.h
	@echo ' * DO NOT EDIT -- generated by the Makefile' >> terminal.h
	@echo ' */' >> terminal.h
	@echo '' >> terminal.h
	@echo '#if !defined(USE_TERMIOS)' >> terminal.h
	@echo '#if !defined(USE_TERMIO)' >> terminal.h
	@echo '#if !defined(USE_SGTTY)' >> terminal.h
	-@if [ -f /usr/include/termios.h ]; then \
		echo '#define USE_TERMIOS  /* <termios.h> */' >> terminal.h; \
		echo '#undef USE_TERMIO    /* <termio.h> */' >> terminal.h; \
		echo '#undef USE_SGTTY     /* <sys/ioctl.h> */' >> terminal.h; \
	elif [ -f /usr/include/termio.h ]; then \
		echo '#undef USE_TERMIOS   /* <termios.h> */' >> terminal.h; \
		echo '#define USE_TERMIO   /* <termio.h> */' >> terminal.h; \
		echo '#undef USE_SGTTY     /* <sys/ioctl.h> */' >> terminal.h; \
	else \
		echo '#undef USE_TERMIOS   /* <termios.h> */' >> terminal.h; \
		echo '#undef USE_TERMIO    /* <termio.h> */' >> terminal.h; \
		echo '#define USE_SGTTY    /* <sys/ioctl.h> */' >> terminal.h; \
	fi
	@echo '#endif' >> terminal.h
	@echo '#endif' >> terminal.h
	@echo '#endif' >> terminal.h
	@echo 'terminal.h formed'

args.h: Makefile try_stdarg.c
	rm -f args.h
	@echo 'forming args.h'
	@echo '/*' > args.h
	@echo ' * DO NOT EDIT -- generated by the Makefile' >> args.h
	@echo ' */' >> args.h
	@echo '' >> args.h
	@echo '/* what sort of variable args do we have? */' >> args.h
	-@if [ ! -z "${VARARG}" ]; then \
		echo '#define ${VARARG}' >> args.h; \
	elif [ -f /usr/include/stdarg.h -a -f /usr/include/varargs.h ]; then \
		rm -f try_stdarg; \
		${CC} ${CFLAGS} ${CCMAIN} try_stdarg.c -o try_stdarg; \
		if ./try_stdarg; then \
		    echo '#define STDARG' >> args.h; \
		else \
		    echo '#define VARARGS' >> args.h; \
		fi; \
		rm -f try_stdarg core core.try_stdarg; \
	elif [ -f /usr/include/stdarg.h ]; then \
		echo '#define STDARG' >> args.h; \
	elif [ -f /usr/include/varargs.h ]; then \
		echo '#define VARARGS' >> args.h; \
	else \
		echo '#define SIMULATE_STDARG' >> args.h; \
	fi
	@echo 'args.h formed'

calc.1: calc.man
	rm -f calc.1
	${SED} -e 's:$${LIBDIR}:${LIBDIR}:g' < calc.man > calc.1

llib-lcalc.ln: ${BUILD_H_SRC} ${LIBSRC} Makefile
	rm -f llib-lcalc.ln llib.out
	-touch llib-lcalc.ln
	${LINTLIB} ${LIBSRC} 2>&1 | ${SED} -f lint.sed | ${TEE} llib.out

lint: ${BUILD_H_SRC} ${CALCSRC} llib-lcalc.ln lint.sed Makefile
	rm -f lint.out
	${LINT} ${LINTFLAGS} ${LCFLAGS} llib-lcalc.ln ${CALCSRC} 2>&1 | \
	    ${SED} -f lint.sed | ${TEE} lint.out

tags: ${CALCSRC} ${LIBSRC} ${H_SRC} Makefile
	${CTAGS} ${CALCSRC} ${LIBSRC} ${H_SRC}

check: calc ./lib/regress.cal ./lib/lucas.cal ./lib/lucas_chk.cal \
	./lib/test1000.cal ./lib/surd.cal ./lib/cryrand.cal
	CALCPATH="./lib" ./calc -q read regress

lintclean:
	rm -f llib-lcalc.ln llib.out lint.out

clean:
	rm -f ${LIBOBJS} ${CALCOBJS} ${CODEOBJS}
	rm -f endian.o try_stdarg.o try_stdarg
	cd help; ${MAKE} -f Makefile \
	    LIBDIR=${LIBDIR} HELPDIR=${HELPDIR} clean
	cd lib; ${MAKE} -f Makefile LIBDIR=${LIBDIR} clean

clobber: lintclean
	rm -f ${LIBOBJS} ${CALCOBJS} ${CODEOBJS}
	rm -f tags calc code libcalc.a
	rm -f ${BUILD_H_SRC} calc.1 endian longbits
	rm -f *_pure_*.[oa]
	rm -f *.pure_hardlink *.pure_linkinfo
	cd help; ${MAKE} -f Makefile \
	    LIBDIR=${LIBDIR} HELPDIR=${HELPDIR} clobber
	cd lib; ${MAKE} -f Makefile LIBDIR=${LIBDIR} clobber

install: calc libcalc.a ${LIB_H_SRC} calc.1
	-@if [ ! -d ${LIBDIR} ]; then \
		echo mkdir ${LIBDIR}; \
		mkdir ${LIBDIR}; \
	fi
	-chmod 0755 ${LIBDIR}
	-@if [ ! -d ${HELPDIR} ]; then \
		echo mkdir ${HELPDIR}; \
		mkdir ${HELPDIR}; \
	fi
	-chmod 0755 ${HELPDIR}
	-@if [ ! -d ${BINDIR} ]; then \
		echo mkdir ${BINDIR}; \
		mkdir ${BINDIR}; \
	fi
	-chmod 0755 ${BINDIR}
	rm -f ${BINDIR}/calc
	cp calc ${BINDIR}
	-chmod 0555 ${BINDIR}/calc
	cd help; ${MAKE} -f Makefile \
	    LIBDIR=${LIBDIR} HELPDIR=${HELPDIR} install
	cd lib; ${MAKE} -f Makefile LIBDIR=${LIBDIR} install
	rm -f ${LIBDIR}/libcalc.a
	cp libcalc.a ${LIBDIR}/libcalc.a
	-chmod 0644 ${LIBDIR}/libcalc.a
	${RANLIB} ${LIBDIR}/libcalc.a
	@for i in ${LIB_H_SRC}; do \
		echo rm -f ${LIBDIR}/$$i; \
		rm -f ${LIBDIR}/$$i; \
		echo cp $$i ${LIBDIR}; \
		cp $$i ${LIBDIR}; \
		echo chmod 0444 ${LIBDIR}/$$i; \
		chmod 0444 ${LIBDIR}/$$i; \
	done
	@: If lint was made, install the lint library.
	-@if [ -f llib-lcalc.ln ]; then \
		echo rm -f ${LIBDIR}/llib-lcalc.ln; \
		rm -f ${LIBDIR}/llib-lcalc.ln; \
		echo cp llib-lcalc.ln ${LIBDIR}; \
		cp llib-lcalc.ln ${LIBDIR}; \
		echo chmod 0444 ${LIBDIR}/llib-lcalc.ln; \
		chmod 0444 ${LIBDIR}/llib-lcalc.ln; \
	fi
	@: The code program is not part of the calc distribution, do not worry
	@: if you do not have it.
	-@if [ -f code ]; then \
		echo chmod +x code; \
		chmod +x code; \
		echo rm -f ${BINDIR}/code; \
		rm -f ${BINDIR}/code; \
		echo cp code ${BINDIR}; \
		cp code ${BINDIR}; \
		echo chmod 0555 ${BINDIR}/code; \
		chmod 0555 ${BINDIR}/code; \
	fi
	-@if [ -z "${MANDIR}" ]; then \
	    echo "calc man page not installed, $${MANDIR} is empty"; \
	else \
	    echo "rm -f ${MANDIR}/calc.${MANEXT}"; \
	    rm -f ${MANDIR}/calc.${MANEXT}; \
	    echo "cp calc.1 ${MANDIR}/calc.${MANEXT}"; \
	    cp calc.1 ${MANDIR}/calc.${MANEXT}; \
	    echo "chmod 0444 ${MANDIR}/calc.${MANEXT}"; \
	    chmod 0444 ${MANDIR}/calc.${MANEXT}; \
	fi
	-@if [ -z "${CATDIR}" ]; then \
	    echo "calc cat page not installed, $${CATDIR} is empty"; \
	else \
	    echo "rm -f ${CATDIR}/calc.${CATEXT}"; \
	    rm -f ${CATDIR}/calc.${CATEXT}; \
	    echo "${NROFF} ${NROFF_ARG} calc.1 > ${CATDIR}/calc.${CATEXT}"; \
	    ${NROFF} ${NROFF_ARG} calc.1 > ${CATDIR}/calc.${CATEXT}; \
	    echo "chmod 0444 ${CATDIR}/calc.${CATEXT}"; \
	    chmod 0444 ${CATDIR}/calc.${CATEXT}; \
	fi

# The code program is not part of the calc distribution, don't worry
# if you do not have it.
#
code: ${CODEOBJS} libcalc.a
	${LD} ${LDFLAGS} ${CODEOBJS} libcalc.a -o code ${NO_SHARED}

code.o: alloc.h args.h endian.h have_malloc.h have_stdlib.h have_string.h \
	longbits.h qmath.h stdarg.h zmath.h
	${CC} ${CFLAGS} code.c -c


# make depend stuff
#
addop.o: addop.c
addop.o: alloc.h
addop.o: calc.h
addop.o: cmath.h
addop.o: endian.h
addop.o: func.h
addop.o: have_malloc.h
addop.o: have_stdlib.h
addop.o: have_string.h
addop.o: label.h
addop.o: longbits.h
addop.o: opcodes.h
addop.o: qmath.h
addop.o: string.h
addop.o: symbol.h
addop.o: token.h
addop.o: value.h
addop.o: zmath.h
alloc.o: alloc.c
alloc.o: alloc.h
alloc.o: have_malloc.h
alloc.o: have_stdlib.h
alloc.o: have_string.h
assocfunc.o: alloc.h
assocfunc.o: assocfunc.c
assocfunc.o: cmath.h
assocfunc.o: endian.h
assocfunc.o: have_malloc.h
assocfunc.o: have_stdlib.h
assocfunc.o: have_string.h
assocfunc.o: longbits.h
assocfunc.o: qmath.h
assocfunc.o: value.h
assocfunc.o: zmath.h
calc.o: alloc.h
calc.o: calc.c
calc.o: calc.h
calc.o: cmath.h
calc.o: config.h
calc.o: endian.h
calc.o: func.h
calc.o: have_malloc.h
calc.o: have_stdlib.h
calc.o: have_string.h
calc.o: hist.h
calc.o: label.h
calc.o: longbits.h
calc.o: opcodes.h
calc.o: qmath.h
calc.o: symbol.h
calc.o: token.h
calc.o: value.h
calc.o: zmath.h
codegen.o: alloc.h
codegen.o: calc.h
codegen.o: cmath.h
codegen.o: codegen.c
codegen.o: config.h
codegen.o: endian.h
codegen.o: func.h
codegen.o: have_malloc.h
codegen.o: have_stdlib.h
codegen.o: have_string.h
codegen.o: label.h
codegen.o: longbits.h
codegen.o: opcodes.h
codegen.o: qmath.h
codegen.o: string.h
codegen.o: symbol.h
codegen.o: token.h
codegen.o: value.h
codegen.o: zmath.h
comfunc.o: alloc.h
comfunc.o: cmath.h
comfunc.o: comfunc.c
comfunc.o: endian.h
comfunc.o: have_malloc.h
comfunc.o: have_stdlib.h
comfunc.o: have_string.h
comfunc.o: longbits.h
comfunc.o: qmath.h
comfunc.o: zmath.h
commath.o: alloc.h
commath.o: cmath.h
commath.o: commath.c
commath.o: endian.h
commath.o: have_malloc.h
commath.o: have_stdlib.h
commath.o: have_string.h
commath.o: longbits.h
commath.o: qmath.h
commath.o: zmath.h
config.o: alloc.h
config.o: calc.h
config.o: cmath.h
config.o: config.c
config.o: endian.h
config.o: have_malloc.h
config.o: have_stdlib.h
config.o: have_string.h
config.o: longbits.h
config.o: qmath.h
config.o: value.h
config.o: zmath.h
const.o: alloc.h
const.o: calc.h
const.o: cmath.h
const.o: const.c
const.o: endian.h
const.o: have_malloc.h
const.o: have_stdlib.h
const.o: have_string.h
const.o: longbits.h
const.o: qmath.h
const.o: value.h
const.o: zmath.h
file.o: alloc.h
file.o: args.h
file.o: calc.h
file.o: cmath.h
file.o: endian.h
file.o: file.c
file.o: have_malloc.h
file.o: have_stdlib.h
file.o: have_string.h
file.o: longbits.h
file.o: qmath.h
file.o: stdarg.h
file.o: value.h
file.o: zmath.h
func.o: alloc.h
func.o: calc.h
func.o: cmath.h
func.o: endian.h
func.o: func.c
func.o: func.h
func.o: have_malloc.h
func.o: have_stdlib.h
func.o: have_string.h
func.o: label.h
func.o: longbits.h
func.o: opcodes.h
func.o: qmath.h
func.o: string.h
func.o: symbol.h
func.o: token.h
func.o: value.h
func.o: zmath.h
hist.o: have_string.h
hist.o: hist.c
hist.o: hist.h
hist.o: terminal.h
input.o: alloc.h
input.o: calc.h
input.o: cmath.h
input.o: config.h
input.o: endian.h
input.o: have_malloc.h
input.o: have_stdlib.h
input.o: have_string.h
input.o: hist.h
input.o: input.c
input.o: longbits.h
input.o: qmath.h
input.o: value.h
input.o: zmath.h
label.o: alloc.h
label.o: calc.h
label.o: cmath.h
label.o: endian.h
label.o: func.h
label.o: have_malloc.h
label.o: have_stdlib.h
label.o: have_string.h
label.o: label.c
label.o: label.h
label.o: longbits.h
label.o: opcodes.h
label.o: qmath.h
label.o: string.h
label.o: token.h
label.o: value.h
label.o: zmath.h
listfunc.o: alloc.h
listfunc.o: cmath.h
listfunc.o: endian.h
listfunc.o: have_malloc.h
listfunc.o: have_stdlib.h
listfunc.o: have_string.h
listfunc.o: listfunc.c
listfunc.o: longbits.h
listfunc.o: qmath.h
listfunc.o: value.h
listfunc.o: zmath.h
matfunc.o: alloc.h
matfunc.o: cmath.h
matfunc.o: endian.h
matfunc.o: have_malloc.h
matfunc.o: have_stdlib.h
matfunc.o: have_string.h
matfunc.o: longbits.h
matfunc.o: matfunc.c
matfunc.o: qmath.h
matfunc.o: value.h
matfunc.o: zmath.h
obj.o: alloc.h
obj.o: calc.h
obj.o: cmath.h
obj.o: endian.h
obj.o: func.h
obj.o: have_malloc.h
obj.o: have_stdlib.h
obj.o: have_string.h
obj.o: label.h
obj.o: longbits.h
obj.o: obj.c
obj.o: opcodes.h
obj.o: qmath.h
obj.o: string.h
obj.o: symbol.h
obj.o: value.h
obj.o: zmath.h
opcodes.o: alloc.h
opcodes.o: args.h
opcodes.o: calc.h
opcodes.o: cmath.h
opcodes.o: endian.h
opcodes.o: func.h
opcodes.o: have_malloc.h
opcodes.o: have_stdlib.h
opcodes.o: have_string.h
opcodes.o: hist.h
opcodes.o: label.h
opcodes.o: longbits.h
opcodes.o: opcodes.c
opcodes.o: opcodes.h
opcodes.o: qmath.h
opcodes.o: stdarg.h
opcodes.o: symbol.h
opcodes.o: value.h
opcodes.o: zmath.h
qfunc.o: alloc.h
qfunc.o: endian.h
qfunc.o: have_malloc.h
qfunc.o: have_stdlib.h
qfunc.o: have_string.h
qfunc.o: longbits.h
qfunc.o: qfunc.c
qfunc.o: qmath.h
qfunc.o: zmath.h
qio.o: alloc.h
qio.o: args.h
qio.o: endian.h
qio.o: have_malloc.h
qio.o: have_stdlib.h
qio.o: have_string.h
qio.o: longbits.h
qio.o: qio.c
qio.o: qmath.h
qio.o: stdarg.h
qio.o: zmath.h
qmath.o: alloc.h
qmath.o: endian.h
qmath.o: have_malloc.h
qmath.o: have_stdlib.h
qmath.o: have_string.h
qmath.o: longbits.h
qmath.o: qmath.c
qmath.o: qmath.h
qmath.o: zmath.h
qmod.o: alloc.h
qmod.o: endian.h
qmod.o: have_malloc.h
qmod.o: have_stdlib.h
qmod.o: have_string.h
qmod.o: longbits.h
qmod.o: qmath.h
qmod.o: qmod.c
qmod.o: zmath.h
qtrans.o: alloc.h
qtrans.o: endian.h
qtrans.o: have_malloc.h
qtrans.o: have_stdlib.h
qtrans.o: have_string.h
qtrans.o: longbits.h
qtrans.o: qmath.h
qtrans.o: qtrans.c
qtrans.o: zmath.h
string.o: alloc.h
string.o: calc.h
string.o: cmath.h
string.o: endian.h
string.o: have_malloc.h
string.o: have_stdlib.h
string.o: have_string.h
string.o: longbits.h
string.o: qmath.h
string.o: string.c
string.o: string.h
string.o: value.h
string.o: zmath.h
symbol.o: alloc.h
symbol.o: calc.h
symbol.o: cmath.h
symbol.o: endian.h
symbol.o: func.h
symbol.o: have_malloc.h
symbol.o: have_stdlib.h
symbol.o: have_string.h
symbol.o: label.h
symbol.o: longbits.h
symbol.o: opcodes.h
symbol.o: qmath.h
symbol.o: string.h
symbol.o: symbol.c
symbol.o: symbol.h
symbol.o: token.h
symbol.o: value.h
symbol.o: zmath.h
token.o: alloc.h
token.o: args.h
token.o: calc.h
token.o: cmath.h
token.o: endian.h
token.o: have_malloc.h
token.o: have_stdlib.h
token.o: have_string.h
token.o: longbits.h
token.o: qmath.h
token.o: stdarg.h
token.o: string.h
token.o: token.c
token.o: token.h
token.o: value.h
token.o: zmath.h
value.o: alloc.h
value.o: calc.h
value.o: cmath.h
value.o: endian.h
value.o: func.h
value.o: have_malloc.h
value.o: have_stdlib.h
value.o: have_string.h
value.o: label.h
value.o: longbits.h
value.o: opcodes.h
value.o: qmath.h
value.o: string.h
value.o: symbol.h
value.o: value.c
value.o: value.h
value.o: zmath.h
version.o: alloc.h
version.o: calc.h
version.o: cmath.h
version.o: endian.h
version.o: have_malloc.h
version.o: have_stdlib.h
version.o: have_string.h
version.o: longbits.h
version.o: qmath.h
version.o: value.h
version.o: version.c
version.o: zmath.h
zfunc.o: alloc.h
zfunc.o: endian.h
zfunc.o: have_malloc.h
zfunc.o: have_stdlib.h
zfunc.o: have_string.h
zfunc.o: longbits.h
zfunc.o: zfunc.c
zfunc.o: zmath.h
zio.o: alloc.h
zio.o: args.h
zio.o: endian.h
zio.o: have_malloc.h
zio.o: have_stdlib.h
zio.o: have_string.h
zio.o: longbits.h
zio.o: stdarg.h
zio.o: zio.c
zio.o: zmath.h
zmath.o: alloc.h
zmath.o: endian.h
zmath.o: have_malloc.h
zmath.o: have_stdlib.h
zmath.o: have_string.h
zmath.o: longbits.h
zmath.o: zmath.c
zmath.o: zmath.h
zmod.o: alloc.h
zmod.o: endian.h
zmod.o: have_malloc.h
zmod.o: have_stdlib.h
zmod.o: have_string.h
zmod.o: longbits.h
zmod.o: zmath.h
zmod.o: zmod.c
zmul.o: alloc.h
zmul.o: endian.h
zmul.o: have_malloc.h
zmul.o: have_stdlib.h
zmul.o: have_string.h
zmul.o: longbits.h
zmul.o: zmath.h
zmul.o: zmul.c
