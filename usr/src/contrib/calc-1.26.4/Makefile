#
# Copyright (c) 1993 David I. Bell and Landon Curt Noll
# Permission is granted to use, distribute, or modify this source,
# provided that this copyright notice remains intact.
#
# Arbitrary precision calculator.
#
# calculator by David I. Bell
# makefile by Landon Curt Noll

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
# If your machine defines BYTE_ORDER in <machine/endian.h>, then define 
# BYTE_ORDER be nothing.
#
BYTE_ORDER=
#BYTE_ORDER= -DBYTE_ORDER=BIG_ENDIAN
#BYTE_ORDER= -DBYTE_ORDER=LITTLE_ENDIAN

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
BINDIR= /usr/bin
#BINDIR= /usr/local/bin

# where to install the lib/*.calc files
#
LIBDIR= /usr/lib/calc
#LIBDIR= /usr/local/lib/calc

# where to install help files
#
# The ${LIBDIR}/help is where the help files will be installed.
#
HELPDIR= ${LIBDIR}/help

# where man pages are installed
#
#MANDIR=/usr/local/man/man1
#MANDIR=/usr/man/man1
MANDIR=/usr/share/man/man1
#MANDIR=/usr/man/u_man/man1

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

# If $PAGER is not set, use this program to display a help file
#
CALCPAGER= more
#CALCPAGER= less
#CALCPAGER= pg
#CALCPAGER= cat

# Compile debug options
#
# Select -O, or empty string, if you don't want to debug
DEBUG= -O
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

# Some systems allow 'mkdir -p' to make a directory and any needed parent 
# directories.
#
# If you system has 'mkdir -p', then:	MKDIR=mkdir -p
# otherwise:				MKDIR=mkdir
#
# If you do not have 'mkdir -p', then you must ensure that ${LIBDIR}, 
# ${BINDIR} and ${HELPDIR} exist before you do an install.
#
MKDIR=mkdir -p
#MKDIR=mkdir

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

#
# Some old systems don't know what a uid_t is.  Define UID_T if you get 
# an error regarding 'uid_t' when compiling files such as calc.c
#
UID_T=
#UID_T= -DUID_T

##############################################################################
#-=-=-=-=-=-=-=-=- Be careful if you change something below -=-=-=-=-=-=-=-=-#
##############################################################################

# standard utilities used during make
#
SHELL= /bin/sh
MAKE= make
LINT= lint
#CC= gcc
CC= cc
CTAGS= ctags

# -a	Report assignments of long values to variables that are not long
# -c	Complain about casts which have questionable  portability
# -h	Apply heuristic tests to intuit bugs, improve style, and reduce waste
#
LINTFLAGS= -h
#LINTFLAGS=

# the calc source files
#
CALCSRC= addop.c alloc.c calc.c codegen.c comfunc.c commath.c config.c \
	const.c file.c func.c hist.c input.c io.c label.c listfunc.c \
	matfunc.c obj.c opcodes.c qfunc.c qmath.c qmod.c qtrans.c string.c \
	symbol.c token.c value.c version.c zfunc.c zmath.c zmod.c zmul.c

# we build these .o files for calc
#
CALCOBJS= addop.o alloc.o calc.o codegen.o comfunc.o commath.o config.o \
	const.o file.o func.o hist.o input.o io.o label.o listfunc.o \
	matfunc.o obj.o opcodes.o qfunc.o qmath.o qmod.o qtrans.o string.o \
	symbol.o token.o value.o version.o zfunc.o zmath.o zmod.o zmul.o

# we build these .h files during the make
#
BUILD_H_SRC= config.h have_malloc.h have_stdlib.h have_string.h args.h \
	args.h terminal.h

# The code program is not part of the calc distribution, don't worry
# if you do not have it.
#
CODEOBJS= code.o io_code.o qfunc.o qmath_code.o zfunc.o zmath.o zmul.o zmod.o

# we build these .c files during the make
#
BUILD_CODE_SRC= io_code.c qmath_code.c

# LCFLAGS are flags that both ${CC} and ${LINT} can use
# CFLAGS are the default flags given to ${CC}
#
LCFLAGS= ${MALLOC} ${BYTE_ORDER} ${OLD_BSD} ${DONT_HAVE_VSPRINTF} ${UID_T}
CFLAGS= ${DEBUG} ${LCFLAGS}

all: calc calc.1

calc: ${CALCOBJS}
	${CC} ${CFLAGS} ${CALCOBJS} -o calc ${NO_SHARED}

hist.o: hist.c Makefile
	${CC} ${CFLAGS} ${DEBUG} ${TERMCONTROL} -c hist.c

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

have_malloc.h: Makefile
	rm -f have_malloc.h
	@echo 'forming have_malloc.h'
	@echo '/*' > have_malloc.h
	@echo ' * DO NOT EDIT -- generated by the Makefile' >> have_malloc.h
	@echo ' */' >> have_malloc.h
	@echo '' >> have_malloc.h
	@echo '/* do we have /usr/include/malloc.h? */' > have_malloc.h
	-@if [ -r /usr/include/malloc.h ]; then \
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
	-@if [ -r /usr/include/stdlib.h ]; then \
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
	-@if [ -r /usr/include/string.h ]; then \
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
	-@if [ -r /usr/include/termios.h ]; then \
		echo '#define USE_TERMIOS  /* <termios.h> */' >> terminal.h; \
		echo '#undef USE_TERMIO    /* <termio.h> */' >> terminal.h; \
		echo '#undef USE_SGTTY     /* <sys/ioctl.h> */' >> terminal.h; \
	elif [ -r /usr/include/termio.h ]; then \
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

args.h: Makefile
	rm -f args.h
	@echo 'forming args.h'
	@echo '/*' > args.h
	@echo ' * DO NOT EDIT -- generated by the Makefile' >> args.h
	@echo ' */' >> args.h
	@echo '' >> args.h
	@echo '/* what sort of variable args do we have? */' > args.h
	-@if [ ! -z "${VARARG}" ]; then \
		echo '#define ${VARARG}' >> args.h; \
	elif [ -r /usr/include/stdarg.h ]; then \
		echo '#define STDARG' >> args.h; \
	elif [ -r /usr/include/varargs.h ]; then \
		echo '#define VARARGS' >> args.h; \
	else \
		echo '#define SIMULATE_STDARG' >> args.h; \
	fi
	@echo 'args.h formed'

help/full: help/Makefile
	cd help; ${MAKE} -f Makefile HELPDIR=${HELPDIR} full

calc.1: calc.man
	rm -f calc.1
	sed -e 's:@@@LIBDIR@@@:${LIBDIR}:g' < calc.man > calc.1

lint: ${BUILD_H_SRC} ${CALCSRC} lint.sed
	${LINT} ${LINTFLAGS} ${LCFLAGS} ${CALCSRC} | sed -f lint.sed

tags: ${CALCSRC}
	${CTAGS} ${CALCSRC}

clean:
	rm -f ${CALCOBJS} ${CODEOBJS}
	cd help; ${MAKE} -f Makefile clean

clobber:
	rm -f ${CALCOBJS} ${CODEOBJS}
	rm -f tags calc code ${BUILD_CODE_SRC}
	rm -f ${BUILD_H_SRC} calc.1
	cd help; ${MAKE} -f Makefile clobber

install: all
	-@if [ ! -d ${LIBDIR} ]; then \
		echo ${MKDIR} ${LIBDIR}; \
		${MKDIR} ${LIBDIR}; \
	fi
	-@if [ ! -d ${HELPDIR} ]; then \
		echo ${MKDIR} ${HELPDIR}; \
		${MKDIR} ${HELPDIR}; \
	fi
	-@if [ ! -d ${BINDIR} ]; then \
		echo ${MKDIR} ${BINDIR}; \
		${MKDIR} ${BINDIR}; \
	fi
	chmod 0755 calc
	cp calc ${BINDIR}
	cd help; ${MAKE} -f Makefile HELPDIR=${HELPDIR} install
	cd lib; ${MAKE} -f Makefile LIBDIR=${LIBDIR} install
	-chmod 0444 calc.1
	rm -f ${MANDIR}/calc.1
	-cp calc.1 ${MANDIR}
	@# The code program is not part of the calc distribution, don't worry
	@# if you do not have it.
	-@if [ -f code ]; then \
		echo chmod +x code; \
		chmod +x code; \
		echo cp code ${BINDIR}; \
		cp code ${BINDIR}; \
	fi

# The code program is not part of the calc distribution, don't worry
# if you do not have it.
#
code: ${CODEOBJS}
	${CC} ${CFLAGS} ${CODEOBJS} -o code ${NO_SHARED}
io_code.o: calc.h math.h alloc.h have_string.h have_stdlib.h have_malloc.h \
	func.h opcodes.h config.h token.h symbol.h io_code.c
	${CC} ${CFLAGS} -DCODE io_code.c -c
qmath_code.o: calc.h math.h alloc.h have_string.h have_stdlib.h have_malloc.h \
	func.h opcodes.h config.h token.h symbol.h qmath_code.c
	${CC} ${CFLAGS} -DCODE qmath_code.c -c
io_code.c: io.c
	rm -f io_code.c
	cp io.c io_code.c
qmath_code.c: qmath.c
	rm -f qmath_code.c
	cp qmath.c qmath_code.c
code.o: stdarg.h args.h math.h have_malloc.h Makefile
	${CC} ${CFLAGS} -DCODE code.c -c

# make depend stuff
#
addop.o: calc.h math.h alloc.h have_string.h have_stdlib.h \
	have_malloc.h opcodes.h string.h func.h token.h label.h symbol.h
alloc.o: alloc.h have_string.h have_stdlib.h
calc.o: hist.h calc.h math.h alloc.h have_string.h have_stdlib.h \
	have_malloc.h func.h opcodes.h config.h token.h symbol.h
codegen.o: calc.h math.h alloc.h have_string.h have_stdlib.h \
	have_malloc.h token.h symbol.h label.h opcodes.h string.h \
	func.h config.h hist.h
comfunc.o: calc.h math.h alloc.h have_string.h have_stdlib.h \
	have_malloc.h
commath.o: math.h alloc.h have_string.h have_stdlib.h have_malloc.h
config.o: calc.h math.h alloc.h have_string.h have_stdlib.h \
	have_malloc.h
const.o: calc.h math.h alloc.h have_string.h have_stdlib.h have_malloc.h
file.o: stdarg.h args.h calc.h math.h alloc.h have_string.h have_stdlib.h \
	have_malloc.h have_string.h
func.o: calc.h math.h alloc.h have_string.h have_stdlib.h \
	have_malloc.h opcodes.h token.h func.h string.h
input.o: calc.h math.h hist.h alloc.h have_string.h have_stdlib.h \
	have_malloc.h config.h func.h
io.o: stdarg.h args.h math.h alloc.h have_string.h have_stdlib.h \
	have_malloc.h Makefile
hist.o: hist.h terminal.h Makefile
label.o: calc.h math.h alloc.h have_string.h have_stdlib.h \
	have_malloc.h token.h label.h string.h opcodes.h func.h
listfunc.o: calc.h math.h alloc.h have_string.h have_stdlib.h \
	have_malloc.h
matfunc.o: calc.h math.h alloc.h have_string.h have_stdlib.h \
	have_malloc.h
obj.o: calc.h math.h alloc.h have_string.h have_stdlib.h have_malloc.h \
	opcodes.h func.h symbol.h string.h
opcodes.o: stdarg.h args.h calc.h math.h alloc.h have_string.h have_stdlib.h \
	have_malloc.h have_string.h opcodes.h func.h symbol.h Makefile
qfunc.o: math.h alloc.h have_string.h have_stdlib.h have_malloc.h
qmath.o: math.h alloc.h have_string.h have_stdlib.h have_malloc.h
qmod.o: math.h alloc.h have_string.h have_stdlib.h have_malloc.h
qtrans.o: math.h alloc.h have_string.h have_stdlib.h have_malloc.h
string.o: calc.h math.h alloc.h have_string.h have_stdlib.h \
	have_malloc.h string.h
symbol.o: calc.h math.h alloc.h have_string.h have_stdlib.h \
	have_malloc.h token.h symbol.h string.h opcodes.h func.h
token.o: stdarg.h args.h calc.h math.h alloc.h have_string.h have_stdlib.h \
	have_malloc.h have_string.h token.h string.h Makefile
value.o: calc.h math.h alloc.h have_string.h have_stdlib.h \
	have_malloc.h opcodes.h func.h symbol.h
zfunc.o: math.h alloc.h have_string.h have_stdlib.h have_malloc.h
zmath.o: math.h alloc.h have_string.h have_stdlib.h have_malloc.h
zmod.o: math.h alloc.h have_string.h have_stdlib.h have_malloc.h
zmul.o: math.h alloc.h have_string.h have_stdlib.h have_malloc.h
