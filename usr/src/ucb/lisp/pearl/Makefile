
#	Makefile for pearl 

# Read the ReadMe file for more info.
# This makefile creates these things:
#   pearl - the executable PEARL, loaded into a lisp.
#   pearl.o - the object version of PEARL's functions for fasl'ing
#             into another lisp file at compile time.
#   tags - tags file for PEARL source.
#

# If LibDir is changed, you must also change the pathnames in pearllib.l
LibDir = /usr/lib/lisp
CopyTo = /dev/null
ManDir = /usr/man/man1
ObjDir = /usr/ucb
Liszt = ${ObjDir}/liszt
CdTo = ..

Src =	alias.l create.l db.l fix.l franz.l hash.l history.l hook.l \
	inits.l lowlevel.l match.l path.l pearl.l \
	pearlbase.l pearlbulk.l pearllib.l pearlsmall.l \
	print.l scopy.l symord.l \
	toplevel.l ucisubset.l vars.l

AllSrc =  Makefile ChangeLog ReadMe implement.ms ltags \
	manual.ms pearl.1 ptags template update.ms ${Src}

.l.o:
	${Liszt} $<

# Make "pearl.o" and "pearl" from scratch.
# NOTE: At installations where memory is less than 2.5Mb,
#    "make pearl" normally makes "small" which builds PEARL in two steps.
#    If your installation has more memory, "pearl" can be changed to
#    make "big" instead.  In this case, "install" below should also be
#    changed to make "biginstall" instead of "smallinstall".
pearl:	small
	echo "(savepearl)" | pearl.o
	@echo pearl done

pearlbase.o:	pearlbase.l

pearlbulk.o:	pearlbase.o pearlbulk.l

small:	pearlbase.o pearlbulk.o
	${Liszt} -r pearlsmall.l -o pearl.o

big:
	${Liszt} -r pearl.l

# Install the executable pearl in ObjDir and the
#   fasl'able pearl.o for compiling code using PEARL in LibDir.
#   NOTE: "install" can be changed to use "biginstall" on big enough machines.
install: smallinstall

smallinstall: small
	echo "(savepearl)" | pearl.o
	mv pearlbase.o ${LibDir}/pearlbase.o
	mv pearlbulk.o ${LibDir}/pearlbulk.o
	${Liszt} -r pearllib.l -o pearl.o
	mv pearl.o ${LibDir}/pearl.o
	mv pearl ${ObjDir}/pearl
	cp pearl.1 ${ManDir}/pearl.1
	@echo pearl done

biginstall: big
	echo "(savepearl)" | pearl.o
	mv pearl.o ${LibDir}/pearl.o
	mv pearl ${ObjDir}/pearl
	cp pearl.1 ${ManDir}/pearl.1
	@echo pearl done

tags:	/dev/tty  ${Src}
	-rm -f tags
	awk -f ltags ${Src} | sort > tags

# For distribution purposes.
copysource: ${AllSrc}
	(tar cf - ${AllSrc} | (cd ${CopyTo} ; tar xf -))

scriptcatall: ${AllSrc}
	@(cd ${CdTo} ; scriptcat pearl pearl ${AllSrc})

clean:
	-rm -f pearl pearl.o

