# CKUKER.MAK, Sat Feb  8 15:54:47 1992
#
# Abbreviated version for 2.10 / 2.11 BSD, which chokes on full-size makefile
# because "Make: out of memory".
#
# Instructions:
#   1. Change the name of this file to "makefile".
#   2. Make sure there are no other files called "makefile" or "Makefile"
#      in the same directory.
#   3. "make bsd210"
#
# Author: Frank da Cruz, Columbia University Center for Computing Activities
# 612 West 115th Street, New York, NY 10025, USA.  Phone (212) 854-5126.
# e-mail: fdc@watsun.cc.columbia.edu, fdc@columbia.edu, or FDCCU@CUVMA.BITNET.
# BSD 2.10/2.11 specifics contributed by Steven M Schultz
# sms@wlv.imsd.contel.com
#
##############################################################################
#
# V7-specific variables.
# These are set up for Perkin-Elmer 3230 V7 Unix:
# (Are these really needed for 2.10 BSD?)
#
PROC=proc
DIRECT=
NPROC=nproc
NPTYPE=int
BOOTFILE=/edition7
#
###########################################################################
#
#  Compile and Link variables:
#
#  EXT is the extension (file type) for object files, normally o.
#
EXT=o
LNKFLAGS=
SHAREDLIB=
CC= cc
CC2= cc
SHELL=/bin/sh
#
###########################################################################
#
# Dependencies Section:

wermit: ckcmai.$(EXT) ckucmd.$(EXT) ckuusr.$(EXT) ckuus2.$(EXT) ckuus3.$(EXT) \
		ckuus4.$(EXT) ckuus5.$(EXT) ckuus6.$(EXT) ckuus7.$(EXT) \
		ckuusx.$(EXT) ckuusy.$(EXT) ckcpro.$(EXT) ckcfns.$(EXT) \
		ckcfn2.$(EXT) ckcfn3.$(EXT) ckuxla.$(EXT) ckucon.$(EXT) \
		ckutio.$(EXT) ckufio.$(EXT) ckudia.$(EXT) ckuscr.$(EXT) \
		ckcnet.$(EXT)
	$(CC2) $(LNKFLAGS) -o wermit ckcmai.$(EXT) ckutio.$(EXT) \
		ckufio.$(EXT) ckcfns.$(EXT) ckcfn2.$(EXT) ckcfn3.$(EXT) \
		ckuxla.$(EXT) ckcpro.$(EXT) ckucmd.$(EXT) ckuus2.$(EXT) \
		ckuus3.$(EXT) ckuus4.$(EXT) ckuus5.$(EXT) ckuus6.$(EXT) \
		ckuus7.$(EXT) ckuusx.$(EXT) ckuusy.$(EXT) ckuusr.$(EXT) \
		ckucon.$(EXT) ckudia.$(EXT) ckuscr.$(EXT) ckcnet.$(EXT) $(LIBS)

# For building Kermit with overlays...

ovwermit: ckcmai.$(EXT) ckucmd.$(EXT) ckuusr.$(EXT) ckuus2.$(EXT) \
	ckuus3.$(EXT) ckuus4.$(EXT) ckuus5.$(EXT) ckcpro.$(EXT) \
	ckcfns.$(EXT) ckcfn2.$(EXT) ckcfn3.$(EXT) ckuxla.$(EXT) \
	ckucon.$(EXT) ckutio.$(EXT) ckufio.$(EXT) ckudia.$(EXT) \
	ckuscr.$(EXT) ckcnet.$(EXT) ckuus6.$(EXT) ckuus7.$(EXT) ckuusx.$(EXT) \
	ckuusy.$(EXT) ckustr.o strings.o
	ar x /lib/libc.a getpwent.o ctime.o ndbm.o
	$(CC2) $(LNKFLAGS) -o wermit ckcmai.$(EXT) \
		ckutio.$(EXT) ckufio.$(EXT) ckcfns.$(EXT) ckcfn2.$(EXT) \
		ckcfn3.$(EXT) \
		 -Z ckuxla.$(EXT) ckcpro.$(EXT) ckucmd.$(EXT) ckuus2.$(EXT) \
		    ckuus3.$(EXT) \
		 -Z ckuus4.$(EXT) ckuus5.$(EXT) ckuusr.$(EXT) ckuus6.$(EXT) \
		    ctime.o \
		 -Z ckuus7.$(EXT) ckcfn3.$(EXT) ckudia.$(EXT) ckuscr.$(EXT) \
		    ckcnet.$(EXT) ckuusy.$(EXT) \
		 -Z ckuusx.$(EXT) ckucon.$(EXT) getpwent.o ndbm.o \
		 -Y ckustr.o strings.o $(LIBS)

strings.o: strings
	xstr
	cc -c xs.c
	mv -f xs.o strings.o
	rm -f xs.c

###########################################################################
# Dependencies for each module...
#
ckcmai.$(EXT): ckcmai.c ckcker.h ckcdeb.h ckcsym.h ckcasc.h ckcnet.h

ckcpro.$(EXT): ckcpro.c ckcker.h ckcdeb.h ckcasc.h

ckcpro.c: ckcpro.w wart ckcdeb.h ckcasc.h ckcker.h
	./wart ckcpro.w ckcpro.c

ckcfns.$(EXT): ckcfns.c ckcker.h ckcdeb.h ckcsym.h ckcasc.h ckcxla.h \
		ckuxla.h

ckcfn2.$(EXT): ckcfn2.c ckcker.h ckcdeb.h ckcsym.h ckcasc.h ckcxla.h ckuxla.h

ckcfn3.$(EXT): ckcfn3.c ckcker.h ckcdeb.h ckcsym.h ckcasc.h ckcxla.h \
		ckuxla.h

ckuxla.$(EXT): ckuxla.c ckcker.h ckcdeb.h ckcxla.h ckuxla.h

ckuusr.$(EXT): ckuusr.c ckucmd.h ckcker.h ckuusr.h ckcdeb.h ckcxla.h ckuxla.h \
		ckcasc.h ckcnet.h

ckuus2.$(EXT): ckuus2.c ckucmd.h ckcker.h ckuusr.h ckcdeb.h ckcxla.h ckuxla.h \
		ckcasc.h

ckuus3.$(EXT): ckuus3.c ckucmd.h ckcker.h ckuusr.h ckcdeb.h ckcxla.h ckuxla.h \
		ckcasc.h ckcnet.h

ckuus4.$(EXT): ckuus4.c ckucmd.h ckcker.h ckuusr.h ckcdeb.h ckcxla.h ckuxla.h \
		ckcasc.h ckcnet.h

ckuus5.$(EXT): ckuus5.c ckucmd.h ckcker.h ckuusr.h ckcdeb.h ckcasc.h

ckuus6.$(EXT): ckuus6.c ckucmd.h ckcker.h ckuusr.h ckcdeb.h ckcasc.h

ckuus7.$(EXT): ckuus7.c ckucmd.h ckcker.h ckuusr.h ckcdeb.h ckcxla.h ckuxla.h \
		ckcasc.h ckcnet.h

ckuusx.$(EXT): ckuusx.c  ckcker.h ckuusr.h ckcdeb.h ckcasc.h

ckuusy.$(EXT): ckuusy.c  ckcker.h ckcdeb.h ckcasc.h

ckucmd.$(EXT): ckucmd.c ckcasc.h ckucmd.h ckcdeb.h

ckufio.$(EXT): ckufio.c ckcdeb.h ckuver.h

ckutio.$(EXT): ckutio.c ckcdeb.h ckcnet.h ckuver.h

ckucon.$(EXT): ckucon.c ckcker.h ckcdeb.h ckcasc.h ckcnet.h

ckcnet.$(EXT): ckcnet.c ckcdeb.h ckcker.h ckcnet.h

wart: ckwart.$(EXT)
	$(CC) $(LNKFLAGS) -o wart ckwart.$(EXT) $(LIBS)

ckcmdb.$(EXT): ckcmdb.c ckcdeb.h

ckwart.$(EXT): ckwart.c

ckudia.$(EXT): ckudia.c ckcker.h ckcdeb.h ckucmd.h ckcasc.h

ckuscr.$(EXT): ckuscr.c ckcker.h ckcdeb.h ckcasc.h

###########################################################################
#
# 2.10BSD and 2.11BSD (the latter to a larger extent) are the same as 4.3BSD
# but without a large address space.
#
# NOTE: A string extraction method is used to put approx. 9kb of strings into a
# file. The module ckustr.c should be edited, if necessary, to change the
# pathname of the string file to where this file will reside (or just define
# the environment variable KSTR to be the pathname for the string file before
# running "make").  After make is finished, the file kermit5.sr should be moved
# to the where ckustr.c has been told to look for it.
#
bsd210:
	@echo "Making C-Kermit 5A for 2.10BSD with overlays..."
	@echo -n "Be sure to install kermit5.sr with the same pathname"
	@echo " specified in ckustr.c!"
	chmod +x ckustr.sed
	make ovwermit "CFLAGS= -O -DBSD43 -DLCKDIR -DNODEBUG -DNOTLOG \
	-DNOCSETS -DNOHELP -DNOSCRIPT -DNOSPL -DNOXMIT $(KFLAGS) \
	-Dgethostname=gethnam" "LNKFLAGS= -i " "CC= ./ckustr.sed " "CC2= cc"

#Clean up intermediate and object files
clean:
	@echo 'Removing intermediate files...'
	-rm -f ck*.$(EXT) ckcpro.c wart strings kermit5.sr
