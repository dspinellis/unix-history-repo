#######################################################################
#
#	@(#)Makefile	1.7	(CWI)	90/11/03
#
# Makefile for ditroff and nroff
#
#######################################################################

CFLAGS = -DINCORE -O
NFLAGS = -DNROFF -DSMALLER	# additional flags for nroff compile

# other utilities maintained by this makefile

UDIRS = utilities devhar eqn pic ideal grap tbl term devnver chem

# location to install final output

BINDIR = /usr/local/bin
LIBDIR = /usr/local/libdata/ditroff_font
LIBXDIR = /usr/local/libexec
MANDIR = /usr/local/man/manl

# the directories where the .o files are stored

N = nrobs
T = trobs

# the object files needed by nroff and troff

NOBS = $N/n1.o $N/n2.o $N/n3.o $N/n4.o $N/n5.o $N/n6.o $N/n7.o $N/n8.o \
       $N/n9.o $N/n10.o $N/ni.o $N/nii.o $N/hytab.o $N/suftab.o

TOBS = $T/n1.o $T/n2.o $T/n3.o $T/n4.o $T/n5.o $T/t6.o $T/n7.o $T/n8.o \
       $T/n9.o $T/t10.o $T/ni.o $T/nii.o $T/hytab.o $T/suftab.o

#######################################################################

all:	troff_p $(UDIRS)

troff_p: $(T) $(TOBS)
	$(CC) $(CFLAGS) $(TOBS) -o troff_p -lcompat


nroff:	$(N) $(NOBS)
	$(CC) $(CFLAGS) $(NFLAGS) $(NOBS) -o nroff -lcompat


$(T):
	mkdir $(T)

$(N):
	mkdir $(N)

$(LIBDIR):
	mkdir $(LIBDIR)

install: all $(LIBDIR)
	install -c -s ditroff $(DESTDIR)$(LIBXDIR)/troff_p
	for i in $(UDIRS); do (cd $$i; make $(MFLAGS) DESTDIR=${DESTDIR} install); done
#	install -c -s nroff $(DESTDIR)$(BINDIR)/nroff
#	install -c -m 664 ditroff.1 $(DESTDIR)$(MANDIR)/ditroff.1

clean:  force
	rm -rf make.out troff_p nroff $N $T
	for i in ${UDIRS}; do (cd $$i; make ${MFLAGS} clean); done

$(UDIRS): force
	cd $@; make $(MFLAGS)


# fake dependency to force a make
force:


#######################################################################
#
# The dependencies are divided into two parts: the
# first expresses the dependencies between the files
# and includes the fact that the .o files are in
# different directories than the .c files.  The 2nd
# part expresses no dependencies, but contains the
# rule for compiling the .c files into .o files.
#
# File Dependencies:
#

$T/n1.o:		n1.c  tdef.h tw.h ext.h $T/ni.o
$N/n1.o:		n1.c  tdef.h tw.h ext.h $N/ni.o

$T/n2.o  $N/n2.o:	n2.c  tdef.h tw.h
$T/n3.o  $N/n3.o:	n3.c  tdef.h tw.h
$T/n4.o  $N/n4.o:	n4.c  tdef.h tw.h
$T/n5.o  $N/n5.o:	n5.c  tdef.h tw.h

$T/t6.o:		t6.c  tdef.h tw.h dev.h $T/ni.o
$N/n6.o:		n6.c  tdef.h tw.h

$T/n7.o  $N/n7.o:	n7.c  tdef.h tw.h
$T/n8.o  $N/n8.o:	n8.c  tdef.h tw.h
$T/n9.o  $N/n9.o:	n9.c  tdef.h tw.h

$T/t10.o:		t10.c tdef.h tw.h dev.h $T/ni.o
$N/n10.o:		n10.c tdef.h tw.h

$T/ni.o  $N/ni.o:	ni.c  tdef.h tw.h
$T/nii.o $N/nii.o:	nii.c tdef.h tw.h
$T/hytab.o $N/hytab.o:	hytab.c
$T/suftab.o $N/suftab.o: suftab.c


#
# Generation Rule:
#  .oldestfile is older than everything, so
#  these rules will not be invoked unless
#  one of the above dependencies is.]
#

$(NOBS):  .oldestfile
	  $(CC) $(CFLAGS) $(NFLAGS) -c $@ `basename $@ .o`.c
	mv `basename $@` nrobs

$(TOBS):  .oldestfile
	  $(CC) $(CFLAGS) -c $@ `basename $@ .o`.c
	mv `basename $@` trobs

.oldestfile:
	touch .oldestfile
