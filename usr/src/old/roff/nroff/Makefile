#
#	@(#)Makefile	4.4 %G%
#
DESTDIR =
SRCS = \
	ni.c nii.c n1.c n2.c \
	n3.c n4.c n5.c n6.c \
	n7.c n8.c n9.c n10.c \
	hytab.c ntab.c suftab.c \
	d.h tw.h v.h tdef.h sdef.h

CFLAGS = -O -DNROFF -DVMUNIX -I../troff/ 
c = ni.o nii.o n1.o n2.o n3.o n4.o n5.o n6.o n7.o n8.o n9.o n10.o
s = hytab.o ntab.o suftab.o

all:	nroff term

nroff: $c $s
	$(CC) -o nroff  $s $c  

ni.o:	../troff/ni.c
	$(CC) $(CFLAGS) -c ../troff/ni.c

nii.o:	../troff/nii.c
	$(CC) $(CFLAGS) -c ../troff/nii.c

n1.o:	../troff/n1.c
	$(CC) $(CFLAGS) -c ../troff/n1.c

n2.o:	../troff/n2.c
	$(CC) $(CFLAGS) -c ../troff/n2.c

n3.o:	../troff/n3.c
	$(CC) $(CFLAGS) -c ../troff/n3.c

n4.o:	../troff/n4.c
	$(CC) $(CFLAGS) -c ../troff/n4.c

n5.o:	../troff/n5.c
	$(CC) $(CFLAGS) -c ../troff/n5.c

n7.o:	../troff/n7.c
	$(CC) $(CFLAGS) -c ../troff/n7.c

n8.o:	../troff/n8.c
	$(CC) $(CFLAGS) -c ../troff/n8.c

n9.o:	../troff/n9.c
	$(CC) $(CFLAGS) -c ../troff/n9.c

hytab.o:	../troff/hytab.c
	$(CC) -R -c ../troff/hytab.c

ntab.o:	../troff/ntab.c
	$(CC) -R -c ../troff/ntab.c

suftab.o:	../troff/suftab.c
	$(CC) -R -c ../troff/suftab.c

term:	FRC
	cd term; make ${MFLAGS}

FRC:

install: all
	install -s nroff ${DESTDIR}/usr/bin
	cd term; make ${MFLAGS} DESTDIR=${DESTDIR} install

clean:
	rm -f *.o nroff
	cd term; make ${MFLAGS} clean

depend:
