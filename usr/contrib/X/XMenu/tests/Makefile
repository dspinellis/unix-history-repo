#
#	Athena X window system XMenu system tests makefile.
#
#		Written by:	Tony Della Fera, DEC
#				20-Nov-85
#

DESTDIR =
INCLUDES = -I../../include

C2 = /lib/c2
CFLAGS = -O ${INCLUDES}
XLIB = ../../Xlib/libX.a
XLIB_H = ../../Xlib/Xlib.h
XLIB_P = ../../Xlib/libX_p.a

.SUFFIXES: .o .h .c

.c.o:
	${CC} -pg ${CFLAGS} -c $*.c
	mv $*.o profiled/
	${CC} ${CFLAGS} -c $*.c

all:	posttest posttest_p \
	rstest rstest_p \
	acttest acttest_p \
	deltest deltest_p \
	instest instest_p

#
# Library tests
#

posttest: posttest.o ../libXMenu.a ${XLIB_H}
	$(CC) $(CFLAGS) -o posttest posttest.o \
		../libXMenu.a ${XLIB}

posttest_p: posttest.o ../libXMenu_p.a ${XLIB_H}
	$(CC) $(CFLAGS) -pg -o posttest_p profiled/posttest.o \
		../libXMenu_p.a ${XLIB_P}

rstest: rstest.o ../libXMenu.a ${XLIB_H}
	$(CC) $(CFLAGS) -o rstest rstest.o \
		../libXMenu.a ${XLIB}

rstest_p: rstest.o ../libXMenu_p.a ${XLIB_H}
	$(CC) $(CFLAGS) -pg -o rstest_p profiled/rstest.o \
		../libXMenu_p.a ${XLIB_P}

acttest: acttest.o ../libXMenu.a ${XLIB_H}
	$(CC) $(CFLAGS) -o acttest acttest.o \
		../libXMenu.a ${XLIB}

acttest_p: acttest.o ../libXMenu_p.a ${XLIB_H}
	$(CC) $(CFLAGS) -pg -o acttest_p profiled/acttest.o \
		../libXMenu_p.a ${XLIB_P}

deltest: deltest.o ../libXMenu.a ${XLIB_H}
	$(CC) $(CFLAGS) -o deltest deltest.o \
		../libXMenu.a ${XLIB}

deltest_p: deltest.o ../libXMenu_p.a ${XLIB_H}
	$(CC) $(CFLAGS) -pg -o deltest_p profiled/deltest.o \
		../libXMenu_p.a ${XLIB_P}

instest: instest.o ../libXMenu.a ${XLIB_H}
	$(CC) $(CFLAGS) -o instest instest.o \
		../libXMenu.a ${XLIB}

instest_p: instest.o ../libXMenu_p.a ${XLIB_H}
	$(CC) $(CFLAGS) -pg -o instest_p profiled/instest.o \
		../libXMenu_p.a ${XLIB_P}

#
# Maintenance
#

lint:
	lint -CX *.c

ci:
	ci -u *.[ch] Makefile
	
co:
	co -l *.[ch] Makefile

consistent:
	ci -l -m"Consistency check in." *.[ch] Makefile

touch:
	touch *.o profile/*.o
	touch *test *test_p

igrind:
	igrind *.[ch] *.[ch] Makefile
	
clean: 
	rm -f *.o *.a core *.[ch].[0-9]* *.bak *~ #*
	rm -f profiled/*.o
	rm -f posttest posttest_p
	rm -f rstest rstest_p
	rm -f acttest acttest_p
	rm -f deltest deltest_p
	rm -f instest instest_p

#####     End of makefile     #####
