#
#	$Header: Makefile,v 10.1 86/11/19 10:52:14 jg Exp $
#

SRCDIR = .
HDIR = /usr/include/machinecons
HDIR2 = /X

OBJS = bitblt_apa16.o bitblt_aed.o bitblt_cur.o bitblt_subr.o bitblt.o

CFLAGS= -O -I$(SRCDIR) -I$(HDIR) -I$(HDIR2) -DAPA8 -DAPA16 -DUSE_APA16_HDWR -DAPA8C -DAED

CC= /bin/cc

all:	bitblt.a

bitblt_apa16.o:	$(SRCDIR)/bitblt_int.h $(SRCDIR)/bitblt_ext.h \
		/usr/include/machinecons/xio.h $(SRCDIR)/bitblt_apa16.c \
		$(SRCDIR)/bitblt_apa16.h
	$(CC) $(CFLAGS) -c $(SRCDIR)/bitblt_apa16.c

bitblt_aed.o:	$(SRCDIR)/bitblt_int.h $(SRCDIR)/bitblt_ext.h \
		/usr/include/machinecons/xio.h $(SRCDIR)/bitblt_aed.c \
		$(SRCDIR)/bitblt_aed.h
	$(CC) $(CFLAGS) -c $(SRCDIR)/bitblt_aed.c

bitblt_cur.o:	$(SRCDIR)/bitblt_int.h $(SRCDIR)/bitblt_ext.h \
		/usr/include/machinecons/xio.h $(SRCDIR)/bitblt_cur.c
	$(CC) $(CFLAGS) -c $(SRCDIR)/bitblt_cur.c

bitblt_subr.o:	$(SRCDIR)/bitblt_int.h $(SRCDIR)/bitblt_ext.h \
		/usr/include/machinecons/xio.h $(SRCDIR)/bitblt_subr.c
	$(CC) $(CFLAGS) -c $(SRCDIR)/bitblt_subr.c

bitblt.o:	$(SRCDIR)/bitblt_int.h $(SRCDIR)/bitblt_ext.h \
		/usr/include/machinecons/xio.h $(SRCDIR)/bitblt.c
	$(CC) $(CFLAGS) -c $(SRCDIR)/bitblt.c

bitblt.a:	$(OBJS)
	@rm -f bitblt.a
	ar r bitblt.a $(OBJS)
	@ranlib bitblt.a

clean:
	rm -f *.o core bitblt.a
