#	@(#)Makefile	4.1	(Berkeley)	82/10/24
#
CFLAGS=	-O -w
OBJS=	chkid.o\
	chksid.o\
	del_ba.o\
	date_ba.o\
	dodelt.o\
	del_ab.o\
	dofile.o\
	dohist.o\
	doie.o\
	dolist.o\
	eqsid.o\
	flushto.o\
	newstats.o\
	permiss.o\
	pf_ab.o\
	date_ab.o\
	rdmod.o\
	setup.o\
	sid_ab.o\
	sid_ba.o\
	sidtoser.o\
	sinit.o\
	stats_ab.o\
	fmterr.o\
	getline.o\
	putline.o\
	auxf.o

comobj.a: $(OBJS)
	ar rvu comobj.a $(OBJS)
	ranlib comobj.a

clean:
	rm -f $(OBJS) comobj.a

install:
