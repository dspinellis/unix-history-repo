#	 This is the description file used by the make(I) command
#	 to reconstruct all or part of SCCS

#	(All Macro Definitions are described on line following definition)

GET = get 
#	SCCS command get(I)
REL =
#	Null string used to specify release to get in make(I) command line
CFLAGS = -O
#	Flags used by C compiler
LDFLAGS =
#	Load flags used by C compiler
LIBES = -lpw -lPW -lwrt
#	Libraries used during loading

SCCS:		admin bdiff chghist comb scv \
	delta get help prt rmdel sccsdiff \
	what val snull vc cmt

admin:	admin.o ../com/comobj.a ctime.o alloc.o
	$(CC) $(LDFLAGS) -o admin admin.o ../com/comobj.a $(LIBES)

admin.o:	admin.c ../hdr/defines.h ../hdr/had.h
	$(CC) $(CFLAGS) admin.c

admin.c:	s.admin.c
	$(GET) $(REL) s.admin.c

cmt:	cmt.o ../com/comobj.a
	$(CC) $(LDFLAGS) -o cmt cmt.o ../com/comobj.a $(LIBES)

cmt.o:	cmt.c ../hdr/defines.h ../hdr/had.h
	$(CC) $(CFLAGS) cmt.c

cmt.c:		s.cmt.c
	$(GET) $(REL) s.cmt.c

bdiff:	bdiff.o ../com/comobj.a
	$(CC) $(LDFLAGS) -o bdiff bdiff.o ../com/comobj.a $(LIBES)

bdiff.o:	bdiff.c ../hdr/defines.h ../hdr/had.h
	$(CC) $(CFLAGS) bdiff.c

bdiff.c:	s.bdiff.c
	$(GET) $(REL) s.bdiff.c

comb:	comb.o ../com/comobj.a
	$(CC) $(LDFLAGS) -o comb comb.o ../com/comobj.a $(LIBES)

comb.o:		comb.c ../hdr/defines.h ../hdr/had.h
	$(CC) $(CFLAGS) comb.c

comb.c:		s.comb.c
	$(GET) $(REL) s.comb.c

scv:	scv.o ../com/comobj.a
	$(CC) $(LDFLAGS) -o scv scv.o ../com/comobj.a $(LIBES)

scv.o:		scv.c ../hdr/defines.h ../hdr/had.h
	$(CC) $(CFLAGS) scv.c

scv.c:		s.scv.c
	$(GET) $(REL) s.scv.c

snull:	snull.o ../com/comobj.a
	$(CC) $(LDFLAGS) -o snull snull.o ../com/comobj.a $(LIBES)

snull.o:		snull.c ../hdr/defines.h ../hdr/had.h
	$(CC) $(CFLAGS) snull.c

snull.c:		s.snull.c
	$(GET) $(REL) s.snull.c

delta:	delta.o ../com/comobj.a
	$(CC) $(LDFLAGS) -o delta delta.o ../com/comobj.a $(LIBES) ctime.o alloc.o

delta.o:	delta.c ../hdr/defines.h ../hdr/had.h
	$(CC) $(CFLAGS) delta.c

delta.c:	s.delta.c
	$(GET) $(REL) s.delta.c

get:	get.o ../com/comobj.a
	$(CC) $(LDFLAGS) -o get get.o ../com/comobj.a $(LIBES) ctime.o alloc.o

get.o:		get.c ../hdr/defines.h ../hdr/had.h
	$(CC) $(CFLAGS) get.c

get.c:		s.get.c
	$(GET) $(REL) s.get.c

help:	help.o ../com/comobj.a
	$(CC) $(LDFLAGS) -o help help.o ../com/comobj.a $(LIBES)

help.o:		help.c ../hdr/defines.h ../hdr/had.h
	$(CC) $(CFLAGS) help.c

help.c:		s.help.c
	$(GET) $(REL) s.help.c

prt:	prt.o ../com/comobj.a
	$(CC) $(LDFLAGS) -o prt prt.o ../com/comobj.a $(LIBES) ctime.o alloc.o

prt.o:		prt.c ../hdr/defines.h ../hdr/had.h
	$(CC) $(CFLAGS) prt.c

prt.c:		s.prt.c
	$(GET) $(REL) s.prt.c

chghist:	rmchg
	-rm -f chghist
	ln rmchg chghist

rmdel:	rmchg
	-rm -f rmdel
	ln rmchg rmdel

rmchg:	rmchg.o ../com/comobj.a
	$(CC) $(LDFLAGS) -o rmchg rmchg.o ../com/comobj.a $(LIBES)

rmchg.o:	rmchg.c ../hdr/defines.h ../hdr/had.h
	$(CC) $(CFLAGS) rmchg.c

rmchg.c:		s.rmchg.c
	$(GET) $(REL) s.rmchg.c

sccsdiff:	s.sccsdiff
		$(GET) $(REL) s.sccsdiff; chmod 555 sccsdiff

what:	what.o ../com/comobj.a
	$(CC) $(LDFLAGS) -o what what.o ../com/comobj.a $(LIBES) $(LIBES) ctime.o alloc.o

what.o:		what.c ../hdr/defines.h ../hdr/had.h
	$(CC) $(CFLAGS) what.c

what.c:		s.what.c
	$(GET) $(REL) s.what.c

val:	val.o ../com/comobj.a
	$(CC) $(LDFLAGS) -o val val.o ../com/comobj.a $(LIBES) $(LIBES)

val.o:	val.c ../hdr/defines.h ../hdr/had.h
	$(CC) $(CFLAGS) val.c

val.c:		s.val.c
	$(GET) $(REL) s.val.c

vc:	vc.o ../com/comobj.a
	$(CC) $(LDFLAGS) -o vc vc.o ../com/comobj.a $(LIBES) $(LIBES)

vc.o:	vc.c ../hdr/defines.h ../hdr/had.h
	$(CC) $(CFLAGS) vc.c

vc.c:		s.vc.c
	$(GET) $(REL) s.vc.c

#../hdr/defines.h:		../hdr/s.defines.h
#	-chdir ../hdr; $(GET) $(REL) -s s.defines.h

#../hdr/had.h:		../hdr/s.had.h
#	-chdir ../hdr; $(GET) $(REL) -s s.had.h

../com/comobj.a:
	-chdir ../com; Compile

clean:		# Procedure to clean up unwanted files
	-rm -f *.o
	-rm -f admin.c bdiff.c comb.c rmchg.c scv.c \
	       delta.c get.c help.c prt.c what.c val.c snull.c vc.c \
		cmt.c
	-rm -f admin	bdiff	comb	rmchg	scv	chghist	\
		delta	get	help	prt	what	rmdel	sccsdiff \
		val snull vc cmt
	-rm -f ../errors ../logs
