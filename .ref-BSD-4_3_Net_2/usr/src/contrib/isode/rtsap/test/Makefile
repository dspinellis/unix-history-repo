CFLAGS = -g
test: rtsinit rtsresp

# local files we need to test our special version of rtsap
ROFILES = 
support.c rtsinit.c rtsresp.c: support.h

rts: rtsinit rtsresp

orts: ortsinit ortsresp

rtsinit: rtsinit.o support.o $(ROFILES)
	$(CC) -o rtsinit rtsinit.o support.o $(ROFILES) \
	../../libisode.a /usr/lib/isode/libisode.a -ldbm

ortsinit: rtsinit.o support.o 
	$(CC) -o ortsinit rtsinit.o support.o /usr/lib/isode/libisode.a -ldbm


rtsresp: rtsresp.o support.o $(ROFILES)
	$(CC) -o rtsresp rtsresp.o support.o $(ROFILES) ../../libisode.a \
	/usr/lib/isode/libisode.a -ldbm

ortsresp: rtsresp.o support.o 
	$(CC) -o ortsresp rtsresp.o support.o /usr/lib/isode/libisode.a -ldbm
