CFLAGS = -g
CC=mcc $(CFLAGS)

# local files we need to test our special version of rosap
ROFILES = ../../pepsy/enc.o ../../pepsy/dec.o

all: test-p test-rt test-s

test-p: rospinit rospresp
	rospinit `hostname`

test-s: rossinit rossresp
	rossinit `hostname`

test-rt: rosrtinit rosrtresp
	rosrtinit `hostname`

rospinit: rospinit.o $(ROFILES)
	$(CC) -o rospinit rospinit.o $(ROFILES) \
	../../libisode.a /usr/lib/isode/libisode.a -ldbm

orospinit: rospinit.o
	$(CC) -o orospinit rospinit.o /usr/lib/isode/libisode.a -ldbm


rospresp: rospresp.o $(ROFILES)
	$(CC) -o rospresp rospresp.o $(ROFILES) ../../libisode.a \
	/usr/lib/isode/libisode.a -ldbm

orospresp: orospresp.o
	$(CC) -o rospresp rospresp.o /usr/lib/isode/libisode.a -ldbm

bug-rospresp: rospresp.c /usr/lib/isode/libisode.a
	$(CC) -o  bug-rospresp -DSHOW_BUG rospresp.c /usr/lib/isode/libisode.a -ldbm

rossinit: rossinit.o $(ROFILES) ssapinitiate.o
	$(CC) -o rossinit rossinit.o  ssapinitiate.o $(ROFILES) \
	../../libisode.a /usr/lib/isode/libisode.a -ldbm

orossinit: rossinit.o
	$(CC) -o orossinit rossinit.o /usr/lib/isode/libisode.a -ldbm


rossresp: rossresp.o $(ROFILES)
	$(CC) -o rossresp rossresp.o $(ROFILES)  \
	../../libisode.a /usr/lib/isode/libisode.a -ldbm

orossresp: rossresp.o
	$(CC) -o orossresp rossresp.o /usr/lib/isode/libisode.a -ldbm

rosrtinit: rosrtinit.o $(ROFILES)
	$(CC) -o rosrtinit rosrtinit.o $(ROFILES) \
	../../libisode.a /usr/lib/isode/libisode.a -ldbm

orosrtinit: rosrtinit.o
	$(CC) -o orosrtinit rosrtinit.o /usr/lib/isode/libisode.a -ldbm


rosrtresp: rosrtresp.o $(ROFILES)
	$(CC) -o rosrtresp rosrtresp.o $(ROFILES) ../../libisode.a \
	/usr/lib/isode/libisode.a -ldbm

orosrtresp: orosrtresp.o
	$(CC) -o rosrtresp rosrtresp.o /usr/lib/isode/libisode.a -ldbm

ssapinitiate.o: ssapinitiate.c
	$(CC) -c -I../../h ssapinitiate.c

#dec.o : dec.c
	#$(CC) -c -I../../pepsy -I../../h dec.c

orosrtbresp: rosrtbresp.o 
	$(CC) -o orosrtbresp rosrtbresp.o \
	/usr/lib/isode/libisode.a -ldbm

orosrtbinit: rosrtbinit.o 
	$(CC) -o orosrtbinit rosrtbinit.o \
	/usr/lib/isode/libisode.a -ldbm


rosrtbresp: rosrtbresp.o $(ROFILES)
	$(CC) -o rosrtbresp rosrtbresp.o $(ROFILES) ../../libisode.a \
	/usr/lib/isode/libisode.a -ldbm

rosrtbinit: rosrtbinit.o $(ROFILES)
	$(CC) -o rosrtbinit rosrtbinit.o $(ROFILES) \
	../../libisode.a /usr/lib/isode/libisode.a -ldbm

