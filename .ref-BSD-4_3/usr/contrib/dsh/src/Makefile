#
#   Make the user command and the bidding program
#
all: dbid dsh

dbid: getloadave.o getpath.o dbid.o error.o getrc.o ipc.o
	cc getloadave.o getpath.o dbid.o error.o getrc.o ipc.o -o dbid

dsh: dsh.o error.o getrc.o ipc.o getwd.o getbids.o
	cc -o dsh dsh.o error.o getrc.o ipc.o getwd.o getbids.o

dbid.o: dsh.h dbid.h
getbids.o: dsh.h dbid.h
dsh.o: dsh.h

#
#   install them
#
install:
	install -s -g kmem -m 2755 dbid ${DESTDIR}/usr/new/dbid
	install -s dsh ${DESTDIR}/usr/new/dsh

#
#   clean out the reproducible files
#
clean:
	rm -f *.o dbid dsh
