CC = cc -I../cmds
CFLAGS=-O

strings.a: anoyes.o        \
	   components.o    \
	   current.o       \
	   defalt.o        \
	   distcomps.o     \
	   draft.o         \
	   fileproc.o      \
	   foldprot.o      \
	   hostname.o      \
	   installproc.o   \
	   listname.o      \
	   lockdir.o       \
	   lsproc.o        \
	   mailboxes.o     \
	   mailproc.o      \
	   mh_prof.o       \
	   mhnews.o        \
	   msgprot.o       \
	   pfolder.o       \
	   prproc.o        \
	   scanproc.o      \
	   sendproc.o      \
	   showproc.o      \
	   stdcomps.o      \
	   stddcomps.o     \
	   sysed.o         \
	   Mailprog.o      \
	   unixtomh.o      \
	   localname.o

	ar cr strings.a *.o

clean:
	rm -f *.o strings.a
