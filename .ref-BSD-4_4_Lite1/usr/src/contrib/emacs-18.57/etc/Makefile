CFLAGS= -g
# For Xenix.  Needed for movemail
#  LOADLIBES= -lx
# For Mips.  Needed for who knows what.
#  LOADLIBES = -lmld /usr/bsd43/usr/lib/libc.a

EXECUTABLES = test-distrib etags ctags wakeup make-docfile \
   digest-doc sorted-doc movemail cvtmail fakemail yow env \
   server emacsclient loadst

all: ${EXECUTABLES}

clean:
	-rm -f ${EXECUTABLES} core

distclean:
	-rm -f ${EXECUTABLES} *~ \#* DOC* core

# This justs verifies that the non-ASCII characters
# in the file `testfile' have no been clobbered by
# whatever means were used to copy and distribute Emacs.
# If they were clobbered, all the .elc files were clobbered too.
test-distrib: test-distrib.c
	$(CC) -o test-distrib test-distrib.c
	./test-distrib

etags: etags.c
	$(CC) -o etags ${CFLAGS} -DETAGS etags.c $(LOADLIBES)

ctags: etags.c
	$(CC) -o ctags ${CFLAGS} -DCTAGS etags.c $(LOADLIBES)

wakeup: wakeup.c
	$(CC) -o wakeup ${CFLAGS} wakeup.c $(LOADLIBES)

make-docfile: make-docfile.c
	$(CC) -o make-docfile ${CFLAGS} make-docfile.c $(LOADLIBES)

digest-doc: digest-doc.c
	$(CC) -o digest-doc ${CFLAGS} digest-doc.c $(LOADLIBES)

sorted-doc: sorted-doc.c
	$(CC) -o sorted-doc ${CFLAGS} sorted-doc.c $(LOADLIBES)

#
movemail: movemail.c ../src/config.h
	$(CC) -o movemail ${CFLAGS} movemail.c $(LOADLIBES)

cvtmail: cvtmail.c
	$(CC) -o cvtmail ${CFLAGS} cvtmail.c $(LOADLIBES)

fakemail: fakemail.c ../src/config.h
	$(CC) -o fakemail ${CFLAGS} fakemail.c $(LOADLIBES)

yow: yow.c ../src/paths.h
	$(CC) -o yow ${CFLAGS} yow.c $(LOADLIBES)

loadst: loadst.c ../src/config.h
	$(CC) -o loadst ${CFLAGS} loadst.c $(LOADLIBES)

# this is silly -- just use emacs to edit this file!
# (in any case this program doesn't preserve alphabetical ordering,
#  which is why I'm removing it)
#addyow: addyow.c
#	$(CC) -o addyow ${CFLAGS} addyow.c

env: env.c ../src/config.h
	$(CC) -o env -DEMACS ${CFLAGS} env.c $(LOADLIBES)

server: server.c ../src/config.h
	$(CC) -o server ${CFLAGS} server.c $(LOADLIBES)

emacsclient: emacsclient.c ../src/config.h
	$(CC) -o emacsclient ${CFLAGS} emacsclient.c $(LOADLIBES)

# This one is NOT included in EXECUTABLES.
# See ../src/ymakefile.
emacstool: emacstool.c
	$(CC) emacstool.c -o emacstool -g -lsuntool -lsunwindow -lpixrect $(LOADLIBES)

nemacstool: emacstool.c
	$(CC) -o nemacstool -g -DJLE ${CFLAGS} emacstool.c -lsuntool -lmle -lsunwindow -lpixrect $(LOADLIBES)	# For SUN Japanese Language Environment

xvetool: emacstool.c
	$(CC) -o xvetool -g -DXVIEW ${CFLAGS} emacstool.c -lxview -lX -I$(OPENWINHOME)/include -L$(OPENWINHOME)/lib $(LOADLIBES)
