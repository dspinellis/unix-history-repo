CC=cc
COMFLAGS=-O
BIN=/usr/bin
MAN=/usr/man/man1
#define to "l" if manuals go on /usr/man/manl
L=1

all :	compress atob btoa

compress : compress.c USERMEM
	$(CC) $(COMFLAGS) -DUSERMEM=`cat USERMEM` -o compress compress.c

# USERMEM may have to be set by hand.  It should contain the amount of
# available user memory in bytes.  See the README file for more info.
USERMEM:
	sh usermem > USERMEM

atob:	atob.c
	$(CC) $(COMFLAGS) -o atob atob.c

btoa:	btoa.c
	$(CC) $(COMFLAGS) -o btoa btoa.c

install: compress atob btoa zmore zcmp zdiff compressdir uncompressdir btoa.1 compress.1 compressdir.1 zmore.1 zcmp.1 tarmail untarmail
	cp compress $(BIN)
	rm -f $(BIN)/uncompress $(BIN)/zcat
	ln $(BIN)/compress $(BIN)/uncompress
	ln $(BIN)/compress $(BIN)/zcat
	cp zmore zcmp zdiff compressdir uncompressdir $(BIN)
	cp atob btoa tarmail untarmail $(BIN)
	cp btoa.1 $(MAN)/btoa.$(L)
	rm -f $(MAN)/atob.$(L) $(MAN)/tarmail.$(L) $(MAN)/untarmail.$(L)
	ln $(MAN)/btoa.$(L) $(MAN)/atob.$(L)
	ln $(MAN)/btoa.$(L) $(MAN)/tarmail.$(L)
	ln $(MAN)/btoa.$(L) $(MAN)/untarmail.$(L)
	cp compress.1 $(MAN)/compress.$(L)
	rm -f $(MAN)/uncompress.$(L) $(MAN)/zcat.$(L)
	ln $(MAN)/compress.$(L) $(MAN)/uncompress.$(L)
	ln $(MAN)/compress.$(L) $(MAN)/zcat.$(L)
	cp compressdir.1 $(MAN)/compressdir.$(L)
	cp zmore.1 $(MAN)/zmore.$(L)
	cp zcmp.1 $(MAN)/zcmp.$(L)
	rm -f $(MAN)/zdiff.$(L)
	ln $(MAN)/zcmp.$(L) $(MAN)/zdiff.$(L)

clean:
	rm -f compress atob btoa
