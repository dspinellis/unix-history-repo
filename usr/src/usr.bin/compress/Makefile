#
#	@(#)Makefile	5.6 (Berkeley) %G%
#
COMFLAGS=-DBSD4_2 -O -DSACREDMEM=256000
BIN=${DESTDIR}/usr/ucb

compress: compress.c USERMEM
	cc $(COMFLAGS) -DUSERMEM=`cat USERMEM` -o compress compress.c

# USERMEM may have to be set by hand.  It should contain the amount of
# available user memory in bytes.  Set it to zero, for physical memory
# less than 1 Meg.
USERMEM:
	sh usermem.sh > USERMEM

install: compress
	install -s compress $(BIN)
	rm -f $(BIN)/uncompress $(BIN)/zcat
	ln $(BIN)/compress $(BIN)/uncompress
	ln $(BIN)/compress $(BIN)/zcat

# Temporarily don't delete USERMEM.  When chroot'ed to /nbsd, usermem.sh
# fails totally.
clean:
	rm -f compress core errs
#	rm -f compress USERMEM core errs

depend:
