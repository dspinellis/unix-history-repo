# This makefile compiles the source code for the "RSA Data Security, Inc.
# MD5 Message-Digest Algorithm" as found in RFC 1321.  That code is copyrighted:
#
#	Copyright (C) 1991-2, RSA Data Security, Inc. Created 1991.
#	All rights reserved.
#
# See the source code for copyright restrictions.
#
# No "makefile" is provided in the RFC.
# This one was written by Jim Ellis (jte@cert.org) for convenience.

# Note: a bug in mddriver causes "MD" to default to an incorrect value,
# so we set it to "5" here.
CFLAGS = -O -DMD=5

md5: md5c.o mddriver.o
	cc -o md5 md5c.o mddriver.o

mddriver.o: global.h md5.h
	cc -c $(CFLAGS) mddriver.c

md5c.o: global.h md5.h
	cc -c $(CFLAGS) md5c.c

test: md5 test.rfc
	-./md5 -x | diff - test.rfc > diffs 2>&1
	@-if test -s diffs ; then echo '*** MD5 TEST FAILED'; cat diffs; else echo '*** MD5 Test Passed'; fi
	rm -f diffs

# test.rfc is taken from Appendix 5 of RFC 1321.
test.rfc:
	echo 'MD5 test suite:' > test.rfc
	echo 'MD5 ("") = d41d8cd98f00b204e9800998ecf8427e' >> test.rfc
	echo 'MD5 ("a") = 0cc175b9c0f1b6a831c399e269772661' >> test.rfc
	echo 'MD5 ("abc") = 900150983cd24fb0d6963f7d28e17f72' >> test.rfc
	echo 'MD5 ("message digest") = f96b697d7cb7938d525a2f31aaf161d0' >> test.rfc
	echo 'MD5 ("abcdefghijklmnopqrstuvwxyz") = c3fcd3d76192e4007dfb496cca67e13b' >> test.rfc
	echo 'MD5 ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789") = d174ab98d277d9f5a5611c2c9f419d9f' >> test.rfc
	echo 'MD5 ("12345678901234567890123456789012345678901234567890123456789012345678901234567890") = 57edf4a22be3c955ac49da2e2107b67a' >> test.rfc

clean:
	rm -f *.o md5 diffs test.rfc
