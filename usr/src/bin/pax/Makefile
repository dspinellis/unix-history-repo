#       @(#)Makefile	1.1 (Berkeley) %G%
#
#	Makefile for Pax
#
#	To install on versions prior to BSD4.4 the following may be defined
#	with CFLAGS:	
#	-DNET2_STAT	Use net2 or older stat structure
#	-DNET2_FTS	use older NET2 fts 
#	-DNET2_REGEX	use older regexp.h not regex.h

PROG=   pax
SRCS=	ar_io.c ar_subs.c buf_subs.c cache.c cpio.c file_subs.c ftree.c\
	gen_subs.c options.c pat_rep.c pax.c sel_subs.c tables.c tar.c\
	tty_subs.c

.include <bsd.prog.mk>
