#
# Copyright (c) 1990 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
#	@(#)Makefile	5.9 (Berkeley) %G%
#

PROG=	telnet

CFLAGS+=-DTERMCAP -DKLUDGELINEMODE -DUSE_TERMIO -DAUTHENTICATION -DENCRYPTION
CFLAGS+=-DENV_HACK
CFLAGS+=-I${.CURDIR}/../../lib

CFLAGS+= -DKRB4

LDADD=	-ltermcap -ltelnet
LDADD+=	-lkrb -ldes
DPADD=	${LIBTERMCAP}

SRCS=	authenc.c commands.c main.c network.c ring.c sys_bsd.c telnet.c \
	terminal.c tn3270.c utilities.c

# These are the sources that have encryption stuff in them.
CRYPT_SRC= authenc.c commands.c externs.h main.c network.c
CRYPT_SRC+= ring.c ring.h telnet.c terminal.c utilities.c Makefile
NOCRYPT_DIR=${.CURDIR}/Nocrypt

.include <bsd.prog.mk>

nocrypt:
#ifdef	ENCRYPTION
	@for i in ${CRYPT_SRC}; do \
	    if [ ! -d ${NOCRYPT_DIR} ]; then \
		echo Creating subdirectory ${NOCRYPT_DIR}; \
		mkdir ${NOCRYPT_DIR}; \
	    fi; \
	    echo ${NOCRYPT_DIR}/$$i; \
	    unifdef -UENCRYPTION ${.CURDIR}/$$i | \
		sed "s/ || defined(ENCRYPTION)//" > ${NOCRYPT_DIR}/$$i; \
	done

placeholder:
#else	/* ENCRYPTION */
	@echo "Encryption code already removed."
#endif	/* ENCRYPTION */
