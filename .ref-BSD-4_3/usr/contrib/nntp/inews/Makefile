#
# Makefile for pseudo-inews
#

SRCS	=	inews.c ../common/clientlib.c
OBJS	=	inews.o ../common/clientlib.o
HFILES	=	../common/response_codes.h

CFLAGS	= -O

BINDIR	= /usr/new

all:	inews

inews:  $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o inews

../common/clientlib.o:
	${CC} ${CFLAGS} -c ../common/clientlib.c
	mv clientlib.o ../common/clientlib.o

install:
	cp inews ${BINDIR}/inews
	chmod 755 ${BINDIR}/inews

clean:
	rm -f ${OBJS} inews

lint:
	lint ${SRCS} ${HFILES}

tags:
	ctags $(CFILES) $(HFILES)
