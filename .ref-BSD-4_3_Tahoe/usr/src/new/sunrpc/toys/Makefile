CFLAGS= -O

all: sortit sort_service

sortit: sortit.o sort_prot.o
	${CC} ${CFLAGS} sortit.o sort_prot.o -o sortit

sort_service: sort_service.o sort_prot.o
	${CC} ${CFLAGS} sort_service.o sort_prot.o -o sort_service

.c.o:
	${CC} ${CFLAGS} -c $*.c

