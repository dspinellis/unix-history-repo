ALL=	syscall csw signocsw \
	seqpage randpage gausspage \
	seqread randbread randread randrewrite \
	randwrite seqrewrite seqwrite \
	pipeback pipediscard pipeself
CFLAGS=-O

all:	${ALL}

syscall: syscall.c
	${CC} -o syscall ${CFLAGS} syscall.c

csw: csw.c
	${CC} -o csw ${CFLAGS} csw.c

gausspage: gausspage.c getpagesize.o
	${CC} -o gausspage ${CFLAGS} gausspage.c getpagesize.o -lnm

pipeback: pipeback.c
	${CC} -o pipeback ${CFLAGS} pipeback.c

pipediscard: pipediscard.c
	${CC} -o pipediscard ${CFLAGS} pipediscard.c

pipeself: pipeself.c
	${CC} -o pipeself ${CFLAGS} pipeself.c

randbread: randbread.c random.o
	${CC} -o randbread ${CFLAGS} randbread.c random.o

randpage: randpage.c random.o
	${CC} -o randpage ${CFLAGS} randpage.c random.o

randread: randread.c random.o
	${CC} -o randread ${CFLAGS} randread.c random.o

randrewrite: randrewrite.c random.o
	${CC} -o randrewrite ${CFLAGS} randrewrite.c random.o

randwrite: randwrite.c random.o
	${CC} -o randwrite ${CFLAGS} randwrite.c random.o

seqpage: seqpage.c getpagesize.o
	${CC} -o seqpage ${CFLAGS} seqpage.c getpagesize.o

seqread: seqread.c
	${CC} -o seqread ${CFLAGS} seqread.c

seqrewrite: seqrewrite.c
	${CC} -o seqrewrite ${CFLAGS} seqrewrite.c

seqwrite: seqwrite.c
	${CC} -o seqwrite ${CFLAGS} seqwrite.c

signocsw: signocsw.c
	${CC} -o signocsw ${CFLAGS} signocsw.c

clean:
	rm -f ${ALL} *.o core errs
