ALL=	syscall csw signocsw \
	seqpage randpage gausspage \
	seqread randbread randread randrewrite \
	randwrite seqrewrite seqwrite \
	forks vforks execs vexecs nulljob bigjob \
	pipeback pipediscard pipeself \
	udgback udgself udgrand udgdiscard \
	countenv
CFLAGS=-O

all:	${ALL}

syscall: syscall.c
	${CC} -o syscall ${CFLAGS} syscall.c

csw: csw.c
	${CC} -o csw ${CFLAGS} csw.c

gausspage: gausspage.c valloc.o
	${CC} -o gausspage ${CFLAGS} gausspage.c valloc.o -lm

pipeback: pipeback.c
	${CC} -o pipeback ${CFLAGS} pipeback.c

pipediscard: pipediscard.c
	${CC} -o pipediscard ${CFLAGS} pipediscard.c

pipeself: pipeself.c
	${CC} -o pipeself ${CFLAGS} pipeself.c

udgback: udgback.c
	${CC} -o udgback ${CFLAGS} udgback.c

udgdiscard: udgdiscard.c
	${CC} -o udgdiscard ${CFLAGS} udgdiscard.c

udgself: udgself.c
	${CC} -o udgself ${CFLAGS} udgself.c

udgrand: udgrand.c random.o
	${CC} -o udgrand ${CFLAGS} udgrand.c random.o

randbread: randbread.c random.o
	${CC} -o randbread ${CFLAGS} randbread.c random.o

randpage: randpage.c random.o valloc.o
	${CC} -o randpage ${CFLAGS} randpage.c random.o valloc.o

randread: randread.c random.o
	${CC} -o randread ${CFLAGS} randread.c random.o

randrewrite: randrewrite.c random.o
	${CC} -o randrewrite ${CFLAGS} randrewrite.c random.o

randwrite: randwrite.c random.o
	${CC} -o randwrite ${CFLAGS} randwrite.c random.o

seqpage: seqpage.c valloc.o
	${CC} -o seqpage ${CFLAGS} seqpage.c valloc.o

seqread: seqread.c
	${CC} -o seqread ${CFLAGS} seqread.c

seqrewrite: seqrewrite.c
	${CC} -o seqrewrite ${CFLAGS} seqrewrite.c

seqwrite: seqwrite.c
	${CC} -o seqwrite ${CFLAGS} seqwrite.c

signocsw: signocsw.c
	${CC} -o signocsw ${CFLAGS} signocsw.c

forks: forks.c
	${CC} -o forks ${CFLAGS} forks.c

vforks: vforks.c
	${CC} -o vforks ${CFLAGS} vforks.c

execs: execs.c
	${CC} -o execs ${CFLAGS} execs.c

vexecs: vexecs.c
	${CC} -o vexecs ${CFLAGS} vexecs.c

nulljob: nulljob.c
	${CC} -o nulljob ${CFLAGS} nulljob.c

bigjob: bigjob.c
	${CC} -o bigjob ${CFLAGS} bigjob.c

countenv: countenv.c
	${CC} -o countenv ${CFLAGS} countenv.c

clean:
	rm -f ${ALL} *.o core errs
