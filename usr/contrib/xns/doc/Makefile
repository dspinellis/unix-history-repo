TRFLAGS=
RCSREV=
RCSNAME=current
MAN3=except.n xnscourier.n clearinghouse.n xnsftp.n xnsprint.n

all: ${MAN3}

troff: courier.tbl.me
	tbl courier.tbl.me | ditroff ${TRFLAGS} -me -t > troff

install: troff ${MAN3}
	cp ${MAN3} ${DESTDIR}/usr/man/mann

clearinghouse.n: clearinghousesupport.3
	cp  clearinghousesupport.3 clearinghouse.n

rcs:
	rcs -l RCS/*
	cat ../rcsdescription | ci -u${RCSREV} -N${RCSNAME} -q RCS/*

clean:
	-rm -f *.CKP *.BAK ch.n troff
