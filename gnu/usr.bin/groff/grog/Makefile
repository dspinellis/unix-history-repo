MAN1=		grog.0

afterinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
		$(.CURDIR)/grog.pl $(DESTDIR)$(BINDIR)/grog

.include <bsd.prog.mk>
.include <../../../usr.bin/Makefile.inc>
