MAKEFILE      = Makefile

PRINTER	      = Psp

PRINT	      = ditroff -${PRINTER}

SRCS	      = bib.1 \
		invert.1

SUFFIX	      = .1:s

all:;

clean:;

depend:;	@mkmf -f $(MAKEFILE)

install: ${DEST_MAN}/cat1/bib.0 ${DEST_MAN}/cat1/invert.0

${DEST_MAN}/cat1/bib.0: bib.1
		nroff -man -h bib.1 > ${DEST_MAN}/cat1/bib.0

${DEST_MAN}/cat1/invert.0: invert.1
		nroff -man -h invert.1 > ${DEST_MAN}/cat1/invert.0

#		@echo Installing $(SRCS) in $(DEST_MAN)/mann
#		@for i in $(SRCS); do cp $$i $(DEST_MAN)/mann/`basename $$i .1`.n; done

print:		$(SRCS)
		@$(PRINT) -man $?
		@touch print

update:;
