all: exrm exsummary exchanges edittut viin viapp visummary
# TROFF will frequently be just troff if you have a typesetter
# but no versatec.
TROFF=	/usr/ucb/vtroff
exrm:
	${TROFF} -ms ex.rm;
exsummary:
	tbl ex.summary | ${TROFF} -ms ;
exchanges:
	${TROFF} -ms exdiffs1.nr;
	${TROFF} -ms exdiffs2.nr;
edittut:
	${TROFF} -ms edit.tut;
# index for versatec is different from the one in edit.tut
# because the fonts are different and entries reference page
# rather than section numbers.  if you have a typesetter
# you should just use the index in edit.tut, and ignore editvindex.
editvindex:
	${TROFF} -ms -n22 edit.vindex;
viin:
	tbl vi.in | ${TROFF} -ms 
viapp:
# if using a typesetter use -n20 instead of -n22.
	tbl vi.chars | ${TROFF} -ms -n22
visummary:
	tbl vi.summary | ${TROFF} -ms
