all: exrm exsummary exchanges edittut viin viapp visummary viapwh
# TROFF will frequently be just troff if you have a typesetter
# but no versatec.
TROFF=	/usr/ucb/vtroff -x
exrm:
	${TROFF} -ms ex.rm;
exsummary:
	tbl ex.summary | ${TROFF} -ms ;
exchanges:
	${TROFF} -ms ex1.1-2.0;
	${TROFF} -ms ex2.0-3.1;
	${TROFF} -ms ex3.1-3.5;
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
# numbers below are guessed at
# if using a typesetter use -n23 instead of -n25.
	tbl vi.chars | ${TROFF} -ms -n23
visummary:
	tbl vi.summary | ${TROFF} -ms
viapwh:
	${TROFF} -ms vi.apwh.ms;
