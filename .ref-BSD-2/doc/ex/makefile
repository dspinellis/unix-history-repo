all: exrm exsummary exchanges edittut
exrm:
	/usr/new/vtroff -ms ex.rm;
exsummary:
	tbl ex.summary | /usr/new/vtroff -ms ;
exchanges:
	/usr/new/vtroff -ms ex.changes;
edittut:
	/usr/new/vtroff -ms edit.tut;
# index for versatec is different from the one in edit.tut
# because the fonts are different and entries reference page
# rather than section numbers.  if you have a typesetter
# you should just use the index in edit.tut, and ignore editvindex.
editvindex:
	/usr/new/vtroff -ms -n22 edit.vindex;
