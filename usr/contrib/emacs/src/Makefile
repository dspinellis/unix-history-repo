CPP = /lib/cpp

all: xemacs

xemacs: xmakefile doxemacs

doxemacs:
	make -f xmakefile ${MFLAGS} xemacs
.RECURSIVE: doxemacs

temacs: xmakefile dotemacs

dotemacs:
	make -f xmakefile ${MFLAGS} temacs
.RECURSIVE: dotemacs

xmakefile: ymakefile config.h
	rm -f xmakefile
	$(CPP) ymakefile | sed -e 's/^#.*//' -e 's/^[ \f	]$$//' | \
	sed -n -e '/^..*$$/p' > xmakefile
