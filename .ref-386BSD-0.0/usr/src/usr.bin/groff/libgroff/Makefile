#	@(#)Makefile	6.2 (Berkeley) 4/30/91

.include "../Makefile.g++"
.include "../Makefile.fontdir"

LIB=		groff
SRCS=		assert.cc change_lf.cc cmap.cc cset.cc errarg.cc \
		error.cc fatal.cc filename.cc font.cc fontfile.cc \
		iftoa.cc itoa.cc lf.cc lineno.cc nametoindex.cc \
		new.cc progname.cc ptable.cc string.cc strsave.cc \
		version.cc
GXXFLAGS+=	-I$(.CURDIR) -I$(.CURDIR)/../../../lib/libg++/g++-include \
		-DFONTPATH=\"$(FONTDIR)\"
NOMAN=		noman
NOPROFILE=	noprofile

install:

.include <386bsd.lib.mk>

.SUFFIXES: .cc
