#	@(#)Makefile	6.2 (Berkeley) 3/3/91

.include "../Makefile.g++"
.include "../../Makefile.inc"

PROG=		troff
HYPHENDIR=	/usr/share/misc
SRCS=		column.cc dictionary.cc div.cc env.cc input.cc \
		node.cc number.cc reg.cc symbol.cc
LDADD+=		$(.CURDIR)/../libgroff/obj/libgroff.a
# Delete the -Ig++-include after we fix libg++ and install it
GXXFLAGS+=	-I$(.CURDIR) -I$(.CURDIR)/../libgroff \
		-I$(.CURDIR)/../../../lib/libg++/g++-include \
		-DMACROPATH=\"/usr/share/tmac\" -DSTORE_WIDTH \
		-DHYPHENFILE=\"$(HYPHENDIR)/hyphen\"

afterinstall:
	install -c -o bin -g bin -m 444 $(.CURDIR)/hyphen \
		$(DESTDIR)$(HYPHENDIR)/hyphen

.include <bsd.prog.mk>
