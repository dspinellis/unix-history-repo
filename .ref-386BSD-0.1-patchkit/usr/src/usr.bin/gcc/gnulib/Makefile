#	@(#)Makefile	6.4 (Berkeley) 1/30/91

LIB=		gnulib
CFLAGS+=	-I. -I$(.CURDIR)/../cc1 -I$(.CURDIR)/../cc1/config
NOMAN=		noman
.PATH: $(.CURDIR)/$(MACHINE) $(.CURDIR)/g++ $(.CURDIR)/longlong

.include "$(MACHINE)/Makefile.machine"
GPLUS_SRCS=	builtin_New.c builtin_del.c builtin_new.c eprintf.c
LONGLONG_SRCS=	adddi3.c anddi3.c ashldi3.c ashrdi3.c bdiv.c \
		cmpdi2.c divdi3.c fixdfdi.c fixunsdfdi.c \
		floatdidf.c iordi3.c lshldi3.c lshrdi3.c \
		moddi3.c muldi3.c negdi2.c one_cmpldi2.c \
		subdi3.c ucmpdi2.c udivdi3.c umoddi3.c xordi3.c
SRCS=		$(MACHINE_SRCS) $(GPLUS_SRCS) $(LONGLONG_SRCS)

$(OBJS):	config.h

afterinstall:
	-rm -f $(DESTDIR)$(LIBDIR)/gcc-gnulib
	cd $(DESTDIR)$(LIBDIR); ln -s libgnulib.a gcc-gnulib

.include <bsd.lib.mk>
.include "../Makefile.symlinks"
