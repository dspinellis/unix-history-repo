#	@(#)Makefile	5.12 (Berkeley) %G%

PROG=	routed
SRCS=	af.c if.c input.c main.c output.c startup.c tables.c timer.c \
	trace.c inet.c
MAN8=	routed.0
SUBDIR=	query trace
C2=	/usr/libexec/c2
INLINE=	/sys/${MACHINE}/inline/inline

.include <bsd.prog.mk>

# The following can be deleted where not appropriate to use the kernel's
# inline code expansions.
.c.o:
	${CC} -S ${CFLAGS} ${.CURDIR}/${.PREFIX}.c
	@${C2} ${.PREFIX}.s | ${INLINE} | ${AS} -o ${.PREFIX}.o
	@rm -f ${.PREFIX}.s
