#	@(#)Makefile	8.1 (Berkeley) %G%

PROG=	ed
SRCS=	a.c add_line.c address.c bang.c c.c d.c e.c edhup.c equal.c f.c \
 	filename.c g.c get_line.c get_pattern.c i.c input_lines.c j.c k.c \
	l.c line_number.c m.c main.c p.c q.c r.c re.c rol.c sub.c search.c \
	t.c u.c w.c z.c

CFLAGS+=-DBSD -DDBI
LDADD=	-ltermcap
DPADD=	${TERMLIB}

.include <bsd.prog.mk>
