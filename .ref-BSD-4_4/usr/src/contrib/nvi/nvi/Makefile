#	@(#)Makefile	8.1 (Berkeley) 6/12/93

PROG=	nvi
CFLAGS=-g -DDEBUG
CFLAGS+=-I. -I${.CURDIR} -I${.CURDIR}/ex -I${.CURDIR}/vi
STRIP=
.PATH:	${.CURDIR}/ex ${.CURDIR}/sex ${.CURDIR}/vi ${.CURDIR}/svi

# General sources.
SRCS=	ascii.c cut.c delete.c exf.c filter.c line.c log.c main.c \
	mark.c options.c options_f.c screen.c search.c seq.c recover.c \
	term.c trace.c util.c

# Ex source.
SRCS+=	ex.c ex_abbrev.c ex_append.c ex_args.c ex_argv.c ex_at.c ex_bang.c \
	ex_bdisplay.c ex_cd.c ex_delete.c ex_digraph.c ex_edit.c \
	ex_equal.c ex_errlist.c ex_exit.c ex_file.c ex_global.c \
	ex_init.c ex_join.c ex_map.c ex_mark.c ex_mkexrc.c ex_move.c \
	ex_preserve.c ex_print.c ex_put.c ex_read.c ex_set.c ex_shell.c \
	ex_shift.c ex_source.c ex_split.c ex_stop.c ex_substitute.c \
	ex_tag.c ex_undo.c ex_usage.c ex_util.c ex_version.c ex_write.c \
	ex_yank.c excmd.c system.c

# Ex screen source.
SRCS+=	sex_confirm.c sex_get.c sex_refresh.c sex_screen.c sex_split.c \
	sex_util.c

# Vi source.
SRCS+=	getc.c v_again.c v_at.c v_ch.c v_comment.c v_delete.c \
	v_errlist.c v_ex.c v_exit.c v_exmode.c v_filter.c \
	v_increment.c v_init.c v_join.c v_left.c v_mark.c v_match.c \
	v_ntext.c v_paragraph.c v_put.c v_redraw.c v_replace.c \
	v_right.c v_scroll.c v_search.c v_section.c v_sentence.c \
	v_shift.c v_status.c v_stop.c v_switch.c v_tag.c v_text.c \
	v_ulcase.c v_undo.c v_util.c v_window.c v_word.c v_xchar.c \
	v_yank.c v_z.c vcmd.c vi.c

# Vi curses screen source.
SRCS+=	svi_confirm.c svi_ex.c svi_get.c svi_line.c svi_refresh.c \
	svi_relative.c svi_screen.c svi_smap.c svi_split.c svi_util.c

DPADD= ${LIBCURSES} ${LIBTERM}
LDADD=	-lcurses -ltermlib
SPECHDR=excmd.h options.h
NOMAN=	noman
CLEANFILES+=${SPECHDR}
LINKS=	${BINDIR}/nvi ${BINDIR}/nex

all: nvi
	rm -f nex
	ln nvi nex

warn:: ${SRCS}
	-(cd ${.CURDIR} && \
	    gcc -Wall -O -DDEBUG -Iobj -Ivi -Iex -I. ${.ALLSRC} \
	    -lcurses -ltermlib > /tmp/gout 2>&1)

# excmd.h depends on excmd.c, and vice-versa.  
options.h: options.h.stub # Makefile
	rm -f options.h
	cp ${.CURDIR}/options.h.stub options.h
	chmod 664 options.h
	(echo '/^\/\* O_[A-Z]* \*\/$$/ {'; \
	 echo 'printf("#define %s %d\n", $$2, cnt++)'; \
	 echo 'next'; \
	 echo '}'; \
	 echo 'END {'; \
	 echo 'printf("#define O_OPTIONCOUNT %d\n", cnt)'; \
	 echo '}') | awk -f /dev/stdin ${.CURDIR}/options.c >> options.h

excmd.h: excmd.h.stub # Makefile
	rm -f excmd.h
	cp ${.CURDIR}/ex/excmd.h.stub excmd.h
	chmod 664 excmd.h
	(echo '/^\/\* C_[A-Z]* \*\/$$/ {'; \
	 echo 'printf("#define %s %d\n", $$2, cnt++)'; \
	 echo 'next'; \
	 echo '}') | awk -f /dev/stdin ${.CURDIR}/ex/excmd.c >> excmd.h

.include "../../Makefile.inc"
.include <bsd.prog.mk>

.depend: ${SPECHDR}
