#	@(#)Makefile	8.1 (Berkeley) 6/9/93

OBJ1=	bt_close.o bt_conv.o bt_debug.o bt_delete.o bt_get.o bt_open.o \
	bt_overflow.o bt_page.o bt_put.o bt_search.o bt_seq.o bt_split.o \
	bt_stack.o bt_utils.o
OBJ2=	db.o
OBJ3=	mpool.o
OBJ4=	rec_close.o rec_delete.o rec_get.o rec_open.o rec_put.o rec_search.o \
	rec_seq.o rec_utils.o
OBJ5=	regcomp.o regerror.o regexec.o regfree.o
OBJ6=	ascii.o cut.o delete.o exf.o filter.o line.o log.o main.o mark.o \
	options.o options_f.o recover.o screen.o search.o seq.o term.o \
	trace.o util.o
OBJ7=	ex.o ex_abbrev.o ex_append.o ex_args.o ex_argv.o ex_at.o \
	ex_bang.o ex_bdisplay.o ex_cd.o ex_delete.o ex_digraph.o \
	ex_edit.o ex_equal.o ex_errlist.o ex_exit.o ex_file.o ex_global.o \
	ex_init.o ex_join.o ex_map.o ex_mark.o ex_mkexrc.o ex_move.o \
	ex_preserve.o ex_print.o ex_put.o ex_read.o ex_set.o ex_shell.o \
	ex_shift.o ex_source.o ex_split.o ex_stop.o ex_substitute.o ex_tag.o \
	ex_undo.o ex_usage.o ex_util.o ex_version.o ex_write.o ex_yank.o \
	excmd.o system.o
OBJ8=	sex_confirm.o sex_get.o sex_refresh.o sex_screen.o sex_split.o \
	sex_util.o
OBJ9=	getc.o v_again.o v_at.o v_ch.o v_comment.o v_delete.o v_errlist.o \
	v_ex.o v_exit.o v_exmode.o v_filter.o v_increment.o v_init.o \
	v_join.o v_left.o v_mark.o v_match.o v_ntext.o v_paragraph.o \
	v_put.o v_redraw.o v_replace.o v_right.o v_scroll.o v_search.o \
	v_section.o v_sentence.o v_shift.o v_status.o v_stop.o v_switch.o \
	v_tag.o v_text.o v_ulcase.o v_undo.o v_util.o v_window.o \
	v_word.o v_xchar.o v_yank.o v_z.o vcmd.o vi.o
OBJ10=	svi_confirm.o svi_ex.o svi_get.o svi_line.o svi_refresh.o \
	svi_relative.o svi_screen.o svi_smap.o svi_split.o svi_util.o

# This is a curses library, basically the same as 4.3BSD's, with the addition
# of scrolling.  If you have a 4.3BSD curses library, uncomment these lines
# and use this one -- if you have a System V one, it should work fine.
#
# NOTE: If you use this library, remove the "-lcurses" from the load line
#	below, as well, and uncomment the "CURSES" assignment line below
#	where the compile flags are being set up.
#
#OBJ11=	addbytes.o addch.o addnstr.o box.o clear.o clrtobot.o clrtoeol.o \
#	cr_put.o ctrace.o curses.o delch.o deleteln.o delwin.o erase.o \
#	fullname.o getch.o getstr.o hash.o id_subwins.o idlok.o \
#	initscr.o insch.o insertln.o longname.o move.o mvwin.o newwin.o \
#	overlay.o overwrite.o putchar.o refresh.o scroll.o setterm.o \
#	standout.o toucholap.o touchwin.o tscroll.o tstp.o tty.o unctrl.o

# The file clib/glob.c is a superset of the POSIX 1003.2 glob routine.
# Nvi uses some of the 4BSD extensions, so you'll need to include it.
#
# If you don't have addnstr(3), add Xaddnstr.o (NOT addnstr.o!) to the
# MISC line.
#
# If you don't have strdup(3), add strdup.o to the MISC line.
#
# If you don't have memset(3), add memset.o to the MISC line.
#
# If you don't have memmove(3), add memmove.o to the MISC line.
#
# If you don't have mkstemp(3), add mktemp.o to the MISC line.
#
# If your version of curses doesn't have idlok, add idlok.o to the
# MISC line.
#
# If realloc(3) of a NULL pointer on your system isn't the same as
# a malloc(3) call, add realloc.o to the MISC line.
#
# If you don't have snprintf/vsnprintf(3), add snprintf.o to the MISC line.
# Note, this depends on your having vsprintf(3) -- if you don't, there's
# no workaround other than changing the source code to not use the snprintf
# calls.  If you have to make that change, check to see if your vsprintf
# returns a length or a char *; if it's the latter, set VSPRINTF_CHARSTAR
# in the include/compat.h file.
#
# If you don't have strerror(3), add strerror.o to the MISC line.
#
# If you don't have strsep(3), add strsep.o to the MISC line.
#
# If you don't have strtoul(3), add strtoul.o to the MISC line.
#
# If you don't have err(3) or warn(3), add err.o to the MISC line.
#
# If you don't have fwopen(3), read the PORT/README file, and modify
# vi/v_init.c.
#
MISC=	glob.o

ALL=	${OBJ1} ${OBJ2} ${OBJ3} ${OBJ4} ${OBJ5} ${OBJ6} ${OBJ7} \
	${OBJ8} ${OBJ9} ${OBJ10} ${OBJ11} ${MISC}

# Some vendors use "-ltermcap", not "-ltermlib".
nvi: options.h excmd.h ${ALL}
	${CC} -o $@ ${ALL} -lcurses -ltermlib
	rm -f nex
	ln $@ nex

options.h:
	rm -f options.h
	cp ../options.h.stub options.h
	chmod 664 options.h
	(echo '/^\/\* O_[A-Z]* \*\/$$/ {'; \
	 echo 'printf("#define %s %d\n", $$2, cnt++)'; \
	 echo 'next'; \
	 echo '}'; \
	 echo 'END {'; \
	 echo 'printf("#define O_OPTIONCOUNT %d\n", cnt)'; \
	 echo '}') | awk -f - ../options.c >> options.h

excmd.h:
	rm -f excmd.h
	cp ../ex/excmd.h.stub excmd.h
	chmod 664 excmd.h
	(echo '/^\/\* C_[A-Z]* \*\/$$/ {'; \
	 echo 'printf("#define %s %d\n", $$2, cnt++)'; \
	 echo 'next'; \
	 echo '}') | awk -f - ../ex/excmd.c >> excmd.h

clean:
	rm -f ${ALL} options.h excmd.h nvi nex

CL=	${CC} -c -g -I. -Iinclude

# If you're using the curses library provided with the distribution,
# uncomment the CURSES assignment line.
#CURSES=	-Icurses
INC=	-I. -I.. -I../ex -I../sex -I../vi -I../svi ${CURSES} -Iregex

bt_close.o: db/btree/bt_close.c
	${CL} -Idb/btree db/btree/bt_close.c
bt_conv.o: db/btree/bt_conv.c
	${CL} -Idb/btree db/btree/bt_conv.c
bt_debug.o: db/btree/bt_debug.c
	${CL} -Idb/btree db/btree/bt_debug.c
bt_delete.o: db/btree/bt_delete.c
	${CL} -Idb/btree db/btree/bt_delete.c
bt_get.o: db/btree/bt_get.c
	${CL} -Idb/btree db/btree/bt_get.c
bt_open.o: db/btree/bt_open.c
	${CL} -Idb/btree db/btree/bt_open.c
bt_overflow.o: db/btree/bt_overflow.c
	${CL} -Idb/btree db/btree/bt_overflow.c
bt_page.o: db/btree/bt_page.c
	${CL} -Idb/btree db/btree/bt_page.c
bt_put.o: db/btree/bt_put.c
	${CL} -Idb/btree db/btree/bt_put.c
bt_search.o: db/btree/bt_search.c
	${CL} -Idb/btree db/btree/bt_search.c
bt_seq.o: db/btree/bt_seq.c
	${CL} -Idb/btree db/btree/bt_seq.c
bt_split.o: db/btree/bt_split.c
	${CL} -Idb/btree db/btree/bt_split.c
bt_stack.o: db/btree/bt_stack.c
	${CL} -Idb/btree db/btree/bt_stack.c
bt_utils.o: db/btree/bt_utils.c
	${CL} -Idb/btree db/btree/bt_utils.c

db.o: db/db/db.c
	${CL} db/db/db.c

mpool.o: db/mpool/mpool.c
	${CL} -Idb/mpool db/mpool/mpool.c

rec_close.o: db/recno/rec_close.c
	${CL} -Idb/recno db/recno/rec_close.c
rec_delete.o: db/recno/rec_delete.c
	${CL} -Idb/recno db/recno/rec_delete.c
rec_get.o: db/recno/rec_get.c
	${CL} -Idb/recno db/recno/rec_get.c
rec_open.o: db/recno/rec_open.c
	${CL} -Idb/recno db/recno/rec_open.c
rec_put.o: db/recno/rec_put.c
	${CL} -Idb/recno db/recno/rec_put.c
rec_search.o: db/recno/rec_search.c
	${CL} -Idb/recno db/recno/rec_search.c
rec_seq.o: db/recno/rec_seq.c
	${CL} -Idb/recno db/recno/rec_seq.c
rec_utils.o: db/recno/rec_utils.c
	${CL} -Idb/recno db/recno/rec_utils.c

regcomp.o: regex/regcomp.c
	${CL} -Iregex regex/regcomp.c
regerror.o: regex/regerror.c
	${CL} -Iregex regex/regerror.c
regexec.o: regex/regexec.c
	${CL} -Iregex regex/regexec.c
regfree.o: regex/regfree.c
	${CL} -Iregex regex/regfree.c

ascii.o: ../ascii.c
	${CL} ${INC} ../ascii.c
cut.o: ../cut.c
	${CL} ${INC} ../cut.c
delete.o: ../delete.c
	${CL} ${INC} ../delete.c
exf.o: ../exf.c
	${CL} ${INC} ../exf.c
filter.o: ../filter.c
	${CL} ${INC} ../filter.c
line.o: ../line.c
	${CL} ${INC} ../line.c
log.o: ../log.c
	${CL} ${INC} ../log.c
main.o: ../main.c
	${CL} ${INC} ../main.c
mark.o: ../mark.c
	${CL} ${INC} ../mark.c
options.o: ../options.c
	${CL} ${INC} ../options.c
options_f.o: ../options_f.c
	${CL} ${INC} ../options_f.c
recover.o: ../recover.c
	${CL} ${INC} ../recover.c
screen.o: ../screen.c
	${CL} ${INC} ../screen.c
search.o: ../search.c
	${CL} ${INC} ../search.c
seq.o: ../seq.c
	${CL} ${INC} ../seq.c
term.o: ../term.c
	${CL} ${INC} ../term.c
trace.o: ../trace.c
	${CL} ${INC} ../trace.c
util.o: ../util.c
	${CL} ${INC} ../util.c

ex.o: ../ex/ex.c
	${CL} ${INC} ../ex/ex.c
ex_abbrev.o: ../ex/ex_abbrev.c
	${CL} ${INC} ../ex/ex_abbrev.c
ex_append.o: ../ex/ex_append.c
	${CL} ${INC} ../ex/ex_append.c
ex_args.o: ../ex/ex_args.c
	${CL} ${INC} ../ex/ex_args.c
ex_argv.o: ../ex/ex_argv.c
	${CL} ${INC} ../ex/ex_argv.c
ex_at.o: ../ex/ex_at.c
	${CL} ${INC} ../ex/ex_at.c
ex_bang.o: ../ex/ex_bang.c
	${CL} ${INC} ../ex/ex_bang.c
ex_bdisplay.o: ../ex/ex_bdisplay.c
	${CL} ${INC} ../ex/ex_bdisplay.c
ex_cd.o: ../ex/ex_cd.c
	${CL} ${INC} ../ex/ex_cd.c
ex_delete.o: ../ex/ex_delete.c
	${CL} ${INC} ../ex/ex_delete.c
ex_digraph.o: ../ex/ex_digraph.c
	${CL} ${INC} ../ex/ex_digraph.c
ex_edit.o: ../ex/ex_edit.c
	${CL} ${INC} ../ex/ex_edit.c
ex_equal.o: ../ex/ex_equal.c
	${CL} ${INC} ../ex/ex_equal.c
ex_errlist.o: ../ex/ex_errlist.c
	${CL} ${INC} ../ex/ex_errlist.c
ex_exit.o: ../ex/ex_exit.c
	${CL} ${INC} ../ex/ex_exit.c
ex_file.o: ../ex/ex_file.c
	${CL} ${INC} ../ex/ex_file.c
ex_global.o: ../ex/ex_global.c
	${CL} ${INC} ../ex/ex_global.c
ex_init.o: ../ex/ex_init.c
	${CL} ${INC} ../ex/ex_init.c
ex_join.o: ../ex/ex_join.c
	${CL} ${INC} ../ex/ex_join.c
ex_map.o: ../ex/ex_map.c
	${CL} ${INC} ../ex/ex_map.c
ex_mark.o: ../ex/ex_mark.c
	${CL} ${INC} ../ex/ex_mark.c
ex_mkexrc.o: ../ex/ex_mkexrc.c
	${CL} ${INC} ../ex/ex_mkexrc.c
ex_move.o: ../ex/ex_move.c
	${CL} ${INC} ../ex/ex_move.c
ex_print.o: ../ex/ex_print.c
	${CL} ${INC} ../ex/ex_print.c
ex_preserve.o: ../ex/ex_preserve.c
	${CL} ${INC} ../ex/ex_preserve.c
ex_put.o: ../ex/ex_put.c
	${CL} ${INC} ../ex/ex_put.c
ex_read.o: ../ex/ex_read.c
	${CL} ${INC} ../ex/ex_read.c
ex_set.o: ../ex/ex_set.c
	${CL} ${INC} ../ex/ex_set.c
ex_shell.o: ../ex/ex_shell.c
	${CL} ${INC} ../ex/ex_shell.c
ex_shift.o: ../ex/ex_shift.c
	${CL} ${INC} ../ex/ex_shift.c
ex_source.o: ../ex/ex_source.c
	${CL} ${INC} ../ex/ex_source.c
ex_split.o: ../ex/ex_split.c
	${CL} ${INC} ../ex/ex_split.c
ex_stop.o: ../ex/ex_stop.c
	${CL} ${INC} ../ex/ex_stop.c
ex_substitute.o: ../ex/ex_substitute.c
	${CL} ${INC} ../ex/ex_substitute.c
ex_tag.o: ../ex/ex_tag.c
	${CL} ${INC} ../ex/ex_tag.c
ex_undo.o: ../ex/ex_undo.c
	${CL} ${INC} ../ex/ex_undo.c
ex_usage.o: ../ex/ex_usage.c
	${CL} ${INC} ../ex/ex_usage.c
ex_util.o: ../ex/ex_util.c
	${CL} ${INC} ../ex/ex_util.c
ex_version.o: ../ex/ex_version.c
	${CL} ${INC} ../ex/ex_version.c
ex_write.o: ../ex/ex_write.c
	${CL} ${INC} ../ex/ex_write.c
ex_yank.o: ../ex/ex_yank.c
	${CL} ${INC} ../ex/ex_yank.c
excmd.o: ../ex/excmd.c
	${CL} ${INC} ../ex/excmd.c
system.o: ../ex/system.c
	${CL} ${INC} ../ex/system.c

sex_confirm.o: ../sex/sex_confirm.c
	${CL} ${INC} ../sex/sex_confirm.c
sex_get.o: ../sex/sex_get.c
	${CL} ${INC} ../sex/sex_get.c
sex_refresh.o: ../sex/sex_refresh.c
	${CL} ${INC} ../sex/sex_refresh.c
sex_screen.o: ../sex/sex_screen.c
	${CL} ${INC} ../sex/sex_screen.c
sex_split.o: ../sex/sex_split.c
	${CL} ${INC} ../sex/sex_split.c
sex_util.o: ../sex/sex_util.c
	${CL} ${INC} ../sex/sex_util.c

getc.o: ../vi/getc.c
	${CL} ${INC} ../vi/getc.c
v_again.o: ../vi/v_again.c
	${CL} ${INC} ../vi/v_again.c
v_at.o: ../vi/v_at.c
	${CL} ${INC} ../vi/v_at.c
v_ch.o: ../vi/v_ch.c
	${CL} ${INC} ../vi/v_ch.c
v_comment.o: ../vi/v_comment.c
	${CL} ${INC} ../vi/v_comment.c
v_delete.o: ../vi/v_delete.c
	${CL} ${INC} ../vi/v_delete.c
v_errlist.o: ../vi/v_errlist.c
	${CL} ${INC} ../vi/v_errlist.c
v_ex.o: ../vi/v_ex.c
	${CL} ${INC} ../vi/v_ex.c
v_exit.o: ../vi/v_exit.c
	${CL} ${INC} ../vi/v_exit.c
v_exmode.o: ../vi/v_exmode.c
	${CL} ${INC} ../vi/v_exmode.c
v_filter.o: ../vi/v_filter.c
	${CL} ${INC} ../vi/v_filter.c
v_increment.o: ../vi/v_increment.c
	${CL} ${INC} ../vi/v_increment.c
v_init.o: ../vi/v_init.c
	${CL} ${INC} ../vi/v_init.c
v_join.o: ../vi/v_join.c
	${CL} ${INC} ../vi/v_join.c
v_left.o: ../vi/v_left.c
	${CL} ${INC} ../vi/v_left.c
v_mark.o: ../vi/v_mark.c
	${CL} ${INC} ../vi/v_mark.c
v_match.o: ../vi/v_match.c
	${CL} ${INC} ../vi/v_match.c
v_ntext.o: ../vi/v_ntext.c
	${CL} ${INC} ../vi/v_ntext.c
v_paragraph.o: ../vi/v_paragraph.c
	${CL} ${INC} ../vi/v_paragraph.c
v_put.o: ../vi/v_put.c
	${CL} ${INC} ../vi/v_put.c
v_redraw.o: ../vi/v_redraw.c
	${CL} ${INC} ../vi/v_redraw.c
v_replace.o: ../vi/v_replace.c
	${CL} ${INC} ../vi/v_replace.c
v_right.o: ../vi/v_right.c
	${CL} ${INC} ../vi/v_right.c
v_scroll.o: ../vi/v_scroll.c
	${CL} ${INC} ../vi/v_scroll.c
v_search.o: ../vi/v_search.c
	${CL} ${INC} ../vi/v_search.c
v_section.o: ../vi/v_section.c
	${CL} ${INC} ../vi/v_section.c
v_sentence.o: ../vi/v_sentence.c
	${CL} ${INC} ../vi/v_sentence.c
v_shift.o: ../vi/v_shift.c
	${CL} ${INC} ../vi/v_shift.c
v_status.o: ../vi/v_status.c
	${CL} ${INC} ../vi/v_status.c
v_stop.o: ../vi/v_stop.c
	${CL} ${INC} ../vi/v_stop.c
v_switch.o: ../vi/v_switch.c
	${CL} ${INC} ../vi/v_switch.c
v_tag.o: ../vi/v_tag.c
	${CL} ${INC} ../vi/v_tag.c
v_text.o: ../vi/v_text.c
	${CL} ${INC} ../vi/v_text.c
v_ulcase.o: ../vi/v_ulcase.c
	${CL} ${INC} ../vi/v_ulcase.c
v_undo.o: ../vi/v_undo.c
	${CL} ${INC} ../vi/v_undo.c
v_util.o: ../vi/v_util.c
	${CL} ${INC} ../vi/v_util.c
v_window.o: ../vi/v_window.c
	${CL} ${INC} ../vi/v_window.c
v_word.o: ../vi/v_word.c
	${CL} ${INC} ../vi/v_word.c
v_xchar.o: ../vi/v_xchar.c
	${CL} ${INC} ../vi/v_xchar.c
v_yank.o: ../vi/v_yank.c
	${CL} ${INC} ../vi/v_yank.c
v_z.o: ../vi/v_z.c
	${CL} ${INC} ../vi/v_z.c
vcmd.o: ../vi/vcmd.c
	${CL} ${INC} ../vi/vcmd.c
vi.o: ../vi/vi.c
	${CL} ${INC} ../vi/vi.c

svi_confirm.o: ../svi/svi_confirm.c
	${CL} ${INC} ../svi/svi_confirm.c
svi_ex.o: ../svi/svi_ex.c
	${CL} ${INC} ../svi/svi_ex.c
svi_get.o: ../svi/svi_get.c
	${CL} ${INC} ../svi/svi_get.c
svi_line.o: ../svi/svi_line.c
	${CL} ${INC} ../svi/svi_line.c
svi_refresh.o: ../svi/svi_refresh.c
	${CL} ${INC} ../svi/svi_refresh.c
svi_relative.o: ../svi/svi_relative.c
	${CL} ${INC} ../svi/svi_relative.c
svi_screen.o: ../svi/svi_screen.c
	${CL} ${INC} ../svi/svi_screen.c
svi_smap.o: ../svi/svi_smap.c
	${CL} ${INC} ../svi/svi_smap.c
svi_split.o: ../svi/svi_split.c
	${CL} ${INC} ../svi/svi_split.c
svi_util.o: ../svi/svi_util.c
	${CL} ${INC} ../svi/svi_util.c

CINC=	-D_CURSES_PRIVATE -Icurses

addbytes.o: curses/addbytes.c
	${CL} ${CINC} curses/addbytes.c
addch.o: curses/addch.c
	${CL} ${CINC} curses/addch.c
addnstr.o: curses/addnstr.c
	${CL} ${CINC} curses/addnstr.c
box.o: curses/box.c
	${CL} ${CINC} curses/box.c
clear.o: curses/clear.c
	${CL} ${CINC} curses/clear.c
clrtobot.o: curses/clrtobot.c
	${CL} ${CINC} curses/clrtobot.c
clrtoeol.o: curses/clrtoeol.c
	${CL} ${CINC} curses/clrtoeol.c
cr_put.o: curses/cr_put.c
	${CL} ${CINC} curses/cr_put.c
ctrace.o: curses/ctrace.c
	${CL} ${CINC} curses/ctrace.c
curses.o: curses/curses.c
	${CL} ${CINC} curses/curses.c
delch.o: curses/delch.c
	${CL} ${CINC} curses/delch.c
deleteln.o: curses/deleteln.c
	${CL} ${CINC} curses/deleteln.c
delwin.o: curses/delwin.c
	${CL} ${CINC} curses/delwin.c
erase.o: curses/erase.c
	${CL} ${CINC} curses/erase.c
fullname.o: curses/fullname.c
	${CL} ${CINC} curses/fullname.c
getch.o: curses/getch.c
	${CL} ${CINC} curses/getch.c
getstr.o: curses/getstr.c
	${CL} ${CINC} curses/getstr.c
hash.o: curses/hash.c
	${CL} ${CINC} curses/hash.c
id_subwins.o: curses/id_subwins.c
	${CL} ${CINC} curses/id_subwins.c
idlok.o: curses/idlok.c
	${CL} ${CINC} curses/idlok.c
initscr.o: curses/initscr.c
	${CL} ${CINC} curses/initscr.c
insch.o: curses/insch.c
	${CL} ${CINC} curses/insch.c
insertln.o: curses/insertln.c
	${CL} ${CINC} curses/insertln.c
longname.o: curses/longname.c
	${CL} ${CINC} curses/longname.c
move.o: curses/move.c
	${CL} ${CINC} curses/move.c
mvwin.o: curses/mvwin.c
	${CL} ${CINC} curses/mvwin.c
newwin.o: curses/newwin.c
	${CL} ${CINC} curses/newwin.c
overlay.o: curses/overlay.c
	${CL} ${CINC} curses/overlay.c
overwrite.o: curses/overwrite.c
	${CL} ${CINC} curses/overwrite.c
printw.o: curses/printw.c
	${CL} ${CINC} curses/printw.c
putchar.o: curses/putchar.c
	${CL} ${CINC} curses/putchar.c
refresh.o: curses/refresh.c
	${CL} ${CINC} curses/refresh.c
scanw.o: curses/scanw.c
	${CL} ${CINC} curses/scanw.c
scroll.o: curses/scroll.c
	${CL} ${CINC} curses/scroll.c
setterm.o: curses/setterm.c
	${CL} ${CINC} curses/setterm.c
standout.o: curses/standout.c
	${CL} ${CINC} curses/standout.c
toucholap.o: curses/toucholap.c
	${CL} ${CINC} curses/toucholap.c
touchwin.o: curses/touchwin.c
	${CL} ${CINC} curses/touchwin.c
tscroll.o: curses/tscroll.c
	${CL} ${CINC} curses/tscroll.c
tstp.o: curses/tstp.c
	${CL} ${CINC} curses/tstp.c
tty.o: curses/tty.c
	${CL} ${CINC} curses/tty.c
unctrl.o: curses/unctrl.c
	${CL} ${CINC} curses/unctrl.c

Xaddnstr.o:
	${CL} clib/Xaddnstr.c
glob.o:
	${CL} clib/glob.c
memset.o:
	${CL} clib/memset.c
memmove.o:
	${CL} -DMEMMOVE clib/memmove.c
realloc.o:
	${CL} clib/realloc.c
snprintf.o:
	${CL} clib/snprintf.c
strerror.o:
	${CL} clib/strerror.c
strsep.o:
	${CL} clib/strsep.c
strtoul.o:
	${CL} clib/strtoul.c
err.o:
	${CL} clib/err.c
strdup.o:
	${CL} clib/strdup.c
idlok.o:
	${CL} clib/idlok.c
mktemp.o:
	${CL} clib/mktemp.c
