#	@(#)Makefile	8.46 (Berkeley) 4/17/94

OBJ1=	bt_close.o bt_conv.o bt_debug.o bt_delete.o bt_get.o bt_open.o \
	bt_overflow.o bt_page.o bt_put.o bt_search.o bt_seq.o bt_split.o \
	bt_stack.o bt_utils.o
OBJ2=	hash.o hash_bigkey.o hash_buf.o hash_func.o hash_log2.o hash_page.o \
	hsearch.o ndbm.o
OBJ3=	db.o
OBJ4=	mpool.o
OBJ5=	rec_close.o rec_delete.o rec_get.o rec_open.o rec_put.o rec_search.o \
	rec_seq.o rec_utils.o
OBJ6=	regcomp.o regerror.o regexec.o regfree.o
OBJ7=	cut.o delete.o exf.o intr.o line.o log.o main.o mark.o options.o \
	options_f.o put.o recover.o screen.o search.o seq.o term.o timer.o \
	trace.o util.o
OBJ8=	ex.o ex_abbrev.o ex_append.o ex_args.o ex_argv.o ex_at.o ex_bang.o \
	ex_cd.o ex_delete.o ex_digraph.o ex_display.o ex_edit.o ex_equal.o \
	ex_exit.o ex_file.o ex_global.o ex_init.o ex_join.o ex_map.o \
	ex_mark.o ex_mkexrc.o ex_move.o ex_open.o ex_preserve.o ex_print.o \
	ex_put.o ex_read.o ex_set.o ex_screen.o ex_script.o ex_shell.o \
	ex_shift.o ex_source.o ex_stop.o ex_subst.o ex_tag.o ex_undo.o \
	ex_usage.o ex_util.o ex_version.o ex_visual.o ex_write.o ex_yank.o \
	ex_z.o excmd.o filter.o
OBJ9=	sex_confirm.o sex_get.o sex_refresh.o sex_screen.o sex_term.o \
	sex_util.o
OBJ10=	getc.o v_ch.o v_delete.o v_ex.o v_increment.o v_init.o v_left.o \
	v_mark.o v_match.o v_ntext.o v_paragraph.o v_put.o v_redraw.o \
	v_replace.o v_right.o v_screen.o v_scroll.o v_search.o v_section.o \
	v_sentence.o v_status.o v_stop.o v_text.o v_ulcase.o v_undo.o \
	v_util.o v_word.o v_xchar.o v_yank.o v_z.o v_zexit.o vcmd.o vi.o
OBJ11=	svi_confirm.o svi_ex.o svi_get.o svi_line.o svi_refresh.o \
	svi_relative.o svi_screen.o svi_smap.o svi_split.o svi_util.o
OBJ13=	xaw_screen.o

# This is a curses library, basically the same as 4.3BSD's, with the addition
# of scrolling.  If you have a 4.3BSD curses library, uncomment these lines
# and use this one -- if you have a System V one, it should work fine.
#
# NOTE: If you use this library, remove the "-lcurses" from the load line
#	below, as well, and uncomment the "CURSES" assignment line below
#	where the compile flags are being set up.
#
OBJ12=	addbytes.o addch.o addnstr.o box.o clear.o clrtobot.o clrtoeol.o \
	cr_put.o ctrace.o cur_hash.o curses.o delch.o deleteln.o delwin.o \
	erase.o fullname.o getch.o getstr.o id_subwins.o idlok.o \
	initscr.o insch.o insertln.o longname.o move.o mvwin.o newwin.o \
	overlay.o overwrite.o putchar.o refresh.o scroll.o setterm.o \
	standout.o toucholap.o touchwin.o tscroll.o tstp.o tty.o unctrl.o

MISC=	err.o fwopen.o pty.o snprintf.o

S=	../..
ALL=	${OBJ1} ${OBJ2} ${OBJ3} ${OBJ4} ${OBJ5} ${OBJ6} ${OBJ7} \
	${OBJ8} ${OBJ9} ${OBJ10} ${OBJ11} ${OBJ12} ${OBJ13} ${MISC}

# Some vendors use "-ltermcap", not "-ltermlib".
# If you're using the supplied curses library, delete "-lcurses".
nvi: options.h excmd.h ${ALL}
	echo 'Make sure you apply the patches in OTHER_PATCHES, first!'
	${CC} -o $@ ${ALL} -ltermcap 
	rm -f nex
	ln $@ nex

options.h:
	rm -f options.h
	cat $S/options.h.stub > options.h
	awk -f $S/options.awk $S/options.c >> options.h

excmd.h:
	rm -f excmd.h
	cat $S/ex/excmd.h.stub > excmd.h
	awk -f $S/ex/excmd.awk $S/ex/excmd.c >> excmd.h

clean:
	rm -f ${ALL} options.h excmd.h nvi nex

# If you need the fwopen functionality, add -DFWOPEN_NOT_AVAILABLE
# to this line.
OORG=	-O
CL=	${CC} -DFWOPEN_NOT_AVAILABLE -c ${OORG} -pipe -I. -Iinclude

# If you're using the curses library provided with the distribution,
# uncomment the CURSES assignment line.
 CURSES=	-Icurses
INC=	-I. -I$S -I$S/ex -I$S/sex -I$S/vi -I$S/svi ${CURSES} -Iregex

hash.o: db/hash/hash.c
	${CL} -Idb/hash db/hash/hash.c
hash_bigkey.o: db/hash/hash_bigkey.c
	${CL} -Idb/hash db/hash/hash_bigkey.c
hash_buf.o: db/hash/hash_buf.c
	${CL} -Idb/hash db/hash/hash_buf.c
hash_func.o: db/hash/hash_func.c
	${CL} -Idb/hash db/hash/hash_func.c
hash_log2.o: db/hash/hash_log2.c
	${CL} -Idb/hash db/hash/hash_log2.c
hash_page.o: db/hash/hash_page.c
	${CL} -Idb/hash db/hash/hash_page.c
hsearch.o: db/hash/hsearch.c
	${CL} -Idb/hash db/hash/hsearch.c
ndbm.o: db/hash/ndbm.c
	${CL} -Idb/hash db/hash/ndbm.c

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

cut.o: $S/cut.c
	${CL} ${INC} $S/cut.c
delete.o: $S/delete.c
	${CL} ${INC} $S/delete.c
exf.o: $S/exf.c
	${CL} ${INC} $S/exf.c
intr.o: $S/intr.c
	${CL} ${INC} $S/intr.c
line.o: $S/line.c
	${CL} ${INC} $S/line.c
log.o: $S/log.c
	${CL} ${INC} $S/log.c
main.o: $S/main.c
	${CL} ${INC} $S/main.c
mark.o: $S/mark.c
	${CL} ${INC} $S/mark.c
options.o: $S/options.c
	${CL} ${INC} $S/options.c
options_f.o: $S/options_f.c
	${CL} ${INC} $S/options_f.c
put.o: $S/put.c
	${CL} ${INC} $S/put.c
recover.o: $S/recover.c
	${CL} ${INC} $S/recover.c
screen.o: $S/screen.c
	${CL} ${INC} $S/screen.c
search.o: $S/search.c
	${CL} ${INC} $S/search.c
seq.o: $S/seq.c
	${CL} ${INC} $S/seq.c
term.o: $S/term.c
	${CL} ${INC} $S/term.c
timer.o: $S/timer.c
	${CL} ${INC} $S/timer.c
trace.o: $S/trace.c
	${CL} ${INC} $S/trace.c
util.o: $S/util.c
	${CL} ${INC} $S/util.c

ex.o: $S/ex/ex.c
	${CL} ${INC} $S/ex/ex.c
ex_abbrev.o: $S/ex/ex_abbrev.c
	${CL} ${INC} $S/ex/ex_abbrev.c
ex_append.o: $S/ex/ex_append.c
	${CL} ${INC} $S/ex/ex_append.c
ex_args.o: $S/ex/ex_args.c
	${CL} ${INC} $S/ex/ex_args.c
ex_argv.o: $S/ex/ex_argv.c
	${CL} ${INC} $S/ex/ex_argv.c
ex_at.o: $S/ex/ex_at.c
	${CL} ${INC} $S/ex/ex_at.c
ex_bang.o: $S/ex/ex_bang.c
	${CL} ${INC} $S/ex/ex_bang.c
ex_cd.o: $S/ex/ex_cd.c
	${CL} ${INC} $S/ex/ex_cd.c
ex_delete.o: $S/ex/ex_delete.c
	${CL} ${INC} $S/ex/ex_delete.c
ex_digraph.o: $S/ex/ex_digraph.c
	${CL} ${INC} $S/ex/ex_digraph.c
ex_display.o: $S/ex/ex_display.c
	${CL} ${INC} $S/ex/ex_display.c
ex_edit.o: $S/ex/ex_edit.c
	${CL} ${INC} $S/ex/ex_edit.c
ex_equal.o: $S/ex/ex_equal.c
	${CL} ${INC} $S/ex/ex_equal.c
ex_exit.o: $S/ex/ex_exit.c
	${CL} ${INC} $S/ex/ex_exit.c
ex_file.o: $S/ex/ex_file.c
	${CL} ${INC} $S/ex/ex_file.c
ex_global.o: $S/ex/ex_global.c
	${CL} ${INC} $S/ex/ex_global.c
ex_init.o: $S/ex/ex_init.c
	${CL} ${INC} $S/ex/ex_init.c
ex_join.o: $S/ex/ex_join.c
	${CL} ${INC} $S/ex/ex_join.c
ex_map.o: $S/ex/ex_map.c
	${CL} ${INC} $S/ex/ex_map.c
ex_mark.o: $S/ex/ex_mark.c
	${CL} ${INC} $S/ex/ex_mark.c
ex_mkexrc.o: $S/ex/ex_mkexrc.c
	${CL} ${INC} $S/ex/ex_mkexrc.c
ex_move.o: $S/ex/ex_move.c
	${CL} ${INC} $S/ex/ex_move.c
ex_open.o: $S/ex/ex_open.c
	${CL} ${INC} $S/ex/ex_open.c
ex_print.o: $S/ex/ex_print.c
	${CL} ${INC} $S/ex/ex_print.c
ex_preserve.o: $S/ex/ex_preserve.c
	${CL} ${INC} $S/ex/ex_preserve.c
ex_put.o: $S/ex/ex_put.c
	${CL} ${INC} $S/ex/ex_put.c
ex_read.o: $S/ex/ex_read.c
	${CL} ${INC} $S/ex/ex_read.c
ex_screen.o: $S/ex/ex_screen.c
	${CL} ${INC} $S/ex/ex_screen.c
ex_script.o: $S/ex/ex_script.c
	${CL} ${INC} $S/ex/ex_script.c
ex_set.o: $S/ex/ex_set.c
	${CL} ${INC} $S/ex/ex_set.c
ex_shell.o: $S/ex/ex_shell.c
	${CL} ${INC} $S/ex/ex_shell.c
ex_shift.o: $S/ex/ex_shift.c
	${CL} ${INC} $S/ex/ex_shift.c
ex_source.o: $S/ex/ex_source.c
	${CL} ${INC} $S/ex/ex_source.c
ex_stop.o: $S/ex/ex_stop.c
	${CL} ${INC} $S/ex/ex_stop.c
ex_subst.o: $S/ex/ex_subst.c
	${CL} ${INC} $S/ex/ex_subst.c
ex_tag.o: $S/ex/ex_tag.c
	${CL} ${INC} $S/ex/ex_tag.c
ex_undo.o: $S/ex/ex_undo.c
	${CL} ${INC} $S/ex/ex_undo.c
ex_usage.o: $S/ex/ex_usage.c
	${CL} ${INC} $S/ex/ex_usage.c
ex_util.o: $S/ex/ex_util.c
	${CL} ${INC} $S/ex/ex_util.c
ex_version.o: $S/ex/ex_version.c
	${CL} ${INC} $S/ex/ex_version.c
ex_visual.o: $S/ex/ex_visual.c
	${CL} ${INC} $S/ex/ex_visual.c
ex_write.o: $S/ex/ex_write.c
	${CL} ${INC} $S/ex/ex_write.c
ex_yank.o: $S/ex/ex_yank.c
	${CL} ${INC} $S/ex/ex_yank.c
ex_z.o: $S/ex/ex_z.c
	${CL} ${INC} $S/ex/ex_z.c
filter.o: $S/ex/filter.c
	${CL} ${INC} $S/ex/filter.c
excmd.o: $S/ex/excmd.c
	${CL} ${INC} $S/ex/excmd.c

sex_confirm.o: $S/sex/sex_confirm.c
	${CL} ${INC} $S/sex/sex_confirm.c
sex_get.o: $S/sex/sex_get.c
	${CL} ${INC} $S/sex/sex_get.c
sex_refresh.o: $S/sex/sex_refresh.c
	${CL} ${INC} $S/sex/sex_refresh.c
sex_screen.o: $S/sex/sex_screen.c
	${CL} ${INC} $S/sex/sex_screen.c
sex_term.o: $S/sex/sex_term.c
	${CL} ${INC} $S/sex/sex_term.c
sex_util.o: $S/sex/sex_util.c
	${CL} ${INC} $S/sex/sex_util.c

getc.o: $S/vi/getc.c
	${CL} ${INC} $S/vi/getc.c
v_ch.o: $S/vi/v_ch.c
	${CL} ${INC} $S/vi/v_ch.c
v_delete.o: $S/vi/v_delete.c
	${CL} ${INC} $S/vi/v_delete.c
v_ex.o: $S/vi/v_ex.c
	${CL} ${INC} $S/vi/v_ex.c
v_increment.o: $S/vi/v_increment.c
	${CL} ${INC} $S/vi/v_increment.c
v_init.o: $S/vi/v_init.c
	${CL} ${INC} $S/vi/v_init.c
v_left.o: $S/vi/v_left.c
	${CL} ${INC} $S/vi/v_left.c
v_mark.o: $S/vi/v_mark.c
	${CL} ${INC} $S/vi/v_mark.c
v_match.o: $S/vi/v_match.c
	${CL} ${INC} $S/vi/v_match.c
v_ntext.o: $S/vi/v_ntext.c
	${CL} ${INC} $S/vi/v_ntext.c
v_paragraph.o: $S/vi/v_paragraph.c
	${CL} ${INC} $S/vi/v_paragraph.c
v_put.o: $S/vi/v_put.c
	${CL} ${INC} $S/vi/v_put.c
v_redraw.o: $S/vi/v_redraw.c
	${CL} ${INC} $S/vi/v_redraw.c
v_replace.o: $S/vi/v_replace.c
	${CL} ${INC} $S/vi/v_replace.c
v_right.o: $S/vi/v_right.c
	${CL} ${INC} $S/vi/v_right.c
v_screen.o: $S/vi/v_screen.c
	${CL} ${INC} $S/vi/v_screen.c
v_scroll.o: $S/vi/v_scroll.c
	${CL} ${INC} $S/vi/v_scroll.c
v_search.o: $S/vi/v_search.c
	${CL} ${INC} $S/vi/v_search.c
v_section.o: $S/vi/v_section.c
	${CL} ${INC} $S/vi/v_section.c
v_sentence.o: $S/vi/v_sentence.c
	${CL} ${INC} $S/vi/v_sentence.c
v_status.o: $S/vi/v_status.c
	${CL} ${INC} $S/vi/v_status.c
v_stop.o: $S/vi/v_stop.c
	${CL} ${INC} $S/vi/v_stop.c
v_text.o: $S/vi/v_text.c
	${CL} ${INC} $S/vi/v_text.c
v_ulcase.o: $S/vi/v_ulcase.c
	${CL} ${INC} $S/vi/v_ulcase.c
v_undo.o: $S/vi/v_undo.c
	${CL} ${INC} $S/vi/v_undo.c
v_util.o: $S/vi/v_util.c
	${CL} ${INC} $S/vi/v_util.c
v_word.o: $S/vi/v_word.c
	${CL} ${INC} $S/vi/v_word.c
v_xchar.o: $S/vi/v_xchar.c
	${CL} ${INC} $S/vi/v_xchar.c
v_yank.o: $S/vi/v_yank.c
	${CL} ${INC} $S/vi/v_yank.c
v_z.o: $S/vi/v_z.c
	${CL} ${INC} $S/vi/v_z.c
v_zexit.o: $S/vi/v_zexit.c
	${CL} ${INC} $S/vi/v_zexit.c
vcmd.o: $S/vi/vcmd.c
	${CL} ${INC} $S/vi/vcmd.c
vi.o: $S/vi/vi.c
	${CL} ${INC} $S/vi/vi.c

svi_confirm.o: $S/svi/svi_confirm.c
	${CL} ${INC} $S/svi/svi_confirm.c
svi_ex.o: $S/svi/svi_ex.c
	${CL} ${INC} $S/svi/svi_ex.c
svi_get.o: $S/svi/svi_get.c
	${CL} ${INC} $S/svi/svi_get.c
svi_line.o: $S/svi/svi_line.c
	${CL} ${INC} $S/svi/svi_line.c
svi_refresh.o: $S/svi/svi_refresh.c
	${CL} ${INC} $S/svi/svi_refresh.c
svi_relative.o: $S/svi/svi_relative.c
	${CL} ${INC} $S/svi/svi_relative.c
svi_screen.o: $S/svi/svi_screen.c
	${CL} ${INC} $S/svi/svi_screen.c
svi_smap.o: $S/svi/svi_smap.c
	${CL} ${INC} $S/svi/svi_smap.c
svi_split.o: $S/svi/svi_split.c
	${CL} ${INC} $S/svi/svi_split.c
svi_util.o: $S/svi/svi_util.c
	${CL} ${INC} $S/svi/svi_util.c

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
cur_hash.o: curses/cur_hash.c
	${CL} ${CINC} curses/cur_hash.c
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

xaw_screen.o: $S/xaw/xaw_screen.c
	${CL} ${INC} $S/xaw/xaw_screen.c

# Random replacement and workaround sources.
Xaddnstr.o: clib/Xaddnstr.c
	${CL} clib/Xaddnstr.c
bsearch.o: clib/bsearch.c
	${CL} clib/bsearch.c
err.o: clib/err.c
	${CL} clib/err.c
fchmod.o: clib/fchmod.c
	${CL} ${INC} clib/fchmod.c
flock.o: clib/flock.c
	${CL} ${INC} clib/flock.c
fwopen.o: clib/fwopen.c
	${CL} ${INC} clib/fwopen.c
addidlok.o: clib/addidlok.c
	${CL} clib/addidlok.c
lockf.o: clib/lockf.c
	${CL} ${INC} clib/lockf.c
memchr.o: clib/memchr.c
	${CL} clib/memchr.c
memmove.o: clib/memmove.c
	${CL} -DMEMMOVE clib/memmove.c
memset.o: clib/memset.c
	${CL} clib/memset.c
mktemp.o: clib/mktemp.c
	${CL} clib/mktemp.c
pty.o: clib/pty.c
	${CL} clib/pty.c
pty_s5r4.o: clib/pty_s5r4.c
	${CL} clib/pty_s5r4.c
realloc.o: clib/realloc.c
	${CL} clib/realloc.c
siglist.o: clib/siglist.c
	${CL} clib/siglist.c
snprintf.o: clib/snprintf.c
	${CL} clib/snprintf.c
strdup.o: clib/strdup.c
	${CL} clib/strdup.c
strerror.o: clib/strerror.c
	${CL} clib/strerror.c
strsep.o: clib/strsep.c
	${CL} clib/strsep.c
strtoul.o: clib/strtoul.c
	${CL} clib/strtoul.c

# Local replacement and workaround sources.
