#	@(#)Makefile	8.2 (Berkeley) %G%

LIBDB=	libdb.a
OBJ1=	bt_close.o bt_conv.o bt_debug.o bt_delete.o bt_get.o bt_open.o \
	bt_overflow.o bt_page.o bt_put.o bt_search.o bt_seq.o bt_split.o \
	bt_stack.o bt_utils.o
OBJ2=	db.o
OBJ3=	hash.o hash_bigkey.o hash_buf.o hash_func.o hash_log2.o hash_page.o \
	hsearch.o ndbm.o
OBJ4=	mpool.o
OBJ5=	rec_close.o rec_delete.o rec_get.o rec_open.o rec_put.o rec_search.o \
	rec_seq.o rec_utils.o

MISC=

${LIBDB}: ${OBJ1} ${OBJ2} ${OBJ3} ${OBJ4} ${OBJ5} ${MISC}
	ar cq $@ \
	    `lorder ${OBJ1} ${OBJ2} ${OBJ3} ${OBJ4} ${OBJ5} ${MISC} | tsort`
	ranlib $@

${OBJ1}:
	${CC} -c -O -I. -Iinclude -I../btree ../btree/*.c
${OBJ2}:
	${CC} -c -O -I. -Iinclude -I../db ../db/*.c
${OBJ3}:
	${CC} -c -O -I. -Iinclude -I../hash ../hash/*.c
${OBJ4}:
	${CC} -c -O -I. -Iinclude -I../mpool ../mpool/*.c
${OBJ5}:
	${CC} -c -O -I. -Iinclude -I../recno ../recno/*.c

memmove.o:
	${CC} -DMEMMOVE -c -O -I. -Iinclude clib/memmove.c
mktemp.o:
	${CC} -c -O -I. -Iinclude clib/mktemp.c
realloc.o:
	${CC} -c -O -I. -Iinclude clib/realloc.c
snprintf.o:
	${CC} -c -O -I. -Iinclude clib/snprintf.c

clean:
	rm -f ${LIBDB} ${OBJ1} ${OBJ2} ${OBJ3} ${OBJ4} ${OBJ5} ${MISC}
