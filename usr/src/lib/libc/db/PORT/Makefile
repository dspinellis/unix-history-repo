#	@(#)Makefile	5.2 (Berkeley) %G%

OBJ1=	bt_close.o bt_conv.o bt_debug.o bt_delete.o bt_get.o bt_open.o \
	bt_overflow.o bt_page.o bt_put.o bt_search.o bt_seq.o bt_split.o \
	bt_stack.o bt_utils.o
OBJ2=	db.o
OBJ3=	hash.o hash_bigkey.o hash_buf.o hash_func.o hash_log2.o hash_page.o \
	hsearch.o ndbm.o
OBJ4=	mpool.o
OBJ5=	rec_close.o rec_delete.o rec_get.o rec_open.o rec_put.o rec_search.o \
	rec_seq.o rec_utils.o
# If you need compatibility routines
COMP=	bcopy.o mktemp.o

db.a: ${OBJ1} ${OBJ2} ${OBJ3} ${OBJ4} ${OBJ5} ${COMP}
	ar cq $@ \
	    `lorder ${OBJ1} ${OBJ2} ${OBJ3} ${OBJ4} ${OBJ5} ${COMP} | tsort`
	ranlib $@

${OBJ1}:
	${CC} -c -O -I../btree -I../include ../btree/$*.c
${OBJ2}:
	${CC} -c -O -I../db -I../include ../db/$*.c
${OBJ3}:
	${CC} -c -O -I../hash -I../include ../hash/$*.c
${OBJ4}:
	${CC} -c -O -I../mpool -I../include ../mpool/$*.c
${OBJ5}:
	${CC} -c -O -I../recno -I../include ../recno/$*.c
${COMP}:
	${CC} -c -O -I../port -I../include ../port/$*.c

clean:
	rm -f db.a ${OBJ1} ${OBJ2} ${OBJ3} ${OBJ4} ${OBJ5} ${COMP}
