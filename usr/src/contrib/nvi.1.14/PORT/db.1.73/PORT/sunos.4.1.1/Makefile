#	@(#)Makefile	8.3 (Berkeley) 9/6/93

LIBDB=	libdb.a
OBJ1=	hash.o hash_bigkey.o hash_buf.o hash_func.o hash_log2.o hash_page.o \
	hsearch.o ndbm.o
OBJ2=	bt_close.o bt_conv.o bt_debug.o bt_delete.o bt_get.o bt_open.o \
	bt_overflow.o bt_page.o bt_put.o bt_search.o bt_seq.o bt_split.o \
	bt_stack.o bt_utils.o
OBJ3=	db.o
OBJ4=	mpool.o
OBJ5=	rec_close.o rec_delete.o rec_get.o rec_open.o rec_put.o rec_search.o \
	rec_seq.o rec_utils.o

MISC=	memmove.o realloc.o snprintf.o

${LIBDB}: ${OBJ1} ${OBJ2} ${OBJ3} ${OBJ4} ${OBJ5} ${MISC}
	ar cq $@ \
	    `lorder ${OBJ1} ${OBJ2} ${OBJ3} ${OBJ4} ${OBJ5} ${MISC} | tsort`
	ranlib $@

CL=	${CC} -c -O -I. -Iinclude

hash.o: ../../hash/hash.c
	${CL} -I../../hash ../../hash/hash.c
hash_bigkey.o: ../../hash/hash_bigkey.c
	${CL} -I../../hash ../../hash/hash_bigkey.c
hash_buf.o: ../../hash/hash_buf.c
	${CL} -I../../hash ../../hash/hash_buf.c
hash_func.o: ../../hash/hash_func.c
	${CL} -I../../hash ../../hash/hash_func.c
hash_log2.o: ../../hash/hash_log2.c
	${CL} -I../../hash ../../hash/hash_log2.c
hash_page.o: ../../hash/hash_page.c
	${CL} -I../../hash ../../hash/hash_page.c
hsearch.o: ../../hash/hsearch.c
	${CL} -I../../hash ../../hash/hsearch.c
ndbm.o: ../../hash/ndbm.c
	${CL} -I../../hash ../../hash/ndbm.c

bt_close.o: ../../btree/bt_close.c
	${CL} -I../../btree ../../btree/bt_close.c
bt_conv.o: ../../btree/bt_conv.c
	${CL} -I../../btree ../../btree/bt_conv.c
bt_debug.o: ../../btree/bt_debug.c
	${CL} -I../../btree ../../btree/bt_debug.c
bt_delete.o: ../../btree/bt_delete.c
	${CL} -I../../btree ../../btree/bt_delete.c
bt_get.o: ../../btree/bt_get.c
	${CL} -I../../btree ../../btree/bt_get.c
bt_open.o: ../../btree/bt_open.c
	${CL} -I../../btree ../../btree/bt_open.c
bt_overflow.o: ../../btree/bt_overflow.c
	${CL} -I../../btree ../../btree/bt_overflow.c
bt_page.o: ../../btree/bt_page.c
	${CL} -I../../btree ../../btree/bt_page.c
bt_put.o: ../../btree/bt_put.c
	${CL} -I../../btree ../../btree/bt_put.c
bt_search.o: ../../btree/bt_search.c
	${CL} -I../../btree ../../btree/bt_search.c
bt_seq.o: ../../btree/bt_seq.c
	${CL} -I../../btree ../../btree/bt_seq.c
bt_split.o: ../../btree/bt_split.c
	${CL} -I../../btree ../../btree/bt_split.c
bt_stack.o: ../../btree/bt_stack.c
	${CL} -I../../btree ../../btree/bt_stack.c
bt_utils.o: ../../btree/bt_utils.c
	${CL} -I../../btree ../../btree/bt_utils.c

db.o: ../../db/db.c
	${CL} ../../db/db.c

mpool.o: ../../mpool/mpool.c
	${CL} -I../../mpool ../../mpool/mpool.c

rec_close.o: ../../recno/rec_close.c
	${CL} -I../../recno ../../recno/rec_close.c
rec_delete.o: ../../recno/rec_delete.c
	${CL} -I../../recno ../../recno/rec_delete.c
rec_get.o: ../../recno/rec_get.c
	${CL} -I../../recno ../../recno/rec_get.c
rec_open.o: ../../recno/rec_open.c
	${CL} -I../../recno ../../recno/rec_open.c
rec_put.o: ../../recno/rec_put.c
	${CL} -I../../recno ../../recno/rec_put.c
rec_search.o: ../../recno/rec_search.c
	${CL} -I../../recno ../../recno/rec_search.c
rec_seq.o: ../../recno/rec_seq.c
	${CL} -I../../recno ../../recno/rec_seq.c
rec_utils.o: ../../recno/rec_utils.c
	${CL} -I../../recno ../../recno/rec_utils.c

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
