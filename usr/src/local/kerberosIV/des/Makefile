#	@(#)Makefile	5.2 (Berkeley) %G%

LIB=	des
CFLAGS+=-DKERBEROS -I. -I${.CURDIR}/../include
SRCS=	cbc_encrypt.c cksum.c debug_decl.c des.c key_parity.c \
	key_sched.c new_rnd_key.c pcbc_encrypt.c quad_cksum.c \
	random_key.c read_password.c string_to_key.c util.c weak_key.c
CLEANFILES+=key_perm.h s_table.h ip.c p.c fp.c odd.h p_table.h

TDIR=	${.CURDIR}/..
key_perm.h: ${TDIR}/make_key_perm/obj/make_key_perm
	${TDIR}/make_key_perm/obj/make_key_perm key_perm.h

p_table.h: ${TDIR}/make_p_table/obj/make_p_table
	${TDIR}/make_p_table/obj/make_p_table p_table.h

s_table.h: ${TDIR}/make_s_table/obj/make_s_table
	${TDIR}/make_s_table/obj/make_s_table s_table.h

ip.c: ${TDIR}/make_ip/obj/make_ip
	${TDIR}/make_ip/obj/make_ip ip.c

p.c: ${TDIR}/make_p/obj/make_p
	${TDIR}/make_p/obj/make_p p.c

fp.c: ${TDIR}/make_fp/obj/make_fp
	${TDIR}/make_fp/obj/make_fp fp.c

odd.h: ${TDIR}/make_odd/obj/make_odd
	${TDIR}/make_odd/obj/make_odd odd.h

.include <bsd.lib.mk>

${OBJS} ${POBJS} .depend: key_perm.h p_table.h s_table.h ip.c p.c fp.c odd.h
