#	From: @(#)Makefile	5.3 (Berkeley) 4/1/91
#	$Id$

LIB=	des
CFLAGS+=-DKERBEROS -I. -I${.CURDIR}/../include
SRCS=	cbc_encrypt.c cksum.c debug_decl.c des.c key_parity.c \
	key_sched.c new_rnd_key.c pcbc_encrypt.c quad_cksum.c \
	random_key.c read_password.c string_to_key.c util.c weak_key.c
CLEANFILES+=key_perm.h s_table.h ip.c p.c fp.c odd.h p_table.h

TDIR=	${.CURDIR}/..
key_perm.h:
	cd ${TDIR}/make_key_perm; make
.if exists(${TDIR}/make_key_perm/obj)
	${TDIR}/make_key_perm/obj/make_key_perm key_perm.h
.else
	${TDIR}/make_key_perm/make_key_perm key_perm.h
.endif

p_table.h:
	cd ${TDIR}/make_p_table; make
.if exists(${TDIR}/make_p_table/obj)
	${TDIR}/make_p_table/obj/make_p_table p_table.h
.else
	${TDIR}/make_p_table/make_p_table p_table.h
.endif

s_table.h:
	cd ${TDIR}/make_s_table; make
.if exists(${TDIR}/make_s_table/obj)
	${TDIR}/make_s_table/obj/make_s_table s_table.h
.else
	${TDIR}/make_s_table/make_s_table s_table.h
.endif

ip.c:
	cd ${TDIR}/make_ip; make
.if exists(${TDIR}/make_ip/obj)
	${TDIR}/make_ip/obj/make_ip ip.c
.else
	${TDIR}/make_ip/make_ip ip.c
.endif

p.c:
	cd ${TDIR}/make_p; make
.if exists(${TDIR}/make_p/obj)
	${TDIR}/make_p/obj/make_p p.c
.else
	${TDIR}/make_p/make_p p.c
.endif

fp.c:
	cd ${TDIR}/make_fp; make
.if exists(${TDIR}/make_fp/obj)
	${TDIR}/make_fp/obj/make_fp fp.c
.else
	${TDIR}/make_fp/make_fp fp.c
.endif

odd.h:
	cd ${TDIR}/make_odd; make
.if exists(${TDIR}/make_odd/obj)
	${TDIR}/make_odd/obj/make_odd odd.h
.else
	${TDIR}/make_odd/make_odd odd.h
.endif

beforedepend:	key_perm.h p_table.h s_table.h ip.c p.c fp.c odd.h

.include <bsd.lib.mk>

