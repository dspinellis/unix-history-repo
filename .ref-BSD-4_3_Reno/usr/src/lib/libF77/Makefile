#	@(#)Makefile	5.12 (Berkeley) 6/23/90

LIB=	F77
SRCS=	abort_.c besj0_.c besj1_.c besjn_.c besy0_.c besy1_.c besyn_.c \
	c_abs.c c_cos.c c_div.c c_exp.c c_log.c c_sin.c c_sqrt.c d_abs.c \
	d_acos.c d_asin.c d_atan.c d_atn2.c d_cnjg.c d_cos.c d_cosh.c \
	d_dim.c d_dprod.c d_exp.c d_imag.c d_int.c d_lg10.c d_log.c \
	d_mod.c d_nint.c d_prod.c d_sign.c d_sin.c d_sinh.c d_tan.c d_tanh.c \
	dbesj0_.c dbesj1_.c dbesjn_.c dbesy0_.c dbesy1_.c dbesyn_.c derf_.c \
	derfc_.c ef1asc_.c ef1cmc_.c erf_.c erfc_.c exit_.c h_abs.c \
	h_dim.c h_dnnt.c h_indx.c h_len.c h_mod.c h_nint.c h_sign.c hl_ge.c \
	hl_gt.c hl_le.c hl_lt.c i_abs.c i_dim.c i_dnnt.c i_indx.c i_len.c \
	i_mod.c i_nint.c i_sign.c l_ge.c l_gt.c l_le.c l_lt.c lnblnk_.c \
	long_.c main.c pow_ci.c pow_dd.c pow_di.c pow_hh.c pow_ii.c pow_ri.c \
	pow_zi.c pow_zz.c r_abs.c r_acos.c r_asin.c r_atan.c r_atn2.c \
	r_cnjg.c r_cos.c r_cosh.c r_dim.c r_exp.c r_imag.c r_int.c \
	r_lg10.c r_log.c r_mod.c r_nint.c r_sign.c r_sin.c r_sinh.c r_sqrt.c \
	r_tan.c r_tanh.c rand_.c random_.c range.c rindex_.c s_cat.c \
	s_cmp.c s_copy.c s_paus.c s_rnge.c s_stop.c short_.c signal_.c \
	subout.c traper_.c trapov_.c trpfpe_.c z_cos.c z_div.c z_exp.c \
	z_log.c z_sin.c z_sqrt.c

LDADD=		Version.o
CLEANFILES+=	Usef Version.o Version.c mkvers

libF77.a libF77_p.a:: Version.o

Version.o: Version.c
	${CC} -R -c Version.c

Version.c: mkvers ${SRCS}
	mkvers libF77.a ${.ALLSRC:M*.c} > ${.TARGET}

mkvers: mkvers.c
	${CC} ${CFLAGS} ${.CURDIR}/mkvers.c -o ${.TARGET}

Usef: Usef.c
	${CC} ${CFLAGS} ${.CURDIR}/Usef.c -o ${.TARGET}

trapov_.o trapov_.po: trapov_.c
	${CC} -S -DUCBVAX ${.CURDIR}/trapov_.c;
	(echo "g/.set	L[0-9][0-9]*,0x0$$/s/0x0/0xf10/"; echo "x") | \
	    ex - trapov_.s;
	${AS} trapov_.s -o ${.TARGET}
	rm -f trapov_.s

.include <bsd.lib.mk>

# 'Usef' is part of a kuldge to get around a bug in the C compiler.
# C (sec. 9.10 of C ref manual) should round the return value of functions of
# type float from double to float.  It doesn't, so f77 truncates it losing a
# half bit of accuracy.  See file Test_float for a test.  When it is fixed,
# get rid of Usef & the extraneous stores in pow_ri, r_dim, r_mod, r_nint.

.if (${MACHINE} == "vax" || ${MACHINE} == "tahoe")
.c.o: Usef
	${CC} `Usef ${.PREFIX}` ${CFLAGS} -c ${.IMPSRC} -o ${.TARGET}
	@${LD} -x -r ${.TARGET}
	@mv a.out ${.TARGET}

.c.po: Usef
	${CC} -p `Usef ${.PREFIX}` ${CFLAGS} -c ${.IMPSRC} -o ${.TARGET}
	@${LD} -X -r ${.TARGET}
	@mv a.out ${.TARGET}
.endif
