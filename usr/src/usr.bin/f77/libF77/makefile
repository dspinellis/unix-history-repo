# Makefile for the f77 math & startup lib - libF77.a

#	@(#)makefile	1.11	%G%

LIB = libF77.a
LPR = lpr

# for the CFO 11's
#LIBDIR = /usr/ucb/lib
#TAPE = /dev/nrmt1

# for the VAX
LIBDIR = /usr/lib
TAPE = /dev/rnmt0.1600

MISC =	Version.o main.o s_rnge.o abort_.o exit_.o \
	s_stop.o s_paus.o rand_.o range.o traper_.o trapov_.o \
	dbesj0_.o dbesj1_.o dbesjn_.o dbesy0_.o dbesy1_.o dbesyn_.o \
	besj0_.o besj1_.o besjn_.o besy0_.o besy1_.o besyn_.o
POW =	pow_ci.o pow_dd.o pow_di.o pow_hh.o pow_ii.o  pow_ri.o pow_zi.o pow_zz.o
CX =	c_abs.o c_cos.o c_div.o c_exp.o c_log.o c_sin.o c_sqrt.o
DCX =	z_abs.o z_cos.o z_div.o z_exp.o z_log.o z_sin.o z_sqrt.o
REAL =	r_abs.o r_acos.o r_asin.o r_atan.o r_atn2.o r_cnjg.o r_cos.o\
	r_cosh.o r_dim.o r_exp.o r_imag.o r_int.o\
	r_lg10.o r_log.o r_mod.o r_nint.o r_sign.o\
	r_sin.o r_sinh.o r_sqrt.o r_tan.o r_tanh.o
DBL =	d_abs.o d_acos.o d_asin.o d_atan.o d_atn2.o\
	d_cnjg.o d_cos.o d_cosh.o d_dim.o d_exp.o\
	d_imag.o d_int.o d_lg10.o d_log.o d_mod.o\
	d_nint.o d_prod.o d_sign.o d_sin.o d_sinh.o\
	d_sqrt.o d_tan.o d_tanh.o 
INT =	i_abs.o i_dim.o i_dnnt.o i_indx.o i_len.o i_mod.o i_nint.o i_sign.o
HALF =	h_abs.o h_dim.o h_dnnt.o h_indx.o h_len.o h_mod.o  h_nint.o h_sign.o
CMP =	l_ge.o l_gt.o l_le.o l_lt.o hl_ge.o hl_gt.o hl_le.o hl_lt.o
EFL =	ef1asc_.o ef1cmc_.o
CHAR =	s_cat.o rindex_.o s_cmp.o s_copy.o lnblnk_.o

ALL =	$(MISC) $(POW) $(CX) $(DCX) $(REAL) $(DBL) $(INT) $(HALF) \
	$(CMP) $(EFL) $(CHAR) cabs.o tanh.o sinh.o

all:	$(LIB)

$(LIB):	$(ALL)
	@echo 'Loading...'
	@rm -f $(LIB)
	@ar rc $(LIB) $(ALL)
	@ls -l $(LIB)

c_abs.o c_cos.o c_div.o c_div.o c_exp.o c_log.o c_sin.o c_sqrt.o d_cnjg.o d_imag.o pow_ci.o pow_zi.o pow_zz.o r_cnjg.o r_imag.o z_abs.o z_cos.o z_div.o z_div.o z_exp.o z_log.o z_sin.o z_sqrt.o:	complex

trapov_.o: trapov_.c
	@cc -S trapov_.c
	@(echo "g/.set	L[0-9][0-9]*,0x0$$/s/0x0/0xf10/"; echo "x") \
		| ex - trapov_.s
	@as trapov_.s -o trapov_.o
	@rm trapov_.s

# compile, then strip unnecessary symbols
.c.o :
	cc -O -c -DVAX -DUCBVAX $*.c
	-ld -r -x $*.o
	mv a.out $*.o

install:	$(LIB)
	mv $(LIBDIR)/$(LIB) $(LIBDIR)/$(LIB).old
	cp $(LIB) $(LIBDIR)/$(LIB)
	ranlib $(LIBDIR)/$(LIB)
	chmod 644 $(LIBDIR)/$(LIB)

clean:
	rm -f $(ALL) $(LIB)

SOURCES = Version.c main.c s_rnge.c abort_.c exit_.c \
	s_stop.c s_paus.c rand_.c bit.c range.c traper_.c trapov_.c \
	dbesj0_.c dbesj1_.c dbesjn_.c dbesy0_.c dbesy1_.c dbesyn_.c \
	besj0_.c besj1_.c besjn_.c besy0_.c besy1_.c besyn_.c \
	cabs.c tanh.c sinh.c \
	pow_ci.c pow_dd.c pow_di.c pow_hh.c pow_ii.c  pow_ri.c pow_zi.c \
	pow_zz.c \
	c_abs.c c_cos.c c_div.c c_exp.c c_log.c c_sin.c c_sqrt.c \
	z_abs.c z_cos.c z_div.c z_exp.c z_log.c z_sin.c z_sqrt.c \
	r_abs.c r_acos.c r_asin.c r_atan.c r_atn2.c r_cnjg.c r_cos.c \
	r_cosh.c r_dim.c r_exp.c r_imag.c r_int.c \
	r_lg10.c r_log.c r_mod.c r_nint.c r_sign.c \
	r_sin.c r_sinh.c r_sqrt.c r_tan.c r_tanh.c \
	d_abs.c d_acos.c d_asin.c d_atan.c d_atn2.c \
	d_cnjg.c d_cos.c d_cosh.c d_dim.c d_exp.c \
	d_imag.c d_int.c d_lg10.c d_log.c d_mod.c \
	d_nint.c d_prod.c d_sign.c d_sin.c d_sinh.c \
	d_sqrt.c d_tan.c d_tanh.c \
	i_abs.c i_dim.c i_dnnt.c i_indx.c i_len.c i_mod.c i_nint.c i_sign.c \
	h_abs.c h_dim.c h_dnnt.c h_indx.c h_len.c h_mod.c  h_nint.c h_sign.c \
	l_ge.c l_gt.c l_le.c l_lt.c hl_ge.c hl_gt.c hl_le.c hl_lt.c \
	ef1asc_.c ef1cmc_.c \
	s_cat.c s_cmp.c s_copy.c lnblnk_.c rindex_.c

print:	index
	@pr index Makefile `ls $(SOURCES)` | $(LPR)

index:	$(SOURCES) mkindx
	@mkindx "libF77 Sources " Makefile $(SOURCES)

mkindx:	mkindx.c
	@cc mkindx.c -o mkindx

# End of Makefile
# Below this line is not normally distributed ...

Makefile:	makefile
	@sed -n -e "1,/End of Makefile/p" makefile > Makefile

tape:	Makefile
	@-ln man/s.makefile man.s.makefile
	@ls man.s.makefile > /dev/null
	@rm -f man/s.makefile
	@-tar cbf 20 $(TAPE) READ_ME Makefile man complex $(SOURCES) \
		mkindx.c bit.c subout.c
	@ln man.s.makefile man/s.makefile
	@rm -f man.s.makefile
