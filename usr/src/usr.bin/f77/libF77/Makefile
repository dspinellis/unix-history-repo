# Makefile for f77 math & startup lib, libF77.a
#
# @(#)Makefile	1.6 (Berkeley) %G%

CFLAGS	      = -O

DEST	      = $(DESTDIR)/usr/lib

DESTDIR	      =

EXTHDRS	      =

FFLAGS	      = -O

HDRS	      = opcodes.h

LIBRARY	      = libF77.a

LIBRARY_P     = libF77_p.a

MAKEFILE      = Makefile

OBJS	      = abort_.o \
		besj0_.o \
		besj1_.o \
		besjn_.o \
		besy0_.o \
		besy1_.o \
		besyn_.o \
		c_abs.o \
		c_cos.o \
		c_div.o \
		c_exp.o \
		c_log.o \
		c_sin.o \
		c_sqrt.o \
		cabs.o \
		d_abs.o \
		d_acos.o \
		d_asin.o \
		d_atan.o \
		d_atn2.o \
		d_cnjg.o \
		d_cos.o \
		d_cosh.o \
		d_dim.o \
		d_exp.o \
		d_imag.o \
		d_int.o \
		d_lg10.o \
		d_log.o \
		d_mod.o \
		d_nint.o \
		d_prod.o \
		d_sign.o \
		d_sin.o \
		d_sinh.o \
		d_sqrt.o \
		d_tan.o \
		d_tanh.o \
		dbesj0_.o \
		dbesj1_.o \
		dbesjn_.o \
		dbesy0_.o \
		dbesy1_.o \
		dbesyn_.o \
		derf_.o \
		derfc_.o \
		ef1asc_.o \
		ef1cmc_.o \
		erf_.o \
		erfc_.o \
		exit_.o \
		h_abs.o \
		h_dim.o \
		h_dnnt.o \
		h_indx.o \
		h_len.o \
		h_mod.o \
		h_nint.o \
		h_sign.o \
		hl_ge.o \
		hl_gt.o \
		hl_le.o \
		hl_lt.o \
		i_abs.o \
		i_dim.o \
		i_dnnt.o \
		i_indx.o \
		i_len.o \
		i_mod.o \
		i_nint.o \
		i_sign.o \
		l_ge.o \
		l_gt.o \
		l_le.o \
		l_lt.o \
		lnblnk_.o \
		long_.o \
		main.o \
		no_lg.o \
		pow_ci.o \
		pow_dd.o \
		pow_di.o \
		pow_hh.o \
		pow_ii.o \
		pow_ri.o \
		pow_zi.o \
		pow_zz.o \
		r_abs.o \
		r_acos.o \
		r_asin.o \
		r_atan.o \
		r_atn2.o \
		r_cnjg.o \
		r_cos.o \
		r_cosh.o \
		r_dim.o \
		r_exp.o \
		r_imag.o \
		r_int.o \
		r_lg10.o \
		r_log.o \
		r_mod.o \
		r_nint.o \
		r_sign.o \
		r_sin.o \
		r_sinh.o \
		r_sqrt.o \
		r_tan.o \
		r_tanh.o \
		rand_.o \
		random_.o \
		range.o \
		rindex_.o \
		s_cat.o \
		s_cmp.o \
		s_copy.o \
		s_paus.o \
		s_rnge.o \
		s_stop.o \
		short_.o \
		signal_.o \
		subout.o \
		traper_.o \
		trapov_.o \
		trpfpe_.o \
		z_abs.o \
		z_cos.o \
		z_div.o \
		z_exp.o \
		z_log.o \
		z_sin.o \
		z_sqrt.o

OLDDIR	      = $(DESTDIR)/usr/old/lib

OPTIONS	      = -DUCBVAX

PRINT	      = pr

SRCS	      = abort_.c \
		besj0_.c \
		besj1_.c \
		besjn_.c \
		besy0_.c \
		besy1_.c \
		besyn_.c \
		c_abs.c \
		c_cos.c \
		c_div.c \
		c_exp.c \
		c_log.c \
		c_sin.c \
		c_sqrt.c \
		cabs.c \
		d_abs.c \
		d_acos.c \
		d_asin.c \
		d_atan.c \
		d_atn2.c \
		d_cnjg.c \
		d_cos.c \
		d_cosh.c \
		d_dim.c \
		d_exp.c \
		d_imag.c \
		d_int.c \
		d_lg10.c \
		d_log.c \
		d_mod.c \
		d_nint.c \
		d_prod.c \
		d_sign.c \
		d_sin.c \
		d_sinh.c \
		d_sqrt.c \
		d_tan.c \
		d_tanh.c \
		dbesj0_.c \
		dbesj1_.c \
		dbesjn_.c \
		dbesy0_.c \
		dbesy1_.c \
		dbesyn_.c \
		derf_.c \
		derfc_.c \
		ef1asc_.c \
		ef1cmc_.c \
		erf_.c \
		erfc_.c \
		exit_.c \
		h_abs.c \
		h_dim.c \
		h_dnnt.c \
		h_indx.c \
		h_len.c \
		h_mod.c \
		h_nint.c \
		h_sign.c \
		hl_ge.c \
		hl_gt.c \
		hl_le.c \
		hl_lt.c \
		i_abs.c \
		i_dim.c \
		i_dnnt.c \
		i_indx.c \
		i_len.c \
		i_mod.c \
		i_nint.c \
		i_sign.c \
		l_ge.c \
		l_gt.c \
		l_le.c \
		l_lt.c \
		lnblnk_.c \
		long_.c \
		main.c \
		no_lg.c \
		pow_ci.c \
		pow_dd.c \
		pow_di.c \
		pow_hh.c \
		pow_ii.c \
		pow_ri.c \
		pow_zi.c \
		pow_zz.c \
		r_abs.c \
		r_acos.c \
		r_asin.c \
		r_atan.c \
		r_atn2.c \
		r_cnjg.c \
		r_cos.c \
		r_cosh.c \
		r_dim.c \
		r_exp.c \
		r_imag.c \
		r_int.c \
		r_lg10.c \
		r_log.c \
		r_mod.c \
		r_nint.c \
		r_sign.c \
		r_sin.c \
		r_sinh.c \
		r_sqrt.c \
		r_tan.c \
		r_tanh.c \
		rand_.c \
		random_.c \
		range.c \
		rindex_.c \
		s_cat.c \
		s_cmp.c \
		s_copy.c \
		s_paus.c \
		s_rnge.c \
		s_stop.c \
		short_.c \
		signal_.c \
		subout.c \
		traper_.c \
		trapov_.c \
		trpfpe_.c \
		z_abs.c \
		z_cos.c \
		z_div.c \
		z_exp.c \
		z_log.c \
		z_sin.c \
		z_sqrt.c

all:		$(LIBRARY) $(LIBRARY_P)

$(LIBRARY):	$(OBJS) Version
		@echo -n "Loading $(LIBRARY) ... "
		@ar cru $(LIBRARY) $(OBJS) Version
		@ranlib $(LIBRARY)
		@echo "done"

$(LIBRARY_P):	$(OBJS) Version
		@echo -n "Loading $(LIBRARY_P) ... "
		@cd profiled; ar cru ../$(LIBRARY_P) $(OBJS) Version
		@ranlib $(LIBRARY_P)
		@echo "done"

Version:	$(SRCS) mkvers
		@rm -f Version.c
		./mkvers $(LIBRARY) $(SRCS) > Version.c
		$(CC) -c Version.c
		mv Version.o Version
		@rm -f profiled/Version
		ln Version profiled/Version

mkvers:		mkvers.c
		@$(CC) mkvers.c -o mkvers

clean:;		@rm -f $(OBJS) profiled/*.o $(LIBRARY) $(LIBRARY_P)

depend:;	@echo Construct dependencies manually

extract:;	@ar xo $(DEST)/$(LIBRARY); rm -f __.SYMDEF
		@cd profiled; -ar xo $(DEST)/$(LIBRARY_P); rm -f __.SYMDEF

index:;		@ctags -wx $(HDRS) $(SRCS)

install:	$(LIBRARY) $(LIBRARY_P)
		@-mv $(DEST)/$(LIBRARY) $(OLDDIR)/$(LIBRARY)
		install -m 644 $(LIBRARY) $(DEST)/$(LIBRARY)
		ranlib $(DEST)/$(LIBRARY)
		@-mv $(DEST)/$(LIBRARY_P) $(OLDDIR)/$(LIBRARY_P)
		install -m 644 $(LIBRARY_P) $(DEST)/$(LIBRARY_P)
		ranlib $(DEST)/$(LIBRARY_P)

library:        $(LIBRARY) $(LIBRARY_P)

print:;		@$(PRINT) $(HDRS) $(SRCS)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

update:         $(DEST)/$(LIBRARY)

$(DEST)/$(LIBRARY): $(SRCS) $(HDRS) $(EXTHDRS)
		@-ar xo $(DEST)/$(LIBRARY)
		@cd profiled; -ar xo $(DEST)/$(LIBRARY_P)
		@make -f $(MAKEFILE) DEST=$(DEST) install clean

trapov_.o:	trapov_.c
		cc -S $(OPTIONS) trapov_.c;
		(echo "g/.set	L[0-9][0-9]*,0x0$$/s/0x0/0xf10/"; echo "x") \
			| ex - trapov_.s;
		as trapov_.s -o trapov_.o;
		mv trapov_.o profiled/trapov_.o
		rm trapov_.s
		cc -S $(OPTIONS) trapov_.c
		(echo "g/.set	L[0-9][0-9]*,0x0$$/s/0x0/0xf10/"; echo "x") \
			| ex - trapov_.s
		as trapov_.s -o trapov_.o
		rm trapov_.s
.c.o:
		$(CC) -p $(CFLAGS) -c $*.c
		-ld -x -r $*.o
		mv a.out profiled/$*.o
		$(CC) $(CFLAGS) -c $*.c
		-ld -x -r $*.o
		mv a.out $*.o
