# make [ OPTIONS= ]
CMD=/bin/make
CC = cc
DAYS=28
ARK=lib.a
TAR=tar
CPIO=cpio
TROFF=nroff
LPR=lpr
UTIME=$(SH).$(CPIO)
SH=ksh
JSH=jsh
ELIB=libedit.a
RJE= $(HOME)/rje
INSDIR = /usr/lbin
OPTIONS=
# the following make options are automatically configured but can be explicitly set
# options can be used to turn on (-)  or off (+) certain features 
# possible options and defaults are:
#	a	shell accounting	off
#	v	vi editmode		on
#	e	emacs edit mode		on
#	r	viraw mode		on
#	j	monitor(job control)	on
#	m	multibyte mode(on only)	automatically configured
#	f	vfork mode		on for BSD, off for Sys V
#	s	allow suid scripts	on and requires /etc/suid_exec
#	4.2	BSD 4.2			on for BSD 4.2, off for Sys V

LIBFILES = \
shlib/adjust.c \
shlib/arith.c \
shlib/assign.c \
shlib/assnum.c \
shlib/builtins.h \
shlib/cannon.c \
shlib/chkid.c \
shlib/convert.c \
shlib/failed.c \
shlib/findnod.c \
shlib/flags.h \
shlib/gettree.c \
shlib/gmatch.c \
shlib/growaray.c \
shlib/gsort.c \
shlib/linknod.c \
shlib/makefile \
shlib/name.h \
shlib/national.h \
shlib/namscan.c \
shlib/rjust.c \
shlib/shtype.h \
shlib/tilde.c \
shlib/unassign.c \
shlib/utos.c \
shlib/valup.c

SHFILES= \
sh/Makefile \
sh/apollo.c \
sh/args.c \
sh/blok.c \
sh/brkincr.h \
sh/builtin.c \
sh/builtins.h \
sh/cmd.c \
sh/ctype.c \
sh/defs.h \
sh/defs.c \
sh/edit.c \
sh/editlib.c \
sh/edit.h \
sh/emacs.c \
sh/echo.c \
sh/error.c \
sh/expand.c \
sh/fault.c \
sh/flags.h \
sh/history.c \
sh/history.h \
sh/io.c \
sh/io.h \
sh/jobs.c \
sh/jobs.h \
sh/macro.c \
sh/main.c \
sh/makefile \
sh/makelib \
sh/makesh \
sh/mode.h \
sh/msg.c \
sh/name.c \
sh/name.h \
sh/national.h \
sh/print.c \
sh/service.c \
sh/shtype.h \
sh/string.c \
sh/stdio.c \
sh/stak.c \
sh/stak.h \
sh/suid_exec.c \
sh/sym.h \
sh/syscall.s \
sh/test.c \
sh/test.h \
sh/timeout.h \
sh/vfork.c \
sh/vi.c \
sh/word.c \
sh/xec.c

JSHFILES= \
jsh/defs.h \
jsh/defs.c \
jsh/jsh.c \
jsh/makefile \
jsh/mpx.c

DOCFILES=\
getopts \
substring \
sh.memo \
sh.1 \
README \
RELEASE \
COMPATIBILITY \
ksh.mk

$(SH): shlib/$(ARK) sh/$(SH)
	-rm -f $(SH)
	ln sh/$(SH) $(SH)

cpio: $(SHFILES) $(LIBFILES) $(JSHFILES) $(DOCFILES) makefile
	ls $(SHFILES) $(LIBFILES) $(JSHFILES) $(DOCFILES) makefile \
	| cpio -ocBv > $(RJE)/$(SH).$@

ucpio: $(SHFILES) $(LIBFILES) $(JSHFILES) $(DOCFILES) makefile
	find $(SHFILES) $(LIBFILES) $(JSHFILES) $(DOCFILES) makefile   \
	-newer $(RJE)/$(UTIME)  -print | cpio -ocBv > $(RJE)/$(SH).$@

tar: $(SHFILES) $(LIBFILES) $(JSHFILES) $(DOCFILES) makefile
	tar -cvpf $(RJE)/$(SH).$@ $(SHFILES) $(LIBFILES) $(JSHFILES) $(DOCFILES)\
		 makefile

utar: $(SHFILES) $(LIBFILES) $(JSHFILES) $(DOCFILES) makefile
	tar -cvfp $(RJE)/$(SH).$@ `find \
	$(SHFILES) $(LIBFILES) $(JSHFILES) $(DOCFILES) makefile \
	-newer $(RJE)/$(UTIME)  -print`

shlib/$(ARK): $(LIBFILES)
	-cd shlib; if test -f /vmunix  \
		-o '(' -f /usr/include/sys/stream.h \
			 -a -f /usr/include/sys/vmparam.h ')'\
		-o -f /venix \
		-o "$$SYSTYPE" = bsd4.1 \
		-o "$$SYSTYPE" = bsd4.2 \
		-o "$$SYSTYPE" = bsd4.3 ;then \
			BSD="BSD=-DBSD ucb";else BSD=;fi  \
	;if test -f /usr/lib/dic/euc.dic \
		;then MULTIBYTE="MULTIBYTE=-DMULTIBYTE" \
		;else	case $$OPTIONS in \
			*-m*)	MULTIBYTE="MULTIBYTE=-DMULTIBYTE";; \
			*)	MULTIBYTE= ;; \
			esac \
		;fi \
	;$(CMD) CC=$(CC) ARK=$(ARK) $$BSD $$MULTIBYTE \
	;touch $(ARK)

sh/$(SH):	$(SHFILES) $(LIBFILES)
	cd sh;CMD=$(CMD) ARK=$(ARK) CC=$(CC) ./makesh $(OPTIONS) $(SH); touch $(SH)

jsh/$(JSH):	$(JSHFILES)
	cd jsh; if test -d /dev/sxt;then $(CMD) CC=$(CC) ; fi

sh/$(ELIB):
	cd sh;CMD=$(CMD) CC=$(CC) ./makelib; touch $(ELIB)

clean:
	cd shlib; $(CMD) clean
	cd sh; $(CMD) clean
	cd jsh; $(CMD) clean

clobber:
	cd shlib; $(CMD) clobber
	cd sh; $(CMD) clobber
	cd jsh; $(CMD) clobber
	rm -f $(SH) suid_exec

install:
	cd sh; $(CMD) install INSDIR=$(INSDIR)

xcl: $(SHFILES) $(LIBFILES) $(JSHFILES) makefile
	xcl $(SHFILES) $(LIBFILES) $(JSHFILES) makefile

man:	sh.1
	$(TROFF) -man sh.1 | $(LPR)
