# This Makefile for building libbuiltins.a is in -*- text -*- for Emacs.
#
MKBUILTINS = mkbuiltins
RANLIB = /usr/bin/ranlib
CFLAGS = -g -I.. -I.
SHELL = /bin/sh
CC = cc
AR = ar
RM = rm -f
CP = cp

.SUFFIXES:
.SUFFIXES: .def .c .o
# How to make a .o file from a .def file.
.def.o:
	$(RM) $@
	./$(MKBUILTINS) $(DIRECTDEFINE) $*.def
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $*.c
	$(RM) $*.c

# How to make a .c file from a .def file.
.def.c:
	$(RM) $@
	./$(MKBUILTINS) $(DIRECTDEFINE) $*.def

# Here is a rule for making .o files from .c files that does not
# force the type of the machine (like -M_MACHINE) into the flags.
.c.o:
	$(RM) $@
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $*.c

DEFS =  alias.def bind.def break.def builtin.def cd.def colon.def \
	command.def declare.def echo.def enable.def eval.def \
	exec.def exit.def fc.def fg_bg.def hash.def help.def history.def \
	jobs.def kill.def let.def read.def return.def set.def setattr.def \
	shift.def source.def suspend.def test.def times.def trap.def \
	type.def ulimit.def umask.def wait.def getopts.def reserved.def

STATIC_SOURCE = common.c getopt.c getopt.h

OFILES = alias.o bind.o break.o builtin.o cd.o colon.o command.o \
	common.o declare.o echo.o enable.o eval.o exec.o exit.o \
	fc.o fg_bg.o hash.o help.o history.o jobs.o kill.o \
	let.o read.o return.o set.o setattr.o shift.o source.o \
	suspend.o test.o times.o trap.o type.o ulimit.o umask.o \
	wait.o getopts.o getopt.o builtins.o

THINGS_TO_TAR = $(DEFS) $(STATIC_SOURCE) Makefile ChangeLog

CREATED_FILES = builtext.h builtins.c psize.aux pipesize.h

all: $(MKBUILTINS) libbuiltins.a

libbuiltins.a: $(MKBUILTINS) $(OFILES)
	$(RM) $@
	$(AR) clq $@ $(OFILES)
	-if [ -f "$(RANLIB)" ]; then $(RANLIB) $@; fi

builtext.h builtins.c: $(MKBUILTINS) $(DEFS)
	$(RM) builtext.h builtins.c
	./$(MKBUILTINS) -externfile builtext.h -structfile builtins.c \
	-noproduction $(DEFS)

mkbuiltins: mkbuiltins.c
	$(CC) $(CFLAGS) -o $(MKBUILTINS) mkbuiltins.c

ulimit.o: ulimit.def pipesize.h

pipesize.h:	psize.aux
	$(SHELL) ./psize.sh > pipesize.h

psize.aux:	psize.c
	$(CC) $(CFLAGS) -o $@ psize.c

documentation: builtins.texi

builtins.texi: $(MKBUILTINS)
	./$(MKBUILTINS) -documentonly $(DEFS)

clean:
	$(RM) $(OFILES) $(CREATED_FILES) $(MKBUILTINS) libbuiltins.a

