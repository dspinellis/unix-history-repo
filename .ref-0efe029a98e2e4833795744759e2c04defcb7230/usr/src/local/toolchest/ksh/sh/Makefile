
#
#      Copyright (c) 1984, 1985, 1986 AT&T
#      All Rights Reserved
#
#      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
#      CODE OF AT&T.
#      The copyright notice above does not 
#      evidence any actual or intended
#      publication of such source code.
#
# @(#)Makefile	1.1
U370 =
TESTDIR = .
INS = /etc/install
INSDIR =
CFLAGS = -O 
RJE = $(HOME)/rje
CPIO = editlib.cpio
ARK = libedit.a
# The following two lines are used for one line emacs editor
EMACS = emacs.o
DESH = -DESH
# The following two lines are used for one line vi editor
VI = vi.o
DVSH = -DVSH
# The following line is used for vi mode always raw (u370 requires this)
# VIRAW = -DRAWONLY
VIRAW =
# The following two or three lines are used for BSD Unix
# DBSD = -BSD 
CC_CMD = $(CC) $(DVSH) $(DESH) $(DBSD) $(VIRAW) $(CFLAGS) -c

LIBFILES = editlib.c \
edit.h \
edit.c \
emacs.c \
history.h \
history.c \
vi.c \
syscall.s


EDITFILES = editlib.o \
edit.o \
history.o \
$(EMACS) \
$(VI)  \
$(SYSCALL)

all: $(ARK)
	if	test "$(DBSD)"; then ranlib $(ARK);fi


$(ARK):	clean $(EDITFILES)
		ar cuv $(TESTDIR)/$(ARK) $(EDITFILES)
		-/bin/rm $(EDITFILES)


emacs.o:	emacs.c
	set -ex; if test  "$(U370)" = "" ;\
	then \
	$(CC_CMD) emacs.c ;\
	else \
	$(CC_CMD) -b2,0 emacs.c ;\
	fi

install:  all
	  $(INS) -o $(TESTDIR)/$(ARK) $(INSDIR)

clean:
	  -rm -f $(EDITFILES)

clobber:  clean
	  -rm -f $(TESTDIR)/$(ARK)

cpio:	$(LIBFILES) Makefile makelib
	ls $(LIBFILES) Makefile makelib | cpio -ocBv > $(RJE)/$(CPIO)

tar:	$(LIBFILES) Makefile makelib
	tar -cvpf $(RJE)/$(TAR) $(LIBFILES) Makefile 

########
#
#	All dependencies and rules not explicitly stated
#	(including header and nested header dependencies)
#
########


edit.o:	edit.c
edit.o:	edit.h
edit.o:	history.h
	$(CC_CMD) edit.c

editlib.o:	edit.h
editlib.o:	editlib.c
	$(CC_CMD) editlib.c

emacs.o:	edit.h
emacs.o:	history.h

history.o:	history.c
history.o:	history.h
	$(CC_CMD) history.c

vi.o:	edit.h
vi.o:	history.h
vi.o:	vi.c
	$(CC_CMD) vi.c

syscall.o:	syscall.s
