#	Makefile	1.10	86/01/12
#
DEST	      = /usr/new

CFLAGS	      = -O -DVAX ${DFLAG} $(HELPFILE)

EXTHDRS	      = /usr/include/ctype.h \
		/usr/include/stdio.h

HDRS	      = command.h \
		cpmfio.h \
		cpmio.h

HELPFILE      = -DHELPFILE=\"/usr/new/lib/cpm.hlp\"

LDFLAGS	      = 

LIBLIST       =

LIST	      = ls -C

LOADER	      = cc

MAKEFILE      = Makefile

OBJS	      = bitmap.o \
		blockio.o \
		cclose.o \
		ccreat.o \
		cfillbuf.o \
		cflsbuf.o \
		cmdhdl.o \
		copen.o \
		copy.o \
		cpm.o \
		delete.o \
		dirhdl.o \
		extent.o \
		ffc.o \
		gensktab.o \
		hexdmp.o \
		interact.o \
		physio.o \
		pip.o \
		rename.o

PRINT	      = pr

PROGRAM	      = cpm

SRCS	      = bitmap.c \
		blockio.c \
		cclose.c \
		ccreat.c \
		cfillbuf.c \
		cflsbuf.c \
		cmdhdl.c \
		copen.c \
		copy.c \
		cpm.c \
		delete.c \
		dirhdl.c \
		extent.c \
		ffc.c \
		gensktab.c \
		hexdmp.c \
		interact.c \
		physio.c \
		pip.c \
		rename.c

$(PROGRAM):     $(OBJS) $(LIBLIST)
		@echo -n "Loading $(PROGRAM) ... "
		@$(LOADER) $(LDFLAGS) $(OBJS) $(LIBLIST) -o $(PROGRAM)
		@echo "done"

debug:;		@echo DFLAG = -g -DDEBUG > Makefile.tmp00
		@cat makefile >>Makefile.tmp00
		@make -f Makefile.tmp00
		@rm -f Makefile.tmp00

clean:;		@rm -f $(OBJS)

install:	$(PROGRAM)
		@echo Installing $(PROGRAM) in $(DEST)
		@install -s $(PROGRAM) $(DESTDIR)$(DEST)
		@cp cpm.hlp ${DESTDIR}$(DEST)/lib

depend:;	@mkmf PROGRAM=$(PROGRAM) DEST=$(DEST)

index:;		@ctags -wx $(HDRS) $(SRCS)

print:;		@$(PRINT) $(HDRS) $(SRCS)

program:        $(PROGRAM)

strip:;		@strip $(PROGRAM)

tags:           $(SRCS); @ctags $(SRCS)

update:		$(DEST)/$(PROGRAM)

$(DEST)/$(PROGRAM): $(SRCS) $(LIBLIST)
		@make -f $(MAKEFILE) DEST=$(DEST) install
###
bitmap.o: /usr/include/stdio.h cpmio.h
blockio.o: /usr/include/stdio.h cpmio.h
cclose.o: /usr/include/stdio.h cpmio.h cpmfio.h
ccreat.o: /usr/include/stdio.h cpmio.h cpmfio.h
cfillbuf.o: /usr/include/stdio.h cpmio.h cpmfio.h
cflsbuf.o: /usr/include/stdio.h cpmio.h cpmfio.h
cmdhdl.o: /usr/include/ctype.h /usr/include/stdio.h cpmio.h
copen.o: /usr/include/stdio.h cpmio.h cpmfio.h
copy.o: /usr/include/stdio.h cpmio.h cpmfio.h
cpm.o: /usr/include/stdio.h cpmio.h cpmfio.h
delete.o: /usr/include/stdio.h cpmio.h cpmfio.h
dirhdl.o: /usr/include/stdio.h cpmio.h
extent.o: /usr/include/stdio.h cpmio.h cpmfio.h
gensktab.o: /usr/include/stdio.h cpmio.h
hexdmp.o: /usr/include/stdio.h cpmio.h cpmfio.h
interact.o: /usr/include/stdio.h command.h
physio.o: /usr/include/stdio.h cpmio.h
pip.o: /usr/include/stdio.h cpmio.h cpmfio.h
rename.o: /usr/include/stdio.h cpmio.h cpmfio.h
