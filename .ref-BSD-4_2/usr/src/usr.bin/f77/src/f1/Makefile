CFLAGS	      = -O -DASSTRINGS -DSTABDOT -DLCOMM -DBUFSTDERR -DFLEXNAMES \
		-I$M -I.

DEST	      = $(DESTDIR)/lib

EXTHDRS	      = /usr/include/ctype.h

HDRS	      = fort.h

LDFLAGS	      =

LIBS	      =

LINKER	      = cc

M	      = ../mip

MAKEFILE      = Makefile

OBJS	      = fort.o \
		freader.o \
		fallo.o \
		fmatch.o \
		ftable.o \
		forder.o \
		flocal2.o \
		fcomm2.o

PRINT	      = pr

PROGRAM	      = f1

SRCS	      = local2.c \
		order.c \
		otable.c \
		table.c

all:		$(PROGRAM)

$(PROGRAM):     $(OBJS) $(LIBS)
		@echo -n "Loading $(PROGRAM) ... "
		@$(LINKER) $(LDFLAGS) $(OBJS) $(LIBS) -o $(PROGRAM)
		@echo "done"

clean:;		@rm -f $(OBJS)

depend:;	@echo Construct dependencies manually

index:;		@ctags -wx $(HDRS) $(SRCS)

install:	$(PROGRAM)
		@echo Installing $(PROGRAM) in $(DEST)
		@install -s $(PROGRAM) $(DEST)

print:;		@$(PRINT) $(HDRS) $(SRCS)

program:        $(PROGRAM)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

update:		$(DEST)/$(PROGRAM)

$(DEST)/$(PROGRAM): $(SRCS) $(LIBS) $(HDRS) $(EXTHDRS)
		@make -f $(MAKEFILE) DEST=$(DEST) install

fort.o: fort.h $M/fort.c
	$(CC) -c $(CFLAGS) $M/fort.c
freader.o:  $M/manifest $M/mfile2 mac2defs macdefs $M/reader.c $M/freader.c
	$(CC) -c $(CFLAGS) $M/freader.c
fallo.o:  $M/manifest $M/mfile2 mac2defs macdefs $M/allo.c $M/fallo.c
	$(CC) -c $(CFLAGS) $M/fallo.c
fmatch.o:  $M/manifest $M/mfile2 mac2defs macdefs $M/match.c $M/fmatch.c
	$(CC) -c $(CFLAGS) $M/fmatch.c
ftable.o:  $M/manifest $M/mfile2 mac2defs macdefs table.c $M/ftable.c
	$(CC) -c -R $(CFLAGS) $M/ftable.c
forder.o:  $M/manifest $M/mfile2 mac2defs macdefs order.c $M/forder.c
	$(CC) -c $(CFLAGS) $M/forder.c
flocal2.o:  $M/manifest $M/mfile2 mac2defs macdefs local2.c $M/flocal2.c
	$(CC) -c $(CFLAGS) $M/flocal2.c
fcomm2.o: $M/common
	$(CC) -c $(CFLAGS) $M/fcomm2.c
$(OBJS):  $M/mfile2 $M/manifest macdefs mac2defs
