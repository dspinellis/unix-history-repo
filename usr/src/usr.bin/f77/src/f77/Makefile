CFLAGS	      = -DHERE=VAX -DTARGET=VAX -DFAMILY=PCC -DUCBVAXASM -DUCBPASS2 \
		-O -I../../include

DEST	      = $(DESTDIR)/usr/bin

EXTHDRS	      = ../../include/defines.h \
		../../include/machdefs.h \
		../../include/version.h \
		/usr/include/ctype.h \
		/usr/include/errno.h \
		/usr/include/signal.h \
		/usr/include/stdio.h \
		/usr/include/sys/stat.h \
		/usr/include/sys/types.h

HDRS	      = drivedefs.h

LDFLAGS	      = -n

LIBS	      =

LINKER	      = cc

MAKEFILE      = Makefile

OBJS	      = f77.o

PRINT	      = pr

PROGRAM	      = f77

SRCS	      = f77.c

all:		$(PROGRAM)

$(PROGRAM):     $(OBJS) $(LIBS)
		@echo -n "Loading $(PROGRAM) ... "
		@$(LINKER) $(LDFLAGS) $(OBJS) $(LIBS) -o $(PROGRAM)
		@echo "done"

clean:;		@rm -f $(OBJS)

depend:;	@mkmf -f $(MAKEFILE) PROGRAM=$(PROGRAM) DEST=$(DEST)

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
###
f77.o: /usr/include/stdio.h /usr/include/ctype.h ../../include/defines.h \
	../../include/machdefs.h drivedefs.h ../../include/version.h \
	/usr/include/signal.h /usr/include/errno.h /usr/include/sys/types.h \
	/usr/include/sys/stat.h
