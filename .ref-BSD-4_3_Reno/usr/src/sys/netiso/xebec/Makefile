BINDIR	= /usr/argo/bin
LIBDIR	= /usr/argo/lib
INCDIR	= /usr/argo/include
#
#
#	$Header: Makefile,v 2.1 88/09/19 12:54:18 nhall Exp $
#	$Source: /usr/argo/src/xebec/RCS/Makefile,v $
#
#
#
# Makefile for xebec
# This source desperately needs rewriting!
# It works, but it's grotesque.
# It uses fmq, the binary of which is included here.
# Source is not available.
# Strictly speaking, there's no reason for fmq to be needed any more.
#
#CFLAGS =
CFLAGS = -DDEBUG
CC = /bin/cc

.SUFFIXES:	.bnf .h .c .s .o
#
OBJ	=\
	llscan.o\
	putdriver.o\
	procs.o\
	sets.o\
	main.o\
	xebec.o\
	malloc.o\
	llparse.o

CSRC	=\
	putdriver.c\
	sets.c\
	procs.c\
	xebec.c\
	malloc.c\
	main.c\
	llscan.c\
	llparse.c

#	Default RCS arguments
#
RCSSOURCE = Makefile $(CSRC) debug.h llparse.h main.h malloc.h procs.h\
	sets.h states.h test_def.h xebec.h
RCSARGS = -f -r2.0 -nBETA

all: xebec
	@echo All versions up to date

lint:
	/usr/bin/lint $(CFLAGS) -DLINT $(CSRC) 

clean:
	rm -f junk core a.out *.o test_events.h test_driver.c test_states.h \
	test_states.init test_astring.c made xebec tags

reconfigure:
	 chmod +w Makefile
	ed Makefile < /tmp/edscript
	 chmod -w Makefile

.c.o:
	$(CC) -c $(CFLAGS) $*.c 
	ld -x -r -o tmp$$$$ $*.o; mv tmp$$$$ $*.o
	rm -f $*.s

xebec:	$(OBJ)
	$(CC) $(LFLAGS) $(CFLAGS) -o xebec $(OBJ) 
	chmod 755 xebec

install:
	install -c -s -g kmem xebec $(BINDIR)/xebec

#
ci:
	ci -q -m"$(RCSMSG)" $(RCSARGS) $(RCSSOURCE)
	-rcs -q -u $(RCSSOURCE)
	co -q $(RCSSOURCE)
#
#	Same thing but force checkin
#
#
#	Check out source (locked)
#
co:
	co -l -q $(RCSSOURCE)
#
#	Lock source
#
lock:
	rcs -l -q $(RCSSOURCE)

llparse.o:xebec.o
llscan.o:xebec.o

depend:
	@grep '^#[	 ]*include' $(DSRC) \
		| sed 's/:[^"]*"\([^"]*\).*/: \1/' \
		| sed 's/\.[cmyl]:/.o:/' > Makedep
	@echo '/^# DO NOT DELETE THIS LINE/+2,$$d' > eddep
	@echo '$$r Makedep' >> eddep
	@echo 'w' >> eddep
	@echo 'q' >> eddep
	@cp Makefile Makefile.bak
	@ed - Makefile < eddep
	@rm eddep Makedep
	@echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	@echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	@echo '# see Make depend above ' >> Makefile

# DO NOT DELETE THIS LINE -- Make depend uses it
# DEPENDENCIES MUST END AT END OF FILE
