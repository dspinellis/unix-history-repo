# Things you might want to put in ENV and LENV:
# -Dvoid=int		compilers that don't do void
# -DCHARBITS=0377	compilers that don't do unsigned char
# -DSTATIC=extern	compilers that don't like "static foo();" as forward decl
# -DSTRCSPN		library does not have strcspn()
# -Dstrchr=index	library does not have strchr()
# -DERRAVAIL		have utzoo-compatible error() function and friends
ENV= 
LENV=

# Things you might want to put in TEST:
# -DDEBUG		debugging hooks
# -I.			regexp.h from current directory, not /usr/include
TEST= -I.

# Things you might want to put in PROF:
# -Dstatic='/* */'	make everything global so profiler can see it.
# -p			profiler
PROF=

CFLAGS=-O $(ENV) $(TEST) $(PROF)
LINTFLAGS=$(LENV) $(TEST) -ha
#LDFLAGS=-i		uncomment for pdp 11

OBJ=regexp.o regsub.o
LSRC=regexp.c regsub.c regerror.c
DTR=README.regexp dMakefile regexp.3 regexp.h regexp.c regsub.c regerror.c \
	regmagic.h try.c timer.c tests

try:	try.o $(OBJ)
	cc $(LDFLAGS) try.o $(OBJ) -o try

# Making timer will probably require putting stuff in $(PROF) and then
# recompiling everything; the following is just the final stage.
timer:	timer.o $(OBJ)
	cc $(LDFLAGS) $(PROF) timer.o $(OBJ) -o timer

timer.o:	timer.c timer.t.h

timer.t.h:	tests
	sed 's/	/","/g;s/\\/&&/g;s/.*/{"&"},/' tests >timer.t.h

# Regression test.
r:	try tests
	@echo 'No news is good news...'
	try <tests

lint:	timer.t.h
	@echo 'Complaints about multiply-declared regerror() are legit.'
	lint $(LINTFLAGS) $(LSRC) try.c
	lint $(LINTFLAGS) $(LSRC) timer.c

regexp.o:	regexp.c regexp.h regmagic.h
regsub.o:	regsub.c regexp.h regmagic.h

clean:
	rm -f *.o core mon.out timer.t.h dMakefile dtr try timer

dtr:	r makedtr $(DTR)
	makedtr $(DTR) >dtr

dMakefile:	Makefile
	sed '/^L*ENV=/s/ *-DERRAVAIL//' Makefile >dMakefile
