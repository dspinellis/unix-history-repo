#
# Makefile for compiling Perl under OS/2
#
# Needs Microsoft C 6.00 and NMAKE
#

EXP =   c:\ms\lib\setargv.obj -link /noe

DEF =   os2\perl.def
BAD =   os2\perl.bad

OBJ =	array.obj cmd.obj cons.obj consarg.obj doarg.obj doio.obj \
	dolist.obj dump.obj eval.obj form.obj hash.obj perl.obj perly.obj \
	regcomp.obj regexec.obj stab.obj str.obj toke.obj util.obj
OBJO =	os2.obj popen.obj suffix.obj director.obj alarm.obj crypt.obj

LIBS =  lgdbm.lib

YACC=bison
YFLAGS=-d

CC=cl -nologo
CCL=cl -nologo -B2C2L -B3C3L

CFLAGS=-W1 -AL -Zep -J -G2s -Olt -Gt 2048 -DDEBUGGING
#CFLAGS=-W1 -AL -Ziep -J -G2 -Od -Gt 2048 -DDEBUGGING

LDFLAGS=-AL -Lp -F 8000
#LDFLAGS=-AL -Lp -Zi -Li -F 8000

STRIP=bind -nologo
#STRIP=rem

.c.obj:
        $(CC) -c $(CFLAGS) $<

{os2}.c{}.obj:
        $(CC) -c $(CFLAGS) -I. -Ios2 $<

all: perl.exe perlglob.exe

perl.exe: $(OBJ) $(OBJO)
        $(CC) $(LDFLAGS) $(OBJ) $(OBJO) $(LIBS) os2\perl.def -o $@ $(EXP)
        $(STRIP) $@ -n @$(BAD)

$(OBJ) $(OBJO): config.h
perl.obj str.obj cons.obj toke.obj: perly.h

config.h: os2\config.h
        cp os2\config.h config.h

perly.c perly.h: perly.y
        $(YACC) $(YFLAGS) -o $*.c $*.y

eval.obj:	eval.c
	$(CCL) -c $(CFLAGS) $*.c
toke.obj:	toke.c
	$(CCL) -c $(CFLAGS) $*.c

perlglob.exe: os2\glob.c os2\director.c
	$(CC) -Zep -G2s -Olt -Lp os2\glob.c $(DEF) -o $@ $(EXP)
        $(STRIP) $@ -n @$(BAD)

clean:
        -rm perly.c perly.h config.h *.obj >nul
