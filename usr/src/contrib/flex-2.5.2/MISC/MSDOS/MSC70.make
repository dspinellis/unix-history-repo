#
# make file for "flex" tool
# @(#) $Header: /usr/fsys/odin/a/vern/flex/RCS/Makefile,v 2.9 90/05/26 17:28:44
 vern Exp $ (LBL)
#
# the first time around use "make f_flex"
#
#  This makefile is specific for Microsoft's C/C++ compiler  (v7), nmake and
#  lib      
#         - Paul Stuart, Jan 93 (pjs@scammell.ecos.tne.oz.au)
#


SKELFLAGS = -DDEFAULT_SKELETON_FILE=\"c:/src/flex/flex.skl\"
CFLAGS = -nologo -AL -W2 -F 8000 -Ox -Gt16000 -DMS_DOS -DUSG
LDFLAGS = /nologo /NOI /BATCH /ONERROR:NOEXE  /STACK:8000
FLEX_FLAGS = -ist8 -Sflex.skl

FLEX = .\flex.exe
CC = cl
YACC = c:\lib\byacc
MAKE = nmake /nologo

#
# break obj-list into two because of 128 character command-line limit of
# Microsoft's link and lib utilities.
#
FLEXOBJS1 = \
	ccl.obj \
	dfa.obj \
	ecs.obj \
	gen.obj \
	main.obj \
	misc.obj \
	nfa.obj \
	parse.obj

FLEXOBJS2 = \
	scan.obj \
	sym.obj \
	tblcmp.obj \
	yylex.obj

FLEX_C_SOURCES = \
	ccl.c \
	dfa.c \
	ecs.c \
	gen.c \
	main.c \
	misc.c \
	nfa.c \
	parse.c \
	scan.c \
	sym.c \
	tblcmp.c \
	yylex.c

FLEX_LIB_OBJS = \
	libmain.obj


all : flex.exe 

#
# lib is used to get around the 128 character command-line limit of 'link'.
#
flex.exe : $(FLEXOBJS1) $(FLEXOBJS2)
	lib /nologo tmplib $(FLEXOBJS1);
	link $(LDFLAGS) $(FLEXOBJS2),$*.exe,,tmplib;
	del tmplib.lib

f_flex:
	copy initscan.c scan.c
	touch scan.c
	@echo  compiling first flex  
	$(MAKE) flex.exe 
	del scan.c
	@echo using first flex to generate final version...
	$(MAKE) flex.exe

#
# general inference rule
#
.c.obj:
	$(CC) -c $(CFLAGS) $*.c

parse.h parse.c : parse.y
	$(YACC) -d parse.y
	@mv y_tab.c parse.c
	@mv y_tab.h parse.h

scan.c : scan.l
	$(FLEX) $(FLEX_FLAGS) $(COMPRESSION) scan.l >scan.c


scan.obj : scan.c parse.h flexdef.h

main.obj : main.c flexdef.h
	$(CC) $(CFLAGS) -c $(SKELFLAGS) main.c

ccl.obj : ccl.c flexdef.h
dfa.obj : dfa.c flexdef.h
ecs.obj : ecs.c flexdef.h
gen.obj : gen.c flexdef.h
misc.obj : misc.c flexdef.h
nfa.obj : nfa.c flexdef.h
parse.obj : parse.c flexdef.h
sym.obj : sym.c flexdef.h
tblcmp.obj : tblcmp.c flexdef.h
yylex.obj : yylex.c flexdef.h


clean :
	del *.obj
	del *.map
