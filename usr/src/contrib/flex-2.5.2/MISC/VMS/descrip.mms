# descrip.mms -- makefile for building `flex' using MMS or MMK on VMS;
#	created manually from Makefile.in
#						flex 2.5.0	Jan'95

MAKEFILE  = descrip.mms		    # from [.MISC.VMS]
MAKE	  = $(MMS) /Descr=$(MAKEFILE)
MAKEFLAGS = $(MMSQUALIFIERS)

# Possible values for DEFS:
# "VMS" -- used just to make sure parentheses aren't empty;
# For flex to always generate 8-bit scanners, append
# ,"DEFAULT_CSIZE=256" inside /Define=() of DEFS.

DEFS	  = /Define=("VMS")
LDFLAGS	  = /noMap

# compiler handling
.ifdef GNUC
CC	  = gcc
GCCINIT	  = 	! SET COMMAND GNU_CC:[000000]GCC
CFLAGS	  = /noList/Opt=2/Debug/noVerbose
LIBS	  = gnu_cc:[000000]gcclib.olb/Library, sys$library:vaxcrtl.olb/Library
C_CHOICE  = "GNUC=1"
.else		! not GNU C
CC	  = cc
GCCINIT	  =
.ifdef DECC
CFLAGS	  = /noList/Prefix=All
LIBS	  =
C_CHOICE  = "DECC=1"
.else		! not DEC C; assume VAX C
CFLAGS	  = /noList/Optimize=noInline
LIBS	  = sys$share:vaxcrtl.exe/Shareable
C_CHOICE  = "VAXC=1"
.endif
.endif

# parser handling
#	mms/macro=("xxxC=1","zzz_parser=1"), where "zzz_parser" is
#	either "bison_parser" or "byacc_parser" or "yacc_parser",
#	otherwise assumed to be "no_parser"; and where "xxxC=1" is
#	either "VAXC=1", "GNUC=1", or "DECC=1" as above
.ifdef bison_parser
YACC	  = bison
YACCFLAGS = /Defines/Fixed_Outfiles
YACCINIT  = set command gnu_bison:[000000]bison
ALLOCA	  = ,[]alloca.obj		# note leading comma
.else
YACCFLAGS = -d
YACCINIT  =
ALLOCA	  =
.ifdef byacc_parser
YACC	  = byacc
.else
.ifdef yacc_parser
YACC	  = yacc
.else
#	none of bison, byacc, or yacc specified
.ifdef no_parser
.else
no_parser=1
.endif	#<none>
.endif	#yacc
.endif	#byacc
.endif	#bison

# VMS-specific hackery
ECHO	  = write sys$output		# requires single quoted arg
COPY	  = copy_			#
MOVE	  = rename_/New_Vers		# within same device only
MUNG	  = search_/Exact/Match=NOR	# to strip unwanted `#module' directive
NOOP	  = continue			# non-empty command that does nothing
PURGE	  = purge_/noConfirm/noLog	# relatively quiet file removal
REMOVE	  = delete_/noConfirm/noLog	# ditto
TOUCH	  = append_/New _NL:		# requires single file arg
TPU	  = edit_/TPU/noJournal/noDisplay/noSection

# You can define this to be "lex.exe" if you want to replace lex at your site.
FLEX	=flex.exe
#	note: there should be no whitespace between `=' and the name,
#	or else $(FLEX_EXEC) below will not function properly.
FLEXLIB	  = flexlib.olb

# You normally do not need to modify anything below this point.
# ------------------------------------------------------------

VMSDIR	  = [.MISC.VMS]
MISCDIR	  = [.MISC]
CURDIR	  = sys$disk:[]

CPPFLAGS  = $(DEFS)/Include=[]
LIBOPT	  = $(CURDIR)crtl.opt		# run-time library(s)
ID_OPT	  = $(CURDIR)ident.opt		# version identification

.SUFFIXES :	# avoid overhead of umpteen built-in rules
.SUFFIXES : .obj .c

.c.obj :
	$(CC)$(CFLAGS)$(CPPFLAGS) $<

VMSHDRS = $(VMSDIR)vms-conf.h	    # copied to []config.h
VMSSRCS = $(VMSDIR)vms-code.c	    # copied to []vms-code.c
VMSOBJS = ,vms-code.obj		    # note leading comma

HEADERS = flexdef.h version.h

SOURCES = ccl.c dfa.c ecs.c gen.c main.c misc.c nfa.c parse.y \
	scan.l skel.c sym.c tblcmp.c yylex.c
OBJECTS = ccl.obj,dfa.obj,ecs.obj,gen.obj,main.obj,misc.obj,nfa.obj,parse.obj,\
	scan.obj,skel.obj,sym.obj,tblcmp.obj,yylex.obj $(VMSOBJS) $(ALLOCA)

LIBSRCS = libmain.c libyywrap.c
LIBOBJS = libmain.obj,libyywrap.obj

LINTSRCS = ccl.c dfa.c ecs.c gen.c main.c misc.c nfa.c parse.c \
	scan.c skel.c sym.c tblcmp.c yylex.c

DISTFILES = README NEWS COPYING INSTALL FlexLexer.h \
	configure.in conf.in Makefile.in mkskel.sh flex.skl \
	$(HEADERS) $(SOURCES) $(LIBSRCS) MISC \
	flex.1 scan.c install.sh mkinstalldirs configure

DIST_NAME = flex

# flex options to use when generating scan.c from scan.l
COMPRESSION =
PERF_REPORT = -p
# which "flex" to use to generate scan.c from scan.l
FLEX_EXEC   = mcr $(CURDIR)$(FLEX)
FLEX_FLAGS  = -t $(PERF_REPORT) #$(COMPRESSION)

MARKER	= make.bootstrap

##### targets start here #####

all : $(FLEX) flex.doc
	@ $(NOOP)

install : $(FLEX) flex.doc flex.skl $(FLEXLIB) FlexLexer.h
	@ $(ECHO) "-- Installation must be done manually."
	@ $(ECHO) "   $+"

.ifdef GCCINIT
.FIRST
	$(GCCINIT)

.endif	#GCCINIT

flex : $(FLEX)
	@ $(NOOP)

$(FLEX) : $(MARKER) $(OBJECTS) $(FLEXLIB) $(LIBOPT) $(ID_OPT)
	$(LINK)/Exe=$(FLEX) $(LDFLAGS)\
 $(OBJECTS),$(FLEXLIB)/Lib,$(LIBOPT)/Opt,$(ID_OPT)/Opt

$(MARKER) : initscan.c
	@- if f$search("scan.c").nes."" then $(REMOVE) scan.c;*
	$(COPY) initscan.c scan.c
	@ $(TOUCH) $(MARKER)

parse.c : parse.y
	@- if f$search("y_tab.%").nes."" then $(REMOVE) y_tab.%;*
.ifdef no_parser
	$(COPY) $(MISCDIR)parse.% $(CURDIR)y_tab.*
.else
	$(YACCINIT)
	$(YACC) $(YACCFLAGS) parse.y
.endif
	$(MUNG) y_tab.c "#module","#line" /Output=parse.c
	@- $(REMOVE) y_tab.c;*
	$(MOVE) y_tab.h parse.h

parse.h : parse.c
	@ $(TOUCH) parse.h

scan.c : scan.l
	$(FLEX_EXEC) $(FLEX_FLAGS) $(COMPRESSION) scan.l > scan.c

scan.obj : scan.c parse.h flexdef.h config.h
yylex.obj : yylex.c parse.h flexdef.h config.h

skel.c : flex.skl $(VMSDIR)mkskel.tpu
	$(TPU) /Command=$(VMSDIR)mkskel.tpu flex.skl /Output=skel.c

main.obj : main.c flexdef.h config.h version.h
ccl.obj : ccl.c flexdef.h config.h
dfa.obj : dfa.c flexdef.h config.h
ecs.obj : ecs.c flexdef.h config.h
gen.obj : gen.c flexdef.h config.h
misc.obj : misc.c flexdef.h config.h
nfa.obj : nfa.c flexdef.h config.h
parse.obj : parse.c flexdef.h config.h
skel.obj : skel.c flexdef.h config.h
sym.obj : sym.c flexdef.h config.h
tblcmp.obj : tblcmp.c flexdef.h config.h
vms-code.obj : vms-code.c flexdef.h config.h

[]alloca.obj : alloca.c
	$(CC)$(CFLAGS)/Define=("STACK_DIRECTION=-1","xmalloc=yy_flex_xmalloc") alloca.c

alloca.c : $(MISCDIR)alloca.c
	$(COPY) $(MISCDIR)alloca.c alloca.c

config.h : $(VMSDIR)vms-conf.h
	$(COPY) $(VMSDIR)vms-conf.h config.h

vms-code.c : $(VMSDIR)vms-code.c
	$(COPY) $(VMSDIR)vms-code.c vms-code.c

test : check
	@ $(NOOP)
check : $(FLEX)
	@ $(ECHO) ""
	@ $(ECHO) "  Checking with COMPRESSION="$(COMPRESSION)""
	$(FLEX_EXEC) $(FLEX_FLAGS) $(COMPRESSION) scan.l > scan.chk
	diff_/Output=_NL:/Maximum_Diff=1 scan.c scan.chk

bigcheck :
	@- if f$search("scan.c").nes."" then $(REMOVE) scan.c;*
	$(MAKE)$(MAKEFLAGS) /Macro=($(C_CHOICE),"COMPRESSION=""-C""") check
	@- $(REMOVE) scan.c;*
	$(MAKE)$(MAKEFLAGS) /Macro=($(C_CHOICE),"COMPRESSION=""-Ce""") check
	@- $(REMOVE) scan.c;*
	$(MAKE)$(MAKEFLAGS) /Macro=($(C_CHOICE),"COMPRESSION=""-Cm""") check
	@- $(REMOVE) scan.c;*
	$(MAKE)$(MAKEFLAGS) /Macro=($(C_CHOICE),"COMPRESSION=""-f""") check
	@- $(REMOVE) scan.c;*
	$(MAKE)$(MAKEFLAGS) /Macro=($(C_CHOICE),"COMPRESSION=""-Cfea""") check
	@- $(REMOVE) scan.c;*
	$(MAKE)$(MAKEFLAGS) /Macro=($(C_CHOICE),"COMPRESSION=""-CFer""") check
	@- $(REMOVE) scan.c;*
	$(MAKE)$(MAKEFLAGS) /Macro=($(C_CHOICE),"COMPRESSION=""-l""","PERF_REPORT=") check
	@- $(REMOVE) scan.c;*,scan.chk;*
	$(MAKE)$(MAKEFLAGS) $(FLEX)
	@- $(PURGE) scan.obj
	@ $(ECHO) "All checks successful"

$(FLEXLIB) : $(LIBOBJS)
	library/Obj $(FLEXLIB)/Create $(LIBOBJS)/Insert
	@ if f$search("$(FLEXLIB);-1").nes."" then $(PURGE) $(FLEXLIB)

# We call it .doc instead of .man, to lessen culture shock.  :-}
#	If MISC/flex.man is out of date relative to flex.1, there's
#	not much we can do about it with the tools readily available.
flex.doc : flex.1
	@ if f$search("$(MISCDIR)flex.man").eqs."" then \
		$(COPY) flex.1 $(MISCDIR)flex.man
	$(COPY) $(MISCDIR)flex.man flex.doc

#
#	This is completely VMS-specific...
#

# Linker options file specifying run-time library(s) to link against;
# choice depends on which C compiler is used, and might be empty.
$(LIBOPT) : $(MAKEFILE)
	@ open/Write optfile $(LIBOPT)
	@ write optfile "$(LIBS)"
	@ close optfile

# Linker options file putting the version number where the ANALYZE/IMAGE
# command will be able to find and report it; assumes that the first line
# of version.h has the version number enclosed within the first and second
# double quotes on it [as in ``#define FLEX_VERSION "2.5.0"''].
$(ID_OPT) : version.h
	@ version = "# flex ""2.5"""	!default, overridden by version.h
	@- open/Read hfile version.h
	@- read hfile version
	@- close/noLog hfile
	@ version = f$element(1,"""",version)
	@ open/Write optfile $(ID_OPT)
	@ write optfile "identification=""flex ''version'"""
	@ close optfile


#
#	This is the only stuff moderately useful from the remainder
#	of Makefile.in...
#

mostlyclean :
	@- if f$search("scan.chk").nes."" then $(REMOVE) scan.chk;*
	@- if f$search("*.obj;-1").nes."" then $(PURGE) *.obj
	@- if f$search("*.exe;-1").nes."" then $(PURGE) *.exe
	@- if f$search("*.opt;-1").nes."" then $(PURGE) *.opt

clean : mostlyclean
	@- if f$search("*.obj").nes."" then $(REMOVE) *.obj;*
	@- if f$search("parse.h").nes."" then $(REMOVE) parse.h;*
	@- if f$search("parse.c").nes."" then $(REMOVE) parse.c;*
	@- if f$search("alloca.c").nes."" .and.-
	 f$search("$(MISCDIR)alloca.c").nes."" then $(REMOVE) alloca.c;*
	@- if f$search("$(LIBOPT)").nes."" then $(REMOVE) $(LIBOPT);*
	@- if f$search("$(ID_OPT)").nes."" then $(REMOVE) $(ID_OPT);*

distclean : clean
	@- if f$search("$(MARKER)").nes."" then $(REMOVE) $(MARKER);*
	@- if f$search("$(FLEX)").nes."" then $(REMOVE) $(FLEX);*
	@- if f$search("$(FLEXLIB)").nes."" then $(REMOVE) $(FLEXLIB);*
	@- if f$search("flex.doc").nes."" then $(REMOVE) flex.doc;*
	@- if f$search("scan.c").nes."" then $(REMOVE) scan.c;*
	@- if f$search("vms-code.c").nes."" .and.-
	 f$search("$(VMSDIR)vms-code.c").nes."" then $(REMOVE) vms-code.c;*
	@- if f$search("config.h").nes."" .and.-
	 f$search("$(VMSDIR)vms-conf.h").nes."" then $(REMOVE) config.h;*
#	@- if f$search("descrip.mms").nes."" .and.-
#	 f$search("$(VMSDIR)descrip.mms").nes."" then $(REMOVE) descrip.mms;*

realclean : distclean
	@- if f$search("skel.c").nes."" then $(REMOVE) skel.c;*

