# Descrip.MMS -- Makefile for building GNU Awk on VMS with VAXC and MMS.
#
# usage:
#  $ MMS /Description=[.vms]Descrip.MMS gawk
#
# gawk.exe :
#	You'll need to modify this Makefile to use gcc or vaxc v2.x rather
#	than vaxc v3.x.  Change the CFLAGS macro definition (move '#' from
#	beginning of 2nd alternative to beginning of 1st), and also perhaps
#	enable the following ".first" rule and its associated action.  For
#	GNU C, change the LIBS macro definition.
#
# awktab.c :
#	If you don't have bison but do have VMS POSIX or DEC/Shell,
#	change the PARSER and PASERINIT macros to use yacc.  If you don't
#	have either yacc or bison, you'll have to make sure that the
#	distributed version of "awktab.c" has its modification date later
#	than the date of "awk.y", so that MMS won't try to build that
#	target.  If you use bison and it is already defined system-wide,
#	comment out the PARSERINIT definition.
#
# install.help :
#	You can make the target 'install.help' to load the VMS help text
#	into a help library.  Modify the HELPLIB macro if you don't want
#	to put entry into the regular VMS library.  (If you use an alternate
#	help library, it must already exist; this target won't create it.)
#
# gawk.dvi :
#	If you have TeX, you can make the target 'gawk.dvi' to process
#	_The_GAWK_Manual_ from gawk.texi.  You'll need to use a device
#	specific post-processor on gawk.dvi in order to get printable data.
#

# location of the VMS-specific files, relative to the 'main' directory
VMSDIR	= [.vms]
MAKEFILE = $(VMSDIR)Descrip.MMS

# debugging &c		!'ccflags' is an escape to allow external compile flags
#CCFLAGS = /noOpt/Debug

# work within the main directory, even when handling files in [.vms]
#	note: use 2nd variant for either VAX C V2.x or for GNU C
CFLAGS	= /Include=[]/Object=[]/Opt=noInline/Define="GAWK" $(CCFLAGS)
#CFLAGS	= /Include=([],$(VMSDIR))/Object=[]/Define="GAWK" $(CCFLAGS)

# uncomment this for GNU C
#CC	= gcc
# beta VAX/VMS -> Alpha/VMS cross-compiler
#CC	= gemcc/Standard=VAXC/G_Float
# Alpha/VMS
#CC	= cc/Standard=VAXC/G_Float

# uncomment these two lines for GNU C _if_ it's not installed system-wide
#.first		!compiler init, needed if there's no system-wide setup
#	set command gnu_cc:[000000]gcc

# uncomment these three lines for VAX C V2.x
#.first		!compiler init, find all #include files
#	define/nolog vaxc$library sys$library:,sys$disk:$(VMSDIR)
#	define/nolog c$library [],$(VMSDIR)
#!(it appears that if vaxc$library is defined, then the /Include
#! qualifier is ignored, making a c$library definition essential)

# run-time libraries; use the 2nd one for GNU C
LIBS	= sys$share:vaxcrtl.exe/Shareable
#LIBS	= gnu_cc:[000000]gcclib.olb/Library,sys$library:vaxcrtl.olb/Library
#LIBS	=	# DECC$SHR instead of VAXCRTL; for Alpha/VMS (or VMS V6.x?)

PARSER	= bison
PARSERINIT = set command gnu_bison:[000000]bison
#PARSER	= yacc
#PARSERINIT = yacc := posix/run/path=posix """/bin/yacc"
#PARSERINIT = yacc := $shell$exe:yacc

# this is used for optional target 'install.help'
HELPLIB = sys$help:helplib.hlb
#HELPLIB = sys$help:local.hlb

#
########  nothing below this line should need to be changed  ########
#

# ALLOCA
ALLOCA	= alloca.obj

# object files
AWKOBJS = main.obj,eval.obj,builtin.obj,msg.obj,iop.obj,io.obj,\
	field.obj,array.obj,node.obj,version.obj,missing.obj,re.obj,getopt.obj

ALLOBJS = $(AWKOBJS),awktab.obj

# GNUOBJS
#	GNU stuff that gawk uses as library routines.
GNUOBJS = regex.obj,dfa.obj,$(ALLOCA)

# VMSOBJS
#	VMS specific stuff
VMSCODE = vms_misc.obj,vms_popen.obj,vms_fwrite.obj,vms_args.obj,\
	vms_gawk.obj,vms_cli.obj
VMSCMD	= gawk_cmd.obj			# built from .cld file
VMSOBJS = $(VMSCODE),$(VMSCMD)

VMSSRCS = $(VMSDIR)vms_misc.c,$(VMSDIR)vms_popen.c,$(VMSDIR)vms_fwrite.c,\
	$(VMSDIR)vms_args.c,$(VMSDIR)vms_gawk.c,$(VMSDIR)vms_cli.c
VMSHDRS = $(VMSDIR)vms.h,$(VMSDIR)fcntl.h,$(VMSDIR)varargs.h,$(VMSDIR)unixlib.h
VMSOTHR = $(VMSDIR)Descrip.MMS,$(VMSDIR)vmsbuild.com,$(VMSDIR)version.com,\
	$(VMSDIR)gawk.hlp

# Release of gawk
REL=2.14
PATCHLVL=0

# dummy target to allow building "gawk" in addition to explicit "gawk.exe"
gawk : gawk.exe
	write sys$output " GAWK "

# rules to build gawk
gawk.exe : $(ALLOBJS) $(GNUOBJS) $(VMSOBJS) gawk.opt
	$(LINK) $(LINKFLAGS) gawk.opt/options

gawk.opt : $(MAKEFILE)			# create linker options file
	open/write opt gawk.opt		! ~ 'cat <<close >gawk.opt'
	write opt "! GAWK -- Gnu AWK"
      @ write opt "$(ALLOBJS)"
      @ write opt "$(GNUOBJS)"
      @ write opt "$(VMSOBJS)"
      @ write opt "$(LIBS)"
      @ write opt "psect_attr=environ,noshr        !extern [noshare] char **"
      @ write opt "stack=48        !preallocate more pages (default is 20)"
      @ write opt "iosegment=128   !ditto (default is 32)"
	write opt "identification=""V$(REL).$(PATCHLVL)"""
	close opt

$(AWKOBJS)	: awk.h config.h
$(VMSCODE)	: awk.h config.h $(VMSDIR)vms.h
vms_misc.obj	: $(VMSDIR)vms_misc.c
vms_popen.obj	: $(VMSDIR)vms_popen.c
vms_fwrite.obj	: $(VMSDIR)vms_fwrite.c
vms_args.obj	: $(VMSDIR)vms_args.c
vms_gawk.obj	: $(VMSDIR)vms_gawk.c
vms_cli.obj	: $(VMSDIR)vms_cli.c
dfa.obj		: awk.h config.h dfa.h
regex.obj	: awk.h config.h regex.h
getopt.obj	: getopt.h
main.obj	: patchlevel.h
awktab.obj	: awk.h awktab.c

# bison or yacc required
awktab.c	: awk.y		# foo.y :: yacc => y[_]tab.c, bison => foo_tab.c
     @- if f$search("ytab.c")	.nes."" then  delete ytab.c;*	 !POSIX yacc
     @- if f$search("y_tab.c")	.nes."" then  delete y_tab.c;*	 !DEC/Shell yacc
     @- if f$search("awk_tab.c").nes."" then  delete awk_tab.c;* !bison
      - $(PARSERINIT)
	$(PARSER) $(YFLAGS) $<
     @- if f$search("ytab.c")	.nes."" then  rename/new_vers ytab.c  $@
     @- if f$search("y_tab.c")	.nes."" then  rename/new_vers y_tab.c $@
     @- if f$search("awk_tab.c").nes."" then  rename/new_vers awk_tab.c $@

config.h	: [.config]vms-conf.h
	copy $< $@

# Alloca - C simulation
alloca.obj	: alloca.c
	$(CC) $(CFLAGS) /define=("STACK_DIRECTION=(-1)","exit=vms_exit") $<

$(VMSCMD)	: $(VMSDIR)gawk.cld
	set command $(CLDFLAGS)/object=$@ $<

# special target for loading the help text into a VMS help library
install.help	: $(VMS)gawk.hlp
	library/help $(HELPLIB) $< /log

# miscellaneous other targets
tidy :
      - if f$search("*.*;-1").nes."" then  purge
      - if f$search("[.*]*.*;-1").nes."" then  purge [.*]

clean :
      - delete *.obj;*,gawk.opt;*

spotless : clean tidy
      - delete gawk.dvi;*,gawk.exe;*,[.support]texindex.exe;*

#
# build gawk.dvi from within the 'support' subdirectory
#
gawk.dvi : [.support]texindex.exe gawk.texi
      @ set default [.support]
      @ write sys$output " Warnings from TeX are expected during the first pass"
	TeX [-]gawk.texi
	mcr []texindex gawk.cp gawk.fn gawk.ky gawk.pg gawk.tp gawk.vr
      @ write sys$output " Second pass"
	TeX [-]gawk.texi
	mcr []texindex gawk.cp gawk.fn gawk.ky gawk.pg gawk.tp gawk.vr
      @ write sys$output " Third (final) pass"
	TeX [-]gawk.texi
     -@ purge
     -@ delete gawk.lis;,.aux;,gawk.%%;,.cps;,.fns;,.kys;,.pgs;,.toc;,.tps;,.vrs;
      @ rename/new_vers gawk.dvi [-]*.*
      @ set default [-]

[.support]texindex.exe : [.support]texindex.c
      @ set default [.support]
	$(CC) /noOpt/noList/Define=("lines=tlines") texindex.c
	$(LINK) /noMap texindex.obj,sys$library:vaxcrtl.olb/Lib
     -@ delete texindex.obj;*
      @ set default [-]

#eof
