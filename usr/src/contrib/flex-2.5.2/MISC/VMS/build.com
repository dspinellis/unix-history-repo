$! VMS build procedure for flex 2.5.x;
$ v = 'f$verify(0)' 
$!
$! usage:
$!  $ @[.MISC.VMS]BUILD.COM compiler parser [test]
$!	where `compiler' is either "GNUC" or "DECC" or "VAXC" or empty
$!	  and `parser' is either "BISON" or "BYACC" or "YACC" or empty
$!	  and `[test]' is either "CHECK-ONLY" or "NO-CHECK" or empty
$!	empty compiler defaults to VAX C (even under Alpha/VMS);
$!	special "LINK" compiler value does link without compilation;
$!	empty parser defaults to using supplied parse code in [.MISC];
$!	optional test is performed by default.
$!
$
$! we start from [.MISC.VMS], then move to the main source directory
$ where = f$parse("_._;",f$environ("PROCEDURE")) - "_._;"
$ set default 'where'
$ brkt = f$extract(f$length(where)-1,1,where)
$ if f$locate(".MISC.VMS"+brkt,where).lt.f$length(where) then -
	set default 'f$string(f$extract(0,1,f$dir()) + "-.-" + brkt)'
$
$ p1 := 'p1'
$ p2 := 'p2'
$ p3 := 'p3'
$ if p1.eqs."LINK" then goto link
$ if p3.eqs."CHECK-ONLY" then goto check
$ p2 = p2 - "_PARSER"
$!
$ CDEFS = "/Define=(""VMS"")"		! =(""VMS"",""DEFAULT_CSIZE=256"")
$!
$ if p1.eqs."GNUC"
$ then	CC	= "gcc"
$	CFLAGS	= "/noList/Opt=2/Debug/noVerbose"
$	LIBS	= "gnu_cc:[000000]gcclib.olb/Library, sys$library:vaxcrtl.olb/Library"
$ else	CC	= "cc"
$  if p1.eqs."DECC"
$  then CFLAGS	= "/noList/Prefix=All"
$	LIBS	= ""
$	if f$trnlnm("DECC$CC_DEFAULT").nes."" then CC = CC + "/DECC"
$  else CFLAGS	= "/noList/Optimize=noInline"
$	LIBS	= "sys$share:vaxcrtl.exe/Shareable"
$	if f$trnlnm("DECC$CC_DEFAULT").nes."" then CC = CC + "/VAXC"
$	if p1.nes."" .and. p1.nes."VAXC" then  exit %x002C
$  endif
$ endif
$!
$	no_parser = 0
$ if p2.eqs."BISON"
$ then	YACC	  = "bison"
$	YACCFLAGS = "/Defines/Fixed_Outfiles"
$	ALLOCA	  = ",[]alloca.obj"
$ else
$	YACCFLAGS = "-d"
$	ALLOCA	  = ""
$  if p2.eqs."BYACC" .or. p2.eqs."YACC"
$  then	YACC	  = f$edit(p2,"LOWERCASE")
$  else	YACC	  = "! yacc"
$	if p2.nes."" .and. p2.nes."NO" .and. p2.nes."NONE" then	exit %x002C
$	no_parser = 1
$  endif
$ endif
$!
$ ECHO	 = "write sys$output"
$ COPY	 = "copy_"
$ MOVE	 = "rename_/New_Vers"
$ MUNG	 = "search_/Exact/Match=NOR"
$ PURGE	 = "purge_/noConfirm/noLog"
$ REMOVE = "delete_/noConfirm/noLog"
$ TPU	 = "edit_/TPU/noJournal/noDisplay/noSection"
$!
$ if v then set verify
$!
$ 'COPY' [.misc.vms]vms-conf.h config.h
$ 'COPY' [.misc.vms]vms-code.c vms-code.c
$ 'COPY' [.misc]flex.man flex.doc
$ if ALLOCA.nes."" then 'COPY' [.MISC]alloca.c alloca.c
$ 'COPY' initscan.c scan.c	!make.bootstrap
$!
$ if f$search("skel.c").nes."" then -
     if f$cvtime(f$file_attr("skel.c","RDT")).gts. -
	f$cvtime(f$file_attr("flex.skl","RDT")) then goto skip_mkskel
$ 'TPU' /Command=[.misc.vms]mkskel.tpu flex.skl /Output=skel.c
$skip_mkskel:
$!
$ if f$search("parse.c").nes."" .and. f$search("parse.h").nes."" then -
     if f$cvtime(f$file_attr("parse.c","RDT")).gts. -
	f$cvtime(f$file_attr("parse.y","RDT")) then goto skip_yacc
$ if f$search("y_tab.%").nes."" then 'REMOVE' y_tab.%;*
$ if no_parser
$ then	'COPY' [.misc]parse.% sys$disk:[]y_tab.*
$ else	'YACC' 'YACCFLAGS' parse.y
$ endif
$ 'MUNG' y_tab.c "#module","#line" /Output=parse.c
$ 'REMOVE' y_tab.c;*
$ 'MOVE' y_tab.h parse.h
$skip_yacc:
$!
$ 'CC' 'CFLAGS' 'CDEFS' /Include=[] ccl.c
$ 'CC' 'CFLAGS' 'CDEFS' /Include=[] dfa.c
$ 'CC' 'CFLAGS' 'CDEFS' /Include=[] ecs.c
$ 'CC' 'CFLAGS' 'CDEFS' /Include=[] gen.c
$ 'CC' 'CFLAGS' 'CDEFS' /Include=[] main.c
$ 'CC' 'CFLAGS' 'CDEFS' /Include=[] misc.c
$ 'CC' 'CFLAGS' 'CDEFS' /Include=[] nfa.c
$ 'CC' 'CFLAGS' 'CDEFS' /Include=[] parse.c
$ 'CC' 'CFLAGS' 'CDEFS' /Include=[] scan.c
$ 'CC' 'CFLAGS' 'CDEFS' /Include=[] skel.c
$ 'CC' 'CFLAGS' 'CDEFS' /Include=[] sym.c
$ 'CC' 'CFLAGS' 'CDEFS' /Include=[] tblcmp.c
$ 'CC' 'CFLAGS' 'CDEFS' /Include=[] yylex.c
$ 'CC' 'CFLAGS' 'CDEFS' /Include=[] vms-code.c
$ if ALLOCA.nes."" then -	!bison
  'CC' 'CFLAGS' /Define=("STACK_DIRECTION=-1","xmalloc=yy_flex_xmalloc") alloca.c
$!
$ 'CC' 'CFLAGS' 'CDEFS' /Include=[] libmain.c
$ 'CC' 'CFLAGS' 'CDEFS' /Include=[] libyywrap.c
$ library/Obj flexlib.olb/Create libmain.obj,libyywrap.obj/Insert
$ if f$search("flexlib.olb;-1").nes."" then 'PURGE' flexlib.olb
$!
$ open/Write optfile sys$disk:[]crtl.opt
$ write optfile LIBS
$ close optfile
$ if f$search("crtl.opt;-1").nes."" then 'PURGE' crtl.opt
$!
$ version = "# flex ""2.5"""	!default, overridden by version.h
$ open/Read/Error=v_h_2 hfile version.h
$ read/End=v_h_1 hfile version
$v_h_1: close/noLog hfile
$v_h_2: version = f$element(1,"""",version)
$ open/Write optfile sys$disk:[]ident.opt
$ write optfile "identification=""flex ''version'"""
$ close optfile
$ if f$search("ident.opt;-1").nes."" then 'PURGE' ident.opt
$!
$link:
$ link/noMap/Exe=flex.exe ccl.obj,dfa.obj,ecs.obj,gen.obj,main.obj,misc.obj,-
	nfa.obj,parse.obj,scan.obj,skel.obj,sym.obj,tblcmp.obj,yylex.obj,-
	vms-code.obj 'ALLOCA' ,flexlib.olb/Lib,-
	sys$disk:[]crtl.opt/Opt,sys$disk:[]ident.opt/Opt
$!
$ if p3.eqs."NO-CHECK" .or. p3.eqs."NOCHECK" then goto done
$
$check:
$ 'ECHO' ""
$ 'ECHO' "  Checking with COMPRESSION="""""
$ mcr sys$disk:[]flex.exe -t -p  scan.l > scan.chk
$ diff_/Output=_NL:/Maximum_Diff=1 scan.c scan.chk
$ if $status
$ then	'ECHO' "  Test passed."
$	'REMOVE' scan.chk;*
$ else	'ECHO' "? Test failed!"
$ endif
$
$done:
$ exit
