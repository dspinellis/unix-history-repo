$! vmsbuild.com -- Commands to build GAWK		Pat Rankin, Dec'89
$!							   revised, Mar'90
$!						gawk 2.13  revised, Jun'91
$!						gawk 2.14  revised, Sep'92
$!
$ REL = "2.14"	!release version number
$ PATCHLVL = "0"
$!
$!	[ remove "/optimize=noinline" for VAX C V2.x or DEC C ]
$!	[ add "/standard=VAXC" for DEC C and "/g_float" for Alpha ]
$ if f$type(cc)  .nes."STRING" then  cc   := cc/nolist/optimize=noinline
$ if f$type(link).nes."STRING" then  link := link/nomap
$ if f$type(set_command).nes."STRING" then  set_command := set command
$!
$ cc := 'cc'/Include=[]/Define="""GAWK"""
$ libs = "sys$share:vaxcrtl.exe/Shareable"
$
$! uncomment for DEC C
$ ! libs = ""
$
$! uncomment the next two lines for VAX C V2.x
$ ! define vaxc$library sys$library:,sys$disk:[.vms]
$ ! define c$library [],[.vms]
$!
$! uncomment next two lines for GNU C
$ ! cc := gcc/Include=([],[.vms])/Define="""GAWK"""	!use GNU C rather than VAX C
$ ! libs = "gnu_cc:[000000]gcclib.olb/Library,sys$library:vaxcrtl.olb/Library"
$!
$ if f$search("config.h").eqs."" then  copy [.config]vms-conf.h []config.h
$ if f$search("awktab.c").nes."" then  goto awktab_ok
$	write sys$output " You must process `awk.y' with ""yacc"" or ""bison"""
$	if f$search("awk_tab.c").nes."" then -	!bison was run manually
	  write sys$output " or else rename `awk_tab.c' to `awktab.c'."
$	if f$search("ytab.c").nes."" .or. f$search("y_tab.c").nes."" then - !yacc
	  write sys$output " or else rename `ytab.c' or `y_tab.c' to `awktab.c'."
$	exit
$awktab_ok:
$ cc main.c
$ cc eval.c
$ cc builtin.c
$ cc msg.c
$ cc iop.c
$ cc io.c
$ cc field.c
$ cc array.c
$ cc node.c
$ cc version.c
$ cc missing.c
$ cc re.c
$ cc getopt.c
$ cc awktab.c
$ cc regex.c
$ cc dfa.c
$ cc/define=("STACK_DIRECTION=(-1)","exit=vms_exit") alloca
$ cc [.vms]vms_misc.c
$ cc [.vms]vms_popen.c
$ cc [.vms]vms_fwrite.c
$ cc [.vms]vms_args.c
$ cc [.vms]vms_gawk.c
$ cc [.vms]vms_cli.c
$ set_command/object=[]gawk_cmd.obj [.vms]gawk.cld
$!
$ create gawk.opt
! GAWK -- Gnu AWK
main.obj,eval.obj,builtin.obj,msg.obj,iop.obj,io.obj
field.obj,array.obj,node.obj,version.obj,missing.obj
re.obj,getopt.obj,awktab.obj,regex.obj,dfa.obj,[]alloca.obj
[]vms_misc.obj,vms_popen.obj,vms_fwrite.obj
[]vms_args.obj,vms_gawk.obj,vms_cli.obj,gawk_cmd.obj
psect_attr=environ,noshr	!extern [noshare] char **
stack=48	!preallocate more pages (default is 20)
iosegment=128	!ditto (default is 32)
$ open/append Fopt gawk.opt
$ write Fopt libs
$ write Fopt "identification=""V''REL'.''PATCHLVL'"""
$ close Fopt
$!
$ link/exe=gawk.exe gawk.opt/options
