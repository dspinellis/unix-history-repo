$ SRC:=	ex,ex_addr,ex_cmds,ex_cmds2,ex_cmdsub,-
	ex_data,ex_extern,ex_get,ex_io,ex_put,ex_re,-
	ex_set,ex_subr,ex_tagio,ex_temp,ex_tty,ex_unix,-
	ex_v,ex_vadj,ex_vget,ex_vmain,ex_voper,-
	ex_vops,ex_vops2,ex_vops3,ex_vput,ex_vwind,-
	printf,termlib,tgoto,tputs,vms
$ DEF:=	(UCVISUAL,VMUNIX,TABS=8,VFORK,LISPCODE,CHDIR,UNIX_SBRK)
$
$ if ("''p1'" .nes. "ALL") then goto else
$	source := 'SRC'
$	set noon
$	goto endif
$
$ else:
$ if ("''p1'" .eqs. "LINK") then goto dolink
$	source := "''p1'"
$
$ endif:
$	on warning then exit
$	cc/def='DEF' 'source'
$
$ dolink:
$	link 'SRC',ex/option
$
$ set on
$ exit
