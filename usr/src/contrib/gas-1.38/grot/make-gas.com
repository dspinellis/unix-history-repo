$! Set the def dir to proper place for use in batch. Works for interactive to.
$flnm = f$enviroment("PROCEDURE")     ! get current procedure name
$set default 'f$parse(flnm,,,"DEVICE")''f$parse(flnm,,,"DIRECTORY")'
$!
$!	Command file to build a GNU assembler on VMS
$!
$!	If you are using a version of GCC that supports global constants
$!	you should remove the define="const=" from the gcc lines.
$ if "''p1'" .eqs. "LINK" then goto Link
$ gcc/debug/define=("VMS","const=") as.c
$ gcc/debug/define=("VMS", "error=as_fatal","const=") xrealloc.c
$ gcc/debug/define=("VMS", "error=as_fatal","const=") xmalloc.c
$ gcc/debug/define=("VMS", "error=as_fatal","const=") hash.c
$ gcc/debug/define=("VMS","const=") obstack.c
$ gcc/debug/define=("VMS","const=") hex-value.c
$ gcc/debug/define=("VMS","const=") atof-generic.c
$ gcc/debug/define=("VMS","const=") append.c
$ gcc/debug/define=("VMS","const=") messages.c
$ gcc/debug/define=("VMS","const=") expr.c
$ gcc/debug/define=("VMS","const=") app.c
$ gcc/debug/define=("VMS","const=") frags.c
$ gcc/debug/define=("VMS","const=") input-file.c
$ gcc/debug/define=("VMS","const=") input-scrub.c
$ gcc/debug/define=("VMS","const=") output-file.c
$ gcc/debug/define=("VMS","const=") read.c
$ gcc/debug/define=("VMS","const=") subsegs.c
$ gcc/debug/define=("VMS","const=") symbols.c
$ gcc/debug/define=("VMS","const=") write.c
$ gcc/debug/define=("VMS","const=") version.c
$ gcc/debug/define=("VMS","const=") flonum-const.c
$ gcc/debug/define=("VMS","const=") flonum-copy.c
$ gcc/debug/define=("VMS","const=") flonum-mult.c
$ gcc/debug/define=("VMS","const=") strstr.c
$ gcc/debug/define=("VMS","const=") bignum-copy.c
$ gcc/debug/define=("VMS", "error=as_fatal","const=") vax.c
$ gcc/debug/define=("VMS","const=") atof-vax.c
$ write sys$output " If you are building gas to work with the G++ compiler"
$ write sys$output " based upon gcc version 1.37.n or earlier, you should"
$ write sys$output " edit make-gas.com and make the changes indicated in the"
$ write sys$output "comments."
$! For older versions of G++, we need the jsb hack, the HACK_DEC_C_STARTUP
$! enables this.  Just use the compilation for vms.c that defines this instead
$! of the other one.
$ gcc/debug/define=("VMS", "error=as_fatal","const=") vms.c
$! gcc/debug/define=("VMS", "error=as_fatal","HACK_DEC_C_STARTUP","const=") vms.c
$ gcc/debug/define=("VMS","const=") vms-dbg.c
$ Link:
$ link/exec=gcc-as sys$input:/opt
!
!	Linker options file for GNU assembler
!
as,xrealloc,xmalloc,hash,hex-value,atof-generic,append,messages,expr,app,-
frags,input-file,input-scrub,output-file,read,subsegs,symbols,write,-
version,flonum-const,flonum-copy,flonum-mult,strstr,bignum-copy,-
obstack,vax,atof-vax,vms,vms-dbg,-
gnu_cc:[000000]gcclib/lib,sys$share:vaxcrtl/lib
