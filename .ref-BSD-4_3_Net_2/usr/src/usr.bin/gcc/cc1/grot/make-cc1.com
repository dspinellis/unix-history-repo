$!
$!	Build the GNU "C" compiler on VMS
$!   (To try to build with VAX C, replace `gcc' with `cc/noopt'
$!    and delete `cc1_options="-mpcc-alignment"'.
$!    Also add `/sel' after `gcclib/lib' except in the last link.
$!    You also need to get alloca.mar from Bison
$!    and to make definitions for bzero, bcopy and bcmp.)
$!
$ set verify
$!
$!	C compiler
$!
$ CC	:=	gcc
$!
$!	Compiler options
$!
$ CFLAGS =	"/debug/cc1_options=""-mpcc-alignment""/inc=([],[.config])"
$!
$!	Link options
$!
$ LDFLAGS :=	/nomap
$!
$!	Link libraries
$!
$ LIBS :=	gnu_cc:[000000]gcclib/libr,sys$share:vaxcrtl/libr
$!
$ if "''p1'" .eqs. "LINK" then goto Link
$!
$!	Recompile
$!
$ 'CC 'CFLAGS rtl.c
$ 'CC 'CFLAGS obstack.c
$!	Generate insn-flags.h
$ 'CC 'CFLAGS genflags.c
$ link 'LDFLAGS' genflags,rtl,obstack, 'LIBS'
$ assign/user insn-flags.h sys$output:
$ mcr sys$disk:[]genflags md
$!	Generate insn-codes.h
$ 'CC 'CFLAGS gencodes.c
$ link 'LDFLAGS' gencodes,rtl,obstack, 'LIBS'
$ assign/user insn-codes.h sys$output:
$ mcr sys$disk:[]gencodes md
$!	Generate insn-config.h
$ 'CC 'CFLAGS genconfig.c
$ link 'LDFLAGS' genconfig,rtl,obstack, 'LIBS'
$ assign/user insn-config.h sys$output:
$ mcr sys$disk:[]genconfig md
$!
$ 'CC 'CFLAGS toplev.c
$!
$ t1:='f$search("C-PARSE_TAB.C")'
$ if "''t1'" .eqs. "" then goto 10$
$ t1:='f$file_attributes("C-PARSE.Y","RDT")'
$ t1:='f$cvtime(t1)'
$ t2:='f$file_attributes("C-PARSE_TAB.C","RDT")'
$ t2:='f$cvtime(t2)'
$ if t1 .les. t2 then goto 20$
$ 10$:
$ bison /verbose c-parse.y
$ 20$:
$!
$ 'CC 'CFLAGS c-parse_tab.c /define="__inline=inline"
$ 'CC 'CFLAGS version.c
$ 'CC 'CFLAGS tree.c
$ 'CC 'CFLAGS print-tree.c
$ 'CC 'CFLAGS c-decl.c
$ 'CC 'CFLAGS c-typeck.c
$ 'CC 'CFLAGS c-convert.c
$ 'CC 'CFLAGS stor-layout.c
$ 'CC 'CFLAGS fold-const.c
$ 'CC 'CFLAGS varasm.c
$ 'CC 'CFLAGS expr.c
$ 'CC 'CFLAGS stmt.c
$ 'CC 'CFLAGS expmed.c
$ 'CC 'CFLAGS explow.c
$ 'CC 'CFLAGS optabs.c
$ 'CC 'CFLAGS symout.c
$ 'CC 'CFLAGS dbxout.c
$ 'CC 'CFLAGS rtlanal.c
$ 'CC 'CFLAGS emit-rtl.c
$!	Generate insn-emit.c
$ 'CC 'CFLAGS genemit.c
$ link 'LDFLAGS' genemit,rtl,obstack, 'LIBS'
$ assign/user insn-emit.c sys$output:
$ mcr sys$disk:[]genemit md
$!
$ 'CC 'CFLAGS insn-emit.c
$ 'CC 'CFLAGS jump.c
$ 'CC 'CFLAGS cse.c
$ 'CC 'CFLAGS loop.c
$ 'CC 'CFLAGS flow.c
$ 'CC 'CFLAGS stupid.c
$ 'CC 'CFLAGS combine.c
$ 'CC 'CFLAGS regclass.c
$ 'CC 'CFLAGS local-alloc.c
$ 'CC 'CFLAGS global-alloc.c
$ 'CC 'CFLAGS reload.c
$ 'CC 'CFLAGS reload1.c
$!	Generate insn-peep.c
$ 'CC 'CFLAGS genpeep.c
$ link 'LDFLAGS' genpeep,rtl,obstack, 'LIBS'
$ assign/user insn-peep.c sys$output:
$ mcr sys$disk:[]genpeep md
$!
$ 'CC 'CFLAGS insn-peep.c
$ 'CC 'CFLAGS final.c
$ 'CC 'CFLAGS recog.c
$!	Generate insn-recog.c
$ 'CC 'CFLAGS genrecog.c
$ link 'LDFLAGS' genrecog,rtl,obstack, 'LIBS'
$ assign/user insn-recog.c sys$output:
$ mcr sys$disk:[]genrecog md
$!
$ 'CC 'CFLAGS insn-recog.c
$!	Generate insn-extract.c
$ 'CC 'CFLAGS genextract.c
$ link 'LDFLAGS' genextract,rtl,obstack, 'LIBS'
$ assign/user insn-extract.c sys$output:
$ mcr sys$disk:[]genextract md
$!
$ 'CC 'CFLAGS insn-extract.c
$!	Generate insn-output.c
$ 'CC 'CFLAGS genoutput.c
$ link 'LDFLAGS' genoutput,rtl,obstack, 'LIBS'
$ assign/user insn-output.c sys$output:
$ mcr sys$disk:[]genoutput md
$!
$ 'CC 'CFLAGS insn-output.c
$ 'CC 'CFLAGS integrate.c
$ 'CC 'CFLAGS caller-save.c
$!
$!
$!	Link it
$!
$ Link:
$ link 'LDFLAGS' /exe=gcc-cc1 sys$input:/opt,'LIBS'
!
!	"CC1" Linker options file
!
toplev,c-parse_tab,tree,print-tree,c-decl,c-typeck,c-convert,stor-layout,fold-const,-
varasm,rtl,rtlanal,expr,stmt,expmed,explow,optabs,symout,dbxout,emit-rtl,insn-emit,-
jump,cse,loop,flow,stupid,combine,regclass,local-alloc,global-alloc,reload,-
reload1,insn-peep,final,recog,insn-recog,insn-extract,insn-output,obstack,-
integrate,caller-save,version
$!
$!	Done
$!
