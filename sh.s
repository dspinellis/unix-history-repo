" ** 11-56-91.pdf page 30
" sh

clear:
   jmp shell

1:
   dzm i 8
   isz clear
   jmp 1b
   lacq
   jmp 017771
zerop: .-1

comerr:
   lac d1
   sys write; errmes; 1


shell:
   lac d1
   sys write; ready; 1
shell1:
   lac delim
   sad newln
   jms rline
   jms getcom
   lac narg
   sna
   jmp comretrn
   lac args
   sad chcom
   skp
   jmp 3f
   lac args+1
   sad spsp
   jmp doch
3:
   sys fork
      skp
      jmp loadcom
   spa
   jmp comerr
   lmq
   lac delim
   sad amper
   jmp shell1
   lacq			" hand written: send
   clq			" "done" (crossed out)
   sys smes		" mesg to child
comretrn:
   lac delim
   sad newln
   jmp shell
   jmp shell1


loadcom:
   sys open; args; 0
   sma
   jmp 1f
   sys link; system; args; args
   spa
" ** 11-56-91.pdf page 31
   jmp 2f
   -1
   dac lnkflg
   jmp loadcom

2:
   lac lnkflg
   sna
   jmp 3f
   sys unlink; args
3:
   lac d1
   sys write; args; 4
   lac d1
   sys write; errmes; 1
   sys exit

1:
   lac lnkflg
   sna
   jmp 1f
   sys unlink; args
1:
   lac in
   sna
   jmp 2f
   cla
   sys close
   sys open; in; 0
   sma
   jmp 2f
   lac d1
   sys write; in; 4
   lac d1
   sys write; errmes; 1
   sys exit
2:
   lac out
   sna
   jmp 1f
   lac d1
   sys close
   lac o17
   sys creat; out
   spa
   sys exit
1:
   lac narg
   cma
   dac t1
   tad o17771
   dac 017777
   tad dm1
   dac 8
   and o7777
   dac boot+2
   cma
   tad d7
   dac clear
   lac nargp
   dac 9
" ** 11-56-91.pdf page 32
2:
   lac i 9
   dac i 8
   isz t1
   jmp 2b
   lac bootp
   dac 9
   -6
   dac t1
2:
   lac i 9
   dac i 8
   isz t1
   jmp 2b
   lac d2
   lmq
   lac zerop
   dac 8
   jmp clear+1

boot:
   sys read; 4096; ..
   lacq
   sys close
   jmp 4096

getcom: 0
   law args-1
   dac 10
   dzm in
   dzm out
   dzm narg
   dzm lnkflg

   jms get
nparm:
   sad gr
   jmp cgr
   sad ls
   jmp cls
   sad amper
   jmp endcom
   sad semic
   jmp endcom
   sad newln
   jmp endcom
   sad space
   jmp nparm-1
   lmq
   lac narg
   tad d4
   dac narg
   lacq
   jms getparm
   jmp nparm

endcom:
   dac delim
   jmp getcom i

cls:
" ** 11-56-91.pdf page 33
   jms get
   jms getparm
   dac t1
   law in-1
   jmp cpio
cgr:
   jms get
   jms getparm
   dac t1
   law out-1
cpio:
   dac 11
   lac 10
   tad dm4
   dac 10
   lmq
   -4
   dac c1
1:
   lac i 10
   dac i 11
   isz c1
   jmp 1b
   lacq
   dac 10
   lac t1
   jmp nparm

getparm:0
   lmq
   -8
   dac c1
   lacq
   skp
1:
   jms get
   sad space
   jmp 1b
   jms checkdlm
   jmp comerr
   jmp 2f
1:
   jms get
   jms checkdlm
   jmp fill1
2:
   alss 9
   isz c1
   lmq
   jms get
   jms checkdlm
   jmp fill
   omq
   dac i 10
   isz c1
   jmp 1b
1:
   jms get
   jms checkdlm
   jmp i getparm
   jmp 1b

" ** 11-56-91.pdf page 34
fill:
   dac t1
   lac space
   omq
   dac i 10
   isz c1
   nop
   lac t1
fill1:
   lmq
   lac c1
   spa
   jmp 1f
   lacq
   jmp i getparm
1:
   lac spsp
   dac i 10
   isz c1
   isz c1
   jmp 1b
   lacq
   jmp i getparm

checkdlm: 0
   sad space
   jmp i checkdlm
   sad newln
   jmp i checkdlm
   sad amper
   jmp i checkdlm
   sad semic
   jmp i checkdlm
   isz checkdlm
   jmp i checkdlm

get: 0
   lac i 8
   sad slash
   skp
   jmp i get
   lac i 8
   sad newln
   skp
   jmp comerr
   lacq
   dac 1f
   jms rline
   lac 1f
   lmq
   lac space
   jmp i get

1: 0
rline:0
2:
   law lineb-1
   dac 15
   dac 8
1:
   jms getcha
" ** 11-56-91.pdf page 35
   dac i 15
   sad newln
   jmp i rline
   sad sharp
   jmp psharp
   sad atsign
   jmp 2b
   jmp 1b
psharp:
   -1
   tad 15
   sad 2b
   jmp 2b
   tad dm1
   dac 15
   jmp 1b

getcha: 0
   lac char
   dzm char
   sza
   jmp i getcha
   isz nread
   jmp 1f
   cla
   sys read; inbuf; 64
   spa sna
   jmp lgout
   cma
   tad d1
   dac nread
   law inbuf-1
   dac 14
1:
   lac i 14
   lmq
   and o777
   dac char
   ecla llss 9
   jmp i getcha

doch:
   lac narg
   lrss 2
   cma
   tad d1
   dac narg
   law args+4
   dac 1f
2:
   isz narg
   skp
   jmp comretrn
   sys chdir; 1:0
   spa
   jmp cherr
   lac 1b
   tad d4
   dac 1b
   jmp 2b
cherr:
" ** 11-56-91.pdf page 36
   lac 1b
   dac .+3
   lac d1
   sys write; ..; 4
   jmp comerr

lgout:
   clq
   lac d1
   sys smes
   sys exit


d1: 1
dm1: -1
d4: 4
dm4: -4
d2: 2
d7: 7
o17: 017
o17771: 017771
o7777: 07777
o777: 0777
gr: 076
ls: 074
amper: &>
semic: 073
space: 040
sharp: 043
atsign: 0100
newln: 012
slash: 057
in: 0;0;0;0
out: 0;0;0;0
errmes: 077012
chcom: <ch>
ready: 0100040
delim: 012
system: <sy>; <st>; <em>; spsp: 040040
nargp: narg-1
bootp: boot-1
char: 0
nread: -1
lineb: .=.+128
inbuf: .=.+64
c1: .=.+1
t1: .=.+1
lnkflg: .=.+1
narg: .=.+1
args:
