" ls
" list

   lac 017777
   tad d1
   dac name
   lac name i
   sad ali
   dac longflg
   lac 017777 i
   sad d4
   skp
   jmp loop
   law dd
   dac name
   jmp 2f

loop:
   lac 017777 i
   sad d4
   jmp done
   tad dm4
   dac 017777 i
   lac name
   tad d4
   dac name
2:
   law stbuf
   sys status; dd; name:..
   spa
   jmp badfile
   lac s.flags
   and o20
   sna
   jmp badfile
   lac name
   dac 0f
   sys open; 0:..; 0
   spa
   jmp badfile
   dac fi
   jms readdir
   lac fi
   sys close
1:
   lac o200000
   dac maxfn
   jms findf
   lac maxfn
   sad o200000
   jmp loop

   lac longflg
   sza
   jms longout
   law maxfn
   jms putfn
   law 012
   jms putc
   lac o200000
   dac maxfp i
   jmp 1b

longout: 0
   lac name
   dac 0f
   law stbuf
   sys status; 0:..; maxfn
   lac s.i
   jms octal; -3
   lac s.flags
   jms octal; -2
   lac s.uid
   jms octal; -2
   -1
   tad s.nlinks
   cma
   jms octal; -2
   lac s.size
   jms octal; -5
   jmp longout i

octal: 0
   lmq
   lac d5
   tad octal i
   cma
   dac t
1:
   llss 3
   isz t
   jmp 1b
   lac octal i
   dac t
1:
   ecla llss 3
   tad o60
   jms putc
   isz t
   jmp 1b
   law 040
   jms putc
   isz octal
   jmp octal i

toobig:
   law 076
   jms putc
   law 040
   jms putc

badfile:
   lac name
   jms putfn
   law 040
   jms putc
   law 077
   jms putc
   law 012
   jms putc
   jmp loop

putfn: 0
   dac t
   -4
   dac t1
1:
   lac t i
   lrss 9
   sad o40
   jmp putfn i
   jms putc
   lac t i
   and o177
   sad o40
   jmp putfn i
   jms putc
   isz t
   isz t1
   jmp 1b
   jmp putfn i

done:
   lac noc
   sna
   sys exit
   and d1
   sna cla
   jmp 1f
   jms putc
   jmp done
1:
   lac noc
   rcr
   dac 1f
   lac fo
   sys write; iopt+1; 1:..
   sys exit

readdir: 0
   lac fi
   sys read; buf; 2048
   sad .-1
   jmp toobig
   lrss 3
   cma
   tad d1
   dac ndir
   jmp readdir i

findf: 0
   law buf
   dac t
   lac ndir
   dac t1
1:
   lac t i
   sna
   jmp 2f
   isz t
   lac t i
   cma
   tad maxfn
   spa
   jmp 2f+1
   lac t
   dac maxfp
   lac t i
   dac maxfn
   skp
2:
   isz t
   lac t
   tad d7
   dac t
   isz t1
   jmp 1b

   lac maxfp
   dac 8
   lac 8 i
   dac maxfn+1
   lac 8 i
   dac maxfn+2
   lac 8 i
   dac maxfn+3
   jmp findf i

putc: 0
   and o177
   dac 2f+1
   lac opt
   dac 2f
   add o400000
   dac opt
   spa
   jmp 1f
   lac 2f i
   xor 2f+1
   jmp 3f
1:
   lac 2f+1
   alss 9
3:
   dac 2f i
   isz noc
   lac noc
   sad d128
   skp
   jmp putc i
   lac fo
   sys write; iopt+1; 64
   lac iopt
   dac opt
   dzm noc
   jmp putc i
2: 0;0
fi: 0
opt: .+2
iopt: .+1; .=.+64
noc: 0
fo: 1
longflg: 0

d1: 1
d5: 5
o60: 060
o40: 040
o20: 020
d7: 7
o400000: 0400000
d128: 128
d4: 4
dm4: -4
o177: 0177
o200000: 0200000
dd: 056056;040040;040040;040040
ali: <li>

t: .=.+1
t1: .=.+1
maxfn: .=.+4
maxfp: .=.+1
ndir: .=.+1
stbuf:
   s.flags: .=.+8
   s.uid: .=.+1
   s.nlinks: .=.+1
   s.size: .=.+2
   s.i: .=.+1
buf:
