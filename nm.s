" nm

   lac 017777 i
   sad d4
   skp
   jmp 1f
   law n.out
   dac fname
   jmp 2f
1:
   lac 017777
   tad d1
   dac fname

loop:
   lac 017777 i
   sad d4
   jmp done
   tad dm4
   dac 017777 i
   lac fname
   tad d4
   dac fname
2:
   law 012
   jms putc
   sys open; fname: 0; 0
   sma
   jmp 1f
   lac fname
   dac 2f
   lac d1
   sys write; 2: 0; 4
   lac d1
   sys write; mes; 2
   jmp loop
mes:
   040077; 012

1:
   lac d2
   sys read; buf; 3072
   cll; idiv; 6
   lacq
   dac size
   law o200000
   dac c2

print:
   lac o200000
   dac c2 i
   dzm c2name
   -1
   tad size
   cma
   dac c1
   law buf
   dac c3
   lac o200000
   dac c2name
1:
   lac c3 i
   cma
   tad c2name
   spa
   jmp 2f
   lac c3 i
   dac c2name
   lac c3
   dac c2
2:
   law 6
   tad c3
   dac c3
   isz c1
   jmp 1b
   lac c2name
   sad o200000
   skp
   jmp 1f
   lac d2
   sys close
   jmp loop
1:

   lac c2
   tad d3
   dac t
   lac i t
   sna
   jmp print
   isz t
   lac i t
   sna
   jmp print
   -1
   tad c2
   dac 8
   -4
   dac c3
1:
   lac 8 i
   lrss 9
   jms putc
   llss 9
   jms putc
   isz c3
   jmp 1b
   lac i t
   sad d3
   jmp undef
   sna
   jmp 1f
   law 0162
   skp
1:
   law 0141
   dac type
   law 040
   jms putc
   isz t
   lac i t
   lmq
   -6
   dac c3
1:
   cla
   llss 3
   tad o60
   jms putc
   isz c3
   jmp 1b
   law 040
   jms putc
   lac type
   jms putc
   law 012
   jms putc
   jmp print
undef:
   -8
   dac c3
1:
   law 040
   jms putc
   isz c3
   jmp 1b
   law 0165
   jms putc
   law 012
   jms putc
   jmp print

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

putc: 0
   and o777
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
ipt: 0
eipt: 0
iipt: .+1; .=.+64
fi: 0
opt: .+2
iopt: .+1; .=.+64
noc: 0
fo: 1

c1: 0
c2: 0
c3: 0
t: 0
size: 0
c2name: 0
type: 0

d1: 1
d128: 128
o200000: 0200000
o777: 0777
o400000: 0400000
d2: 2
d4: 4
dm4: -4
o60: 060
d3: 3
d6: 6
n.out:
0156056;0157165;0164040;040040
buf:
