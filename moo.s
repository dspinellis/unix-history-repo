" moo

   jmp 1f
reset:
   jms messg; <es>;<et>;012;0
1:
   jms messg; <wr>; <u 077; 040; 0
   sys open; moostat; 0
   spa
   sys exit
   dac fi
   sys open; moostat; 1
   spa
   sys exit
   dac fo
   jms readline
   dzm user
   skp
1:
   isz user
   jms getentry
      jmp noentry
   lac name
   sad u.name
   skp
   jmp 1b
   lac name+1
   sad u.name+1
   skp
   jmp 1b
   lac name+2
   sad u.name+2
   skp
   jmp 1b
   lac name+3
   sad u.name+3
   skp
   jmp 1b

gloop:
   jms messg; <re>; <ad>; <y 077; 040; 0
   law rqname-1
   dac 8
   law rqhand-1
   dac 9
   -nrq
   dac 2f
   jms readch
1:
   sad 8 i
   jmp 9 i
   isz 9
   isz 2f
   jmp 1b
   jms messg; 077012; 0
   jmp gloop
2: 0

noentry:
   jms messg; <na>; <me>; 040; <no>; <t 040; <fo>
      <un>; <d 073; 040; <en>; <te>; <r 077; 040; 0
   jms readch
   sad ch.y
   skp
   sys exit
   jms newline
   lac name
   dac u.name
   lac name+1
   dac u.name+1
   lac name+2
   dac u.name+2
   lac name+3
   dac u.name+3
   dzm u.ngames
   dzm u.nguess
   dzm u.ntime
   dzm u.npenalty
   jms putentry
   jmp gloop

gstart1:
   jms messg; <es>; 012; 0

gstart:
   jms random
   dac a1
1:
   jms random
   sad a1
   jmp 1b
   dac a2
1:
   jms random
   sad a1
   jmp 1b
   sad a2
   jmp 1b
   dac a3
1:
   jms random
   sad a1
   jmp 1b
   sad a2
   jmp 1b
   sad a3
   jmp 1b
   dac a4
   dzm nguess

guessloop:
   jms readguess
   lac nguess
   sza
   jmp 1f
   sys time
   lacq
   rcr
   dac stime
1:
   dzm nbull
   dzm ncow
   lac g1
   sad a1
   isz nbull
   sad a2
   isz ncow
   sad a3
   isz ncow
   sad a4
   isz ncow
   lac g2
   sad a1
   isz ncow
   sad a2
   isz nbull
   sad a3
   isz ncow
   sad a4
   isz ncow
   lac g3
   sad a1
   isz ncow
   sad a2
   isz ncow
   sad a3
   isz nbull
   sad a4
   isz ncow
   lac g4
   sad a1
   isz ncow
   sad a2
   isz ncow
   sad a3
   isz ncow
   sad a4
   isz nbull
   lac nbull
   sad d4
   jmp gdone
   jms messg; 040040; 040040; 040040; <bc>; 075; 0
   lac nbull
   jms number
   lac ncow
   jms number
   jms newline
   isz nguess
   jmp guessloop

gdone:
   sys time
   lacq
   rcr
   cma
   tad stime
   cma
   spa
   tad o400000
   rcr
   dac stime
   lac u.ntime
   tad stime
   dac u.ntime
   lac u.nguess
   tad nguess
   dac u.nguess
   isz u.ngames
   jms putentry
   jms messg; 012; <g 075; 0
   lac nguess
   jms number
   jms messg; 040; <t 075; 0
   lac stime
   cll; idiv; 15
   lacq
   jms number
   jms newline
   jmp gloop

random: 0
   sys time
   lacq
   tad rand
   cll; mul; 78125
   lacq
   dac rand
   cll; idiv; 10
   jmp random i

newline: 0
   jms messg; 012; 0
   jmp newline i

number: 0
   lmq
   law 2f+1
   dac 3f
   lacq
1:
   cll; idiv; 10
   tad o60
   dac 3f i
   isz 3f
   lacq
   sza
   jmp 1b
1:
   -1
   tad 3f
   dac 3f
   lac 3f i
   sna
   jmp number i
   dac .+2
   jms messg; ..; 0
   jmp 1b
2: 0; .=.+10
3: .=.+1

readguess: 0
   jms messg; 077040; 0
   jms cnum
   dac g1
   jms cnum
   dac g2
   jms cnum
   dac g3
   jms cnum
   dac g4
   jms readch
   sad o12
   jmp readguess i
   jmp readguess+1

cnum: 0
   jms readch
   tad om60
   spa
   jmp readguess+1
   dac 1f
   tad dm10
   sma
   jmp readguess+1
   lac 1f
   jmp cnum i
1: 0

readch: 0
   cla
   sys read; 1f; 1
   lac 1f
   lrss 9
   jmp readch i
1: 0

readline: 0
   -1
   dac r1f
   jms read2
   dac name
   jms read2
   dac name+1
   jms read2
   dac name+2
   jms read2
   dac name+3
   jmp readline i

read2: 0
   jms read1
   alss 9
   dac 1f
   jms read1
   xor 1f
   jmp read2 i
1: 0

read1: 0
   lac r1f
   sna
   jmp 1f
   jms readch
   sad o12
   skp
   jmp read1 i
   dzm r1f
1:
   lac o40
   jmp read1 i
r1f: 0

getentry: 0
   lac user
   cll; mul; 16
   lacq
   dac 0f
   lac fi
   sys seek; 0:..; 0
   lac fi
   sys read; userdata; 16
   sza
   isz getentry
   jmp getentry i

putentry: 0
   lac user
   cll; mul; 16
   lacq
   dac 0f
   lac fo
   sys seek; 0:..; 0
   lac fo
   sys write; userdata; 16
   jmp putentry i

average:
   jms messg; <ve>;<r 012; 0
   jms paver
   jmp gloop

standing:
   jms messg; <ta>; <nd>; 012; 0
   lac user
   dac 2f
   dzm user
   skp
1:
   isz user
   jms getentry
      jmp 1f
   lac d1
   sys write; u.name; 4
   jms paver
   jmp 1b
1:
   lac 2f
   dac user
   jms getentry
      nop
   jmp gloop
2: 0

paver: 0
   lac u.ngames
   sna
   jmp 1f
   jms messg; <n; 075; 0
   lac u.ngames
   jms number
   jms messg; 040; <g 075; 0
   lac u.nguess
   jms number
   lac u.nguess
   jms aver
   jms messg; 040; <t 075; 0
   lac u.ntime
   cll; idiv; 15
   lacq
   jms number
   lac u.ntime
   cll; idiv; 15
   lacq
   jms aver
   jms newline
   jmp paver i
1:
   jms messg; <no>; 040; <ga>; <me>; <s 012; 0
   jmp paver i

aver: 0
   dac 1f
   lac u.ngames
   dac 0f
   jms messg; 050; 0
   lac 1f
   cll; idiv; 0: ..
   dac 1f
   lacq
   jms number
   jms messg; 056; 0
   lac u.ngames
   dac 0f
   lac 1f
   cll; mul; 100
   cll; div; 0:..
   ecla; div; 10
   dac 1f
   lacq
   jms number
   lac 1f
   jms number
   jms messg; 051; 0
   jmp aver i
1: .=.+1

messg: 0
   -1
   tad messg
   dac 8
1:
   lac 8 i
   sna
   jmp 8 i
   dac 1f
   lac d1
   sys write; 1f; 1
   jmp 1b
1: 0

rqname:
   y>
   q>
   a>
   s>
   r>
nrq = .-rqname
rqhand:
   jmp gstart1
   jmp quit
   jmp average
   jmp standings
   jmp reset

quit:
   jms messg; <ui>; <t 012; 0
   sys exit

d1: 1
o400000: 0400000
o60: 060
om60: -060
dm10: -10
d4: 4
o40: 040
o12: 012
ch.y: y>

m1: <wr>;<u 077; 040
m1s = .-m1
m2: <re>;<ad>;<y 077; 040
m2s = .-m2
m3: <na>;<me>;040;<no>;<t 040;<fo>;<un>;<d 012
    <en>;<te>;<r 077; 040
m3s = .-m3
m5: 077040
m5s = .-m5
m7: 012;<gu>;<es>;<s 075
m7s = .-m7
m8: <av>;<g 075
m8s = .-m8
m9: 057
m9s = .-m9
m10: 040075; 040
m10s = .-m10
moostat: <mo>;<os>;<ta>;<t 040

fi: .=.+1
fo: .=.+1
name: .=.+4
nguess: .=.+1
nbull: .=.+1
ncow: .=.+1
stime: .=.+1
a1: .=.+1
a2: .=.+1
a3: .=.+1
a4: .=.+1
g1: .=.+1
g2: .=.+1
g3: .=.+1
g4: .=.+1
rand: .=.+1
user: .=.+1
userdata:
   u.name: .=.+4
   u.ngames: .=.+1
   u.nguess: .=.+1
   u.ntime: .=.+1
   u.npenalty: .=.+1
   u.hguess: .=.+1
   u.lguess: .=.+1
   u.htime: .=.+1
   u.ltime: .=.+1
   . = userdata+16
