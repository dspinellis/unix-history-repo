9:.=.+t

end = -1

no = 0000000
rx = 0040000; gx = rx
rc = 0100000
rt = 0140000; gc = rt
rf = 0200000; gf = rf
rw = 0240000; gk = rw
ra = 0300000; gp = ra
rb = 0340000;gq = rb
ro = 0400000
rm = 0440000
rs = 0500000
rv = 0540000


ljmp:jmp
l.llss:llss
l.lrs:lrs
l.lls:lls
l.ra:ra
l.rb:rb
l.rw:rw
l.gk:gk
l.gcx:gc x

x = 020000
st = 0100
fi = 0200
opmask:0740000
exitmask: x

m1:0-1
d0:o0:0
d1:o1:1
d2:o2:2
d3:o3:3
d4:o4:4
d5:o5:5
d6:o6:6
d7:o7:7
d8:o10:8
asciisp:040
asciinl:012
nl:012777
onenl:nl
bugr:.+1;<rn>;<rx>;<rc>;<rt>;<rf>;<rw>;<ra>;<rb>
  <ro>;<rm>;<rs>;<rv>
bugg:.+1;<gn>;<gx>;<gz>;<gc>;<gf>;<gk>;<gp>;<gq>

o17:017
o60:060
o77:077
o777:0777

o417777:0417777
o400000:0400000
o740000:0740000
o377700:0377700
o17777:017777

o757777:0757777
o600600:0600600

junk:0
junk1:0

output:1
input:0

stbit:st
fibit:fi
holdlv:0
rand1:0
rand2:0


symwrite:0
symbot: symtab
symsiz = 500
symmax:symsiz

equwrite:0
equread:0
equ = equread
equbot: equtab
equsiz = 500
equmax: equsiz
delta: 2
mdelta:0-2

sbsiz=50
sbmax:sbsiz
sbwrite:0
sbbot:sbbuf

fflag:0
gflag:0
ignore:.+1;0400000;0;0;0;0;0;0;4
frame:rstack
nframe:rstack+6
env = ignore
d.ii = d2
d.env = d3
d.blkmod = d3
d.j = d4
d.k = d5
dffrmsz:6
framsiz:4
refrsz = d6
gefrsz = d4
ii: start
k:0

rsiz = 500
rmax: rsiz
rbot:rstack
rtop:rstack+rsiz

owrite:0
obot:obuf
osiz=64
