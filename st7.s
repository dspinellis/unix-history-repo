" space travel 7

clistp = 017
br0 = 040004
br1 = 040005
br2 = 040006
br3 = 040007

d10: 10
d3: 3
o246256: 0246256
o253052: 0253052
o246036: 0246036
o177: 0177
nplan: 32
d1: 1
d20: 20
dm1: -1
o141577: 0141577
o161577: 0161577
o164372: 0164372
o114: 0114
o20714: 020714
d383: 383
dm768: -768
o145777: 0145777
o165777: 0165777
o7: 7
o60: 060
o55: 055
o53: 053
o41: 041
o220000: 0220000
d400: 400
dm20: -20
o40004: 040004

fardst: 020;0200000; 0
f2pi: 03;0311037;552421
pid10: -01;0240662;756647
thrs: 02;0200000;0
f400: 011;0310000;0
crash: -028;0200000;0
stheta: 01;0200000;0
ctheta: 0;0;0
fpzero: 0;0;0
scale: 0
vscale: 6
ascale: -1
sdphi: -05;0253436;0700177
cdphi: 000;0377743;0201725
dhalt: 0400000
fpone: 01;0200000;0

9: .=.+t
horizv: .=.+3
.pbson: .=.+1
.pbsint: .=.+1
dspflg: .=.+1
par: .=.+1
absi: .=.+1
absx: .=.+3
absy: .=.+3
v: .=.+3
vv: .=.+3
spx: .=.+3
spy: .=.+3
wx: .=.+3
wy: .=.+3
twx: .=.+3
twy: .=.+3
setx: .=.+1
sety: .=.+1
narcs: .=.+1
nt: .=.+1
inflg: .=.+1
grvflg: .=.+1
dtmp1: .=.+3
dtmp2: .=.+3
delx: .=.+1
dely: .=.+1
tsetx: .=.+1
tsety: .=.+1
accflg: .=.+1
locpar: .=.+1
crflg: .=.+1
rpar: .=.+3
dpar: .=.+3
ax: .=.+3
ay: .=.+3
maxa: .=.+3
maxj: .=.+1
dcplan: .=.+1
fcplan: .=.+1
cplan: .=.+1
shipx: .=.+3
shipy: .=.+3
x: .=.+3
y: .=.+3
ox: .=.+3
oy: .=.+3
lanflg: .=.+1
goflg: .=.+1
forflg: .=.+1
bacflg: .=.+1
sphi: .=.+3
cphi: .=.+3
ftmp1: .=.+3
ftmp2: .=.+3
locflg: .=.+3

dsetx = 0140000
dsety = 0164000
vecx = 0120000
vecy = 0124000
m = 02000
displist:
	075057 "scale 1 intens 3 blink on lp 0 sym 0
	dsetx 800
	dsety 20
dispcl:
	0
	060004 "intens 0 blink off
	dsetx 0
	dsety 20
namedsp:
	.=.+10
	dsetx 400
	dsety 20
dssca:
	.=.+3
	040040 "scale 0
	dsetx 127
	dsety 250
	vecx 768
	dsetx 895
	dsety 255
	vecy 768
	dsetx 895
	dsety 1023
	vecx m 768
	dsetx 127
	dsety 1023
	vecy m 768
	dsetx 127
	dsety 255
	vecx 768
	dsetx 511
	dsety 255
	vecy 767
	dsetx 127
	dsety 639   "[--- - scan markup]
	vecx 767    "[an arrow starts between vecx and dspl, it points to the right - scan markup]
dspl:
	0400000
