" op.s is the op code defination file
" for the assembler
" no iot ops are defined
" system cal entries are defined
"
dac = 0040000
jms = 0100000
dzm = 0140000
lac = 0200000
xor = 0240000
add = 0300000
tad = 0340000
xct = 0400000
isz = 0440000
and = 0500000
sad = 0540000
jmp = 0600000
eae = 0640000
i = 020000
opr = 0740000
law = opr i
cma = opr 1
cml = opr 2
ral = opr 010
rar = opr 020
sma = opr 0100
sza = opr 0200
snl = opr 0400
skp = opr 01000
spa = opr 01100
sna = opr 01200
szl = opr 01400
cll = opr 04000
cla = opr 010000
las = opr 010004
lrs = eae 0500
lrss = i lrs
lls = eae 0600
llss = i lls
als = eae 0700
alss = i als
lacq = eae 01002
lacs = eae 01001
clq = eae 010000
omq = eae 2
cmq = eae 4
lmq = eae 012000

sys = i
save = 1
open = 3
read = 4
write = 5
creat = 6
seek = 7
close = 9
exit = 14
