/ tap4 -- dec-tape lod/dmp

.data
tc:	</dev/tap>
tcx:	<x\0>
mt:	</dev/mt>
mtx:	<0\0>
tboot:	</usr/mdec/tboot\0>
mboot:	</usr/mdec/mboot\0>
.even
.bss
dir:	. = .+[mdirent*dirsiz]
tapeb:
map:	.=.+4096.
emap:
ch:	.=.+1
flc:	.=.+1
flf:	.=.+1
fli:	.=.+1
flm:	.=.+1
flu:	.=.+1
flv:	.=.+1
flw:	.=.+1
.even

command:.=.+2
sum:	.=.+2
size:	.=.+2
nentr:	.=.+2
nused:	.=.+2
nfree:	.=.+2
lused:	.=.+2
catlb:	.=.+20.
narg:	.=.+2
rnarg:	.=.+2
parg:	.=.+2
fio:	.=.+2
mss:	.=.+2
ndirent:.=.+2
ndentd8:.=.+2
edir:	.=.+2
rseeka:	.=.+2
wseeka:	.=.+2
tapsiz:	.=.+2
name:	.=.+32.
name1:	.=.+32.
statb:	.=.+40.

smdate = 30.
