/ ux -- unix

systm:

	.=.+2
	.=.+128.
	.=.+2
	.=.+64.
	s.time: .=.+4
	s.syst: .=.+4
	s.wait: .=.+4
	s.idlet:.=.+4
	s.chrgt:.=.+4
	s.drerr:.=.+2
inode:
	i.flgs: .=.+2
	i.nlks: .=.+1
	i.uid:  .=.+1
	i.size: .=.+2
	i.dskp: .=.+16.
	i.ctim: .=.+4
	i.mtim: .=.+4
	. = inode+32.
_mount:	.=.+1024.
proc:
	p.pid:  .=.+[2*nproc]
	p.dska: .=.+[2*nproc]
	p.ppid: .=.+[2*nproc]
	p.break:.=.+[2*nproc]
	p.link: .=.+nproc
	p.stat: .=.+nproc
tty:
	. = .+[ntty*8.]
fsp:	.=.+[nfiles*8.]
bufp:	.=.+[nbuf*2]+6
sb0:	.=.+8
sb1:	.=.+8
swp:	.=.+8
ii:	.=.+2
idev:	.=.+2
cdev:	.=.+2
deverr: .=.+12.
active:	.=.+2
rfap:	.=.+2
rkap:	.=.+2
tcap:	.=.+2
tcstate:.=.+2
tcerrc:	.=.+2
mnti:	.=.+2
mntd:	.=.+2
mpid:	.=.+2
clockp:	.=.+2
rootdir:.=.+2
toutt:	.=.+16.; touts: .=.+32.
runq:	.=.+6

wlist:	.=.+40.
cc:	.=.+30.
cf:	.=.+31.
cl:	.=.+31.
clist:	.=.+510.
imod:	.=.+1
smod:	.=.+1
mmod:	.=.+1
uquant:	.=.+1
sysflg:	.=.+1
pptiflg:.=.+1
ttyoch:	.=.+1
 .even
 .=.+100.; sstack:
buffer:	.=.+[ntty*140.]
	.=.+[nbuf*520.]

 . = core-64.
user:
	u.sp:    .=.+2
	u.usp:   .=.+2
	u.r0:    .=.+2
	u.cdir:  .=.+2
	u.fp:    .=.+10.
	u.fofp:  .=.+2
	u.dirp:  .=.+2
	u.namep: .=.+2
	u.off:   .=.+2
	u.base:  .=.+2
	u.count: .=.+2
	u.nread: .=.+2
	u.break: .=.+2
	u.ttyp:  .=.+2
	u.dirbuf:.=.+10.
	u.pri:   .=.+2
	u.intr:  .=.+2
	u.quit:  .=.+2
	u.emt:   .=.+2
	u.ilgins:.=.+2
	u.cdev:  .=.+2
	u.uid:   .=.+1
	u.ruid:  .=.+1
	u.bsys:  .=.+1
	u.uno:   .=.+1
. = core

