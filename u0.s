/ u0 -- unix

cold = 0 
orig = 0 . / orig = 0. relocatable

rkda = 177412 / disk address reg            rk03/rk11
rkds = 177400 / driv status reg             rk03/rk11
rkcs = 177404 / control status reg          rk03/rk11
rcsr = 174000 / receiver status reg         dc-11
rcbr = 174002 / receiver buffer reg         dc-11
tcsr = 174004 / xmtr status reg             dc-11
tcbr = 174006 / xmtr buffer reg             dc-11
tcst = 177340 / dec tape control status     tc11/tu56
tccm = 177342 / dec tape command reg        tc11/tu56
tcwc = 177344 /          word count         tc11/tu56
tcba = 177346 /          bus addr           tc11/tu56
tcdt = 177350 /          data reg           tc11/tu56
dcs  = 177460 / drum control status         rf11/rs11
dae  = 177470 / drum address extension      rf11/rs11
lks  = 177546 / clock status reg            kw11-l
prs  = 177550 / papertape reader status     pc11
prb  = 177552 /                  buffer     pc11
pps  = 177554 /            punch status     pc11
ppb  = 177556 /            punch buffer     pc11
/lps = 177514   line printer status         (future)
/lpb = 177516   line printer buffer         (future)
tks  = 177560 / console read status         asr-33
tkb  = 177562 /         read buffer         asr-33
tps  = 177564 /         punch status        asr-33
tpb  = 177566 /         punch buffer        asr-33
ps   = 177776 / processor status

fpsym = 0   / define fpsym even though we cannot use it
.. = 0 .    / ensure a.out starts at location 0, not 040000

halt = 0
wait = 1
rti  = 2

nproc = 16. / number of processes
nfiles = 50.
ntty = 8+1
nbuf = 6
 .if cold / ignored if cold = 0
nbuf = 2
.endif

core = orig+40000  / specifies beginning of user's core
ecore = core+40000 / specifies end of user's core (4096 words)

/      4;4     init by copy
/      unkni;0 " error
/      fpsym;0 " illg in tr
       unkni;0 / trace and trap (see Sec. B.1 page  )
       unkni;0 / trap
       panic;0 / pwr
       rtssym;0 / emt
       sysent;0 / sys
 . = orig+60
       ttyi;240 / interrupt vector tty in       ; processor level 5
       ttyo;240 / interrupt vector tty out
       ppti;240 /                  punch papertape in
       ppto;240 /                  punch papertape out
       clock;340 / clock interrupt vector       ; processor level 7
 . = orig+200
/     lpto; 240  line printer interrupt     ; processor level 5 (future)
 . = orig+204
       drum;300 / drum interrupt         ; processor level 6
 . = orig+214
       tape;300 / dec tape interrupt
       disk;300 / rk03 interrupt
 . = orig+300
       0*4+trcv; 240; 0*4+txmt; 240 / dc11 input,output interrupt vectors
       1*4+trcv; 240; 1*4+txmt; 240
       2*4+trcv; 240; 2*4+txmt; 240
       3*4+trcv; 240; 3*4+txmt; 240
       4*4+trcv; 240; 4*4+txmt; 240
       5*4+trcv; 240; 5*4+txmt; 240
       6*4+trcv; 240; 6*4+txmt; 240
       7*4+trcv; 240; 7*4+txmt; 240

 . = orig+400
/ copy in transfer vectors

	mov    $ecore,sp / put pointer to ecore in the stack pointer
	jsr    r0,copyz; 0; 14 / clear locations 0 to 14 in core
	mov    $4,r0
	clr    r1
	mov    r0,(r1)+ / put value of 4 into location 0
	mov    r0,(r1)+ / put value of 4 into location 2
	mov    $unkni,(r1)+ / put value of unkni into location 4;
                            / time out, bus error
	clr    (r1)+ / put value of 0 into location 6
	mov    $fpsym,(r1)+ / put value of fpsym into location 10
	clr    (r1)+ / put value of 0 into location 12
/ clear core
	.if cold / ignored if cold = 0
	halt / halt before initializing rf file system; user has
             / last chance to reconsider
	.endif

	jsr    r0,copyz; systm; ecore / clear locations systm to ecore
	mov    $s.chrgt+2,clockp / intialize clockp
/ allocate tty buffers; see H.0 for description
	mov    $buffer,r0
	mov    $tty+6,r1
1:
	mov    r0,(r1)
	add    $140.,r0 / tty buffers are 140. bytes long
	add    $8,r1
	cmp    r1,$tty+[ntty*8] / has a buffer been assigned for each tty
	blo    1b

/ allocate disk buffers; see H.0 for description

	mov    $bufp,r1
1:
	mov    r0,(r1)+
	add    $8,r0
	mov    r0,-2(r0)             / bus address
	mov    $-256.,-4(r0)         / word count
	add    $512.,r0              / buffer space
	cmp    r1,$bufp+nbuf+nbuf
	blo    1b
	mov    $sb0,(r1)+            / I/O queue entry drum
	mov    $sb1,(r1)+ / I/O queue entry disk (mounted device)
	mov    $swp,(r1)+ / I/O queue entry core image being swapped
	mov    $[systm-inode]\/2,sb0+4 / sets up initial buffers per
                                       / format given in
	mov    $systm,sb0+6 / memory map
	mov    $-512.,sb1+4
	mov    $_mount,sb1+6
	mov    $user,swp+6

/ set devices to interrupt

	mov    $100,*$lks / put 100 into clock status register;
                           / enables clock interrupt

/ set up time out subroutines

	mov    $touts,r0
	mov    $startty,(r0)+ / if toutt = 0 call startty
	mov    $pptito,(r0)+ / if toutt+1 = 0 call pptito
	tst    (r0)+ / add 2 to r0
	mov    $ntty-1,r1
1 :
	mov    $xmtto,(r0)+ / if toutt+2 thru toutt+2+ntty=0 call xmtto
	dec    r1
	bne    1b

/ free all character blocks; see H.0 for description

	mov    $510.,r2
	mov    $-1,r1
1:
	jsr    r0,put
	sub    $2,r2
	bgt    1b

/ set up drum swap addresses; see H.0 for description

	mov    $1024.-64.,r1 / highest drum address; high 64 blks allocated
                              / to UNIX
	mov    $p.dska,r2 / p.dska contains disk addresses for processes
1 :
	sub    $33.,r1 / 33 blocks per process, allows 16K per process
	mov    r1,(r2)+
	cmp    r2,$p.dska+nproc+nproc
	bne    1b

/ free rest of drum

	.if cold
	mov    $128.,systm / initialize word 1 of drum superblock image;
                           / number of bytes in free storage map=128.
	mov    $64.,systm+2+128. / init. wd 66. of superblock image; # of
                                 / bytes in i-node map=64.
1:
	dec    r1 / r1=687.,...,34.
	jsr    r0,free / free block 'r1', i.e., set bit 'r1' in free
                       / storage map in core
	cmp    r1,$34. / first drum address not in i list
	bgt    1b / if block 34 has been freed, zero i list

/ zero i list

1:
	dec    r1 / r1=33.,...,1
	jsr    r0,clear / zero block 'r1' on fixed head disk
	tst    r1
	bgt    1b / if blocks 33,...,1 have all been zeroed, done.
	.endif

/ make current program a user

	mov    $41.,r0 / rootdir set to 41 and never changed
	mov    r0,rootdir / rootdir is i-number of root directory
	mov    r0,u.cdir / u.cdir is i-number of process current directory
	mov    $1,r0
	movb   r0,u.uno / set process table index for this process to 1
	mov    r0,mpid / initialize mpid to 1
	mov    r0,p.pid / p.pid identifies process
	movb   r0,p.stat / process status = 1 i.e., active
                         /                = 0 free
	.if cold         /                = 2 waiting for a child to die
                         /                = 3 terminated but not yet waited
                         /                  for

/ initialize inodes for special files (inodes 1 to 40.)

	mov    $40.,r1 / set r1=i-node-number 40.
1:
	jsr    r0,iget / read i-node 'r1' from disk into inode area of
                       / core and write modified inode out (if any)
	mov    $100017,i.flgs / set flags in core image of inode to indi-
                              / cate allocated, read (owner, non-owner),
                              / write (owner, non-owner)
	movb   $1,i.nlks / set no. of links = 1
	movb   $1,i.uid / set user id of owner = 1
	jsr    r0,setimod / set imod=1 to indicate i-node modified, also
                          / stuff time of modification into i-node
	dec    r1 / next i-node no. = present i-node no.-1
	bgt    1b / has i-node 1 been initialized; no, branch

/ initialize i-nodes r1.,...,47. and write the root device, binary, etc.,
/ directories onto fixed head disk. user temporary, initialization prog.

	mov    $idata,r0 / r0=base addr. of assembled directories.
	mov    $u.off,u.fofp / pointer to u.off in u.fofp (holds file
                             / offset)
1:
	mov    (r0)+,r1/r1=41.,...,47; "0" in the assembled directory
                       / header signals last
	beq    1f      / assembled directory has been written onto drum
	jsr    r0,imap / locate the inode map bit for i-node 'r1'
	bisb   mq,(r2) / set the bit to indicate the i-node is not
                       / available
	jsr    r0,iget / read inode 'r1' from disk into inode area of
                       / core and write modified i-node on drum (if any)
	mov    (r0)+,i.flgs / set flags in core image of inode from
                            / assembled directories header
	movb   (r0)+,i.nlks / set no. of links from header
	movb   (r0)+,i.uid / set user id of owner from header
	jsr    r0,setimod / set imod=1 to indicate inode modified; also,
                          / stuff time of modification into i-node
	mov    (r0)+,u.count / set byte count for write call equal to
                             / size of directory
	mov    r0,u.base / set buffer address for write to top of directory
	clr    u.off / clear file offset used in 'seek' and 'tell'
	add    u.count,r0 / r0 points to the header of the next directory
	jsr    r0,writei / write the directory and i-node onto drum
	br     1b / do next directory
	.endif

/ next 2 instructions not executed during cold boot.
	bis    $2000,sb0 / sb0 I/O queue entry for superblock on drum;
                         / set bit 10 to 1
	jsr    r0,ppoke / read drum superblock
1:
	tstb   sb0+1 / has I/O request been honored (for drum)?
	bne    1b / no, continue to idle.
1:
	decb   sysflg / mormally sysflag=0, indicates executing in system
	sys    exec; 2f; 1f / generates trap interrupt; trap vector =
                            / sysent; 0
	br     panic / execute file/etc/init

1:
	2f;0
2:
	</etc/init\0> / UNIX looks for strings term, noted by nul\0

panic:
	clr    ps
1:
	dec    $0
	bne    1b
	dec    $5
	bne    1b
	jmp    *$173700 / rom loader address
rtssym:
	mov    r0,-(sp)
	mov    r1,-(sp)
	mov    4(sp),r0
	mov    -(r0),r0
	bic    $!7,r0
	asl    r0
	jmp    *1f(r0)
1:
	0f;1f;2f;3f;4f;5f;badrts;7f
0:
	mov    2(sp),r0
	br     1f
2:
	mov    r2,r1
	br     1f
3:
	mov    r3,r1
	br     1f
4:
	mov    r4,r1
	br     1f
5:
	mov     r5,r1
	br     1f
7:
	mov    8.(sp),r1
1:
	cmp    r1,$core
	blo    badrts
	cmp    r1,$ecore
	bhis   badrts
	bit    $1,r1
	bne    badrts
	tst    (r1)
	beq    badrts
	add    $1f,r0
	mov    r0,4(sp)
	mov    (sp)+,r1
	mov    (sp)+,r0
	rti
1:
	rts    r0
	rts    r1
	rts    r2
	rts    r3
	rts    r4
	rts    r5
	rts    sp
	rts    pc

badrts:
	mov    (sp)+,r1
	mov    (sp)+,r0
rpsym:
	jmp    unkni

	.if cold

idata:

/ root

	41.
	140016
	.byte 7,1
	9f-.-2
	41.
	<..\0\0\0\0\0\0>
	41.
	<.\0\0\0\0\0\0\0>
	42.
	<dev\0\0\0\0\0>
	43.
	<bin\0\0\0\0\0>
	44.
	<etc\0\0\0\0\0>
	45.
	<usr\0\0\0\0\0>
	46.
	<tmp\0\0\0\0\0>
9:

/ device directory

	42.
	140016
	.byte 2,1
	9f-.-2
	41.
	<..\0\0\0\0\0\0>
	42.
	<.\0\0\0\0\0\0\0>
	01.
	<tty\0\0\0\0\0>
	02.
	<ppt\0\0\0\0\0>
	03.
	<mem\0\0\0\0\0>
	04.
	<rf0\0\0\0\0\0>
	05.
	<rk0\0\0\0\0\0>
	06.
	<tap0\0\0\0\0>
	07.
	<tap1\0\0\0\0>
	08.
	<tap2\0\0\0\0>
	09.
	<tap3\0\0\0\0> 
	10.
	<tap4\0\0\0\0>
	11.
	<tap5\0\0\0\0>
	12.
	<tap6\0\0\0\0>
	13.
	<tap7\0\0\0\0>
	14.
	<tty0\0\0\0\0>
	15.
	<tty1\0\0\0\0>
	16.
	<tty2\0\0\0\0>
	17.
	<tty3\0\0\0\0>
	18.
	<tty4\0\0\0\0>
	19.
	<tty5\0\0\0\0>
	20.
	<tty6\0\0\0\0>
	21.
	<tty7\0\0\0\0>
	22.
	<lpr\0\0\0\0\0>
	01.
	<tty8\0\0\0\0> / really tty
9:

/ binary directory

	43.
	140016
	.byte 2,3
	9f-.-2
	41.
	<..\0\0\0\0\0\0>
	43.
  	<.\0\0\0\0\0\0\0>
9:

/ etcetra directory

	44.
	140016
	.byte 2,3
	9f-.-2
	41.
	<..\0\0\0\0\0\0>
	44.
	<.\0\0\0\0\0\0\0>
	47.
	<init\0\0\0\0>
9:

/ user directory

	45.
	140016
	.byte 2,1
	9f-.-2
	41.
	<..\0\0\0\0\0\0>
	45.
	<.\0\0\0\0\0\0\0>
9:

/ temporary directory

	46.
	140017
	.byte 2,1
	9f-.-2
	41.
	<..\0\0\0\0\0\0>
	46.
	<.\0\0\0\0\0\0\0>
9:

/ initialization program

	47.
	100036
	.byte 1,3
	9f-.-2
8:
	sys    break; 0
	sys    open; 6f-8b+core; 0
	mov    r0,r1
	sys    seek; 65.; 0
1:
	mov    r1,r0
	sys    read; 9f-8b+core; 512.
	mov    9f,r5            / size
	beq    1f
	sys    creat; 9f-8b+core+4; 0
	mov    r0,r2
	movb   9f+2,0f
	sys    chmod; 9f-8b+core+4; 0:..
	movb   9f+3,0f
	sys    chown; 9f-8b+core+4; 0:..
2:
	tst    r5
	beq    2f
	mov    r1,r0
	sys    read; 9f-8b+core; 512.
	mov    $512.,0f
	cmp    r5,$512.
	bhi    3f
	mov    r5,0f
3:
	mov    r2,r0
	sys    write; 9f-8b+core; 0:..
	sub    r0,r5
	br     2b
2:
	mov    r2,r0
	sys    close
	br     1b
1:
	mov    r1,r0
	sys    close
	sys    exec; 5f-8b+core; 4f-8b+core
	sys    exit
4:
	5f-8b+core; 0
5:
	</etc/init\0>
6:
	</dev/tap0\0>
	.even
9:

/ end of initialization data

	0

	.endif

