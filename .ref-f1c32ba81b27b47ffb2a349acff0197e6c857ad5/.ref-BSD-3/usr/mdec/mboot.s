#
# Tape boot program to load and transfer
#   to a program on a 9-tr mag tape in 
#    UNIX 'tp' format on the VAX machine.
#  This program is the boot block on the mag tape.
#
#  Entry is made to the desired file via 'calls $0,entrypt'
#    so return can be 'ret'.
#
	.set	core,50  #  assume 50 K words avail.
	.set	BOOBLK,1 #  no. tape blocks for boot
	.set	RELOC,0x50000
	.set	HDRSIZ,040  #  size of file header for VAX
	.set	MAGIC,0410  #  file type id in header
	.set	ENTSIZ,64  #  size of 1 TP dir entry, bytes
	.set	PTHSIZ,32  #  size of TP path name, bytes
	.set	BLKSIZ,512  #  tape block size, bytes
	.set	NUMDIR,24  #  no. of dir blocks on tape
	.set	ENTBLK,8  #  no. of dir entries per tape block
	.set	NL,012  #  new-line char
	.set	CR,015  # carriage-return char
	.set	MAXC,65535  #  max. count for 'movc' op
	.set	TSIZ,4		# text size
	.set	DSIZ,8		# data size
	.set	BSIZ,12		# bss size
	.set	TENT,024  #  task header entry loc
#
	.set	RXCS,32
	.set	RXDB,33
	.set	TXCS,34
	.set	TXDB,35
	.set	RXCS_DONE,0x80
	.set	TXCS_RDY,0x80
	.set	TXCS_pr,7  #  bit position of TXCS ready bit
	.set	RXCS_pd,7  #  bit position of RXCS done bit
#
	.set	FILSIZ,38  #  dir offset for file size
	.set	BNUM,44  #  dir offset for start block no.
#
#  MBA registers
	.set	MBA1,0x20012000  #  MBA 1 device reg's
	.set	M_cfg,0  # MBA config reg
	.set	M_cr,4  #  MBA control reg
	.set	M_stat,8  #  MBA status reg
	.set	M_var,12  #  MBA virt addr reg
	.set	M_bc,16  #  MBA byte count reg
	.set	M_map,0x800  #  start of MBA map reg's
	.set	MBAinit,1  #  MBA init bit in MBA control reg
#
#  TM02  registers
	.set	TM02,MBA1+0x400   #  start of TM02 reg's
	.set	TM_cs1,0  #  TM02 control 1 reg
	.set	TM_ds,4  #  status reg
	.set	TM_er,8  #  error reg
	.set	TM_as,16  #  attention summary
	.set	TM_fc,20  #  frame count
	.set	TM_tc,36  #  TM02 tape control
#  Function codes for TM02 control reg 1
	.set	GO,1  #  GO bit
	.set	RWND,6  # rewind, on-line
	.set	DCLR,010  #  drive clear
	.set	SFWD,030  #  space forward
	.set	SREV,032  # space reverse
	.set	WRIF,060  # write forward
	.set	REDF,070  #  read forward
#  misc bits
	.set	ERR,040000  #  composite error bit in status reg
	.set	TCHAR,012300  #  drive 0, odd parity, PDP11,
#				1600 BPI NRZ, abort on error - for
#				tape control reg
	.set	DRDY,0200  #  TM02/drive ready in status reg
	.set	TM_pd,7  #  bit position of TM DRDY bit
	.set	TM_pe,14  #  bit position of TM ERROR bit
#
	.set	tapa,-4		# desired tape addr
	.set	mtapa,-8	# current tape addr
	.set	name,-8-PTHSIZ	# operator-typed file name
#
	.set	rMBA,r10
	.set	rTM,r11
#  system initialization
init:
	movl	$RELOC,fp	# core loc to which to move this program
	addl3	$name,fp,sp	# set stack pointer, leaving room for locals
	clrl	r0
	cmpl	(r0),$MAGIC
	bneq	rel
	movl	$HDRSIZ,r0	#  skip header
rel :
	movc3	$end,(r0),(fp)  #  move boot up to relocated position
	jmp	start+RELOC  #  jump to relocated code
#
start :
	movab	*$MBA1,%rMBA
	movl	$MBAinit,M_cr(%rMBA)  #  MBA initialize
	movab	*$TM02,%rTM
	movl	$TCHAR,TM_tc(%rTM)  #  drive no., etc.
	movl	$DCLR+GO,TM_cs1(%rTM)  #  drive clear
	bsbw	rew	# rewind input tape
	movab	name(fp),r4  #  start of filename storage
	movzbl	$'=,r0	#  prompt character
	bsbw	putc	# output char to main console
#
gfil :
	movl	r4,r1	# loc at which to store user-specified file name
nxtc :
	bsbw	getc	# get input char's in file name
	cmpb	r0,$NL	#  terminator ?
	beql	nullc
	cmpb	r0,$'@	# r0 = return character
	beql	gfil
	movb	r0,(r1)+
	cmpb	r0,$'#
	bneq	nxtc
	tstw	-(r1)	# subl2	$2,r1
	cmpl	r1,r4
	blss	gfil
	brb	nxtc
nullc:
	subl3	r4,r1,r9	# size of path name
	beql	start		# dumb operator
#
#  user-specified TP filename has been stored at name(fp)
#
#  read in entire tp directory contents into low core
#  'cmpc[35]' instruction uses reg's 0-3
#
dirred :
	movl	$BOOBLK,tapa(fp)	# desired tape block no.
	movl	$(NUMDIR*BLKSIZ),r6	# no. bytes in total dir
	bsbw	taper	# read no. bytes indicated
#
#  search entire directory for user-specified file name
	clrl	r5	# dir buff loc = 0
nxtdir :
	cmpc3	r9,(r5),(r4)  #  see if dir entry matches filename
	beql	fndfil	# found match
	acbl	$NUMDIR*BLKSIZ-1,$ENTSIZ,r5,nxtdir  #  see if done with tp dir
#
	brb		start	# entry not in directory - start all over
#
#  found desired tp dir entry
fndfil :
	movzwl	BNUM(r5),tapa(fp)	#  start block no., 2 bytes
	movzwl	FILSIZ(r5),r6  #  low 2 bytes file size
	insv	FILSIZ-1(r5),$16,$8,r6  #  file size, high byte
	cmpl	r6,$RELOC-512  #  check if file fits below stack
	blss	filok  #  file o.k.
	brw		start  #  file too large
#
#  time to read in desired file from tape
filok :
	movl	r6,r7  #  save r6
	bsbb	taper
	bsbw	rew
#
#  if load module header exists, move down rest of load
#    module to 0
	clrl	r5  #  entry loc 
	cmpl	(r5),$MAGIC		# unix a.out?
	bneq	clrcor
#  move start of exec image down to 0
	clrl	r0
	movq	TSIZ(r0),r1  #  text size and data size
	movq	BSIZ(r0),r3  #  bss size, symbol table size
	movl	TENT(r0),r5  #  entry loc
	movl	r7,r6  # restore file size
movtxt:
	movb	HDRSIZ(r0),(r0)+
	sobgtr	r6,movtxt
#  move data up to page boundary
	extzv	$0,$9,r1,r0		# byte-in-page of last text byte
	beql	clrcor
	subl3	r0,$512,r0		# room needed
	addl2	r2,r1			# current end+1 of data
	addl2	r1,r0			# new end+1 of data
	movl	r0,r7			# start here on bssz clearing
movdat:
	movb	-(r1),-(r0)
	sobgtr	r2,movdat
#
#  clear core up to stack
clrcor :
	subl3	r7,$RELOC-4,r0  #  no. bytes to clear
clrit :
	clrb	(r7)+
	sobgtr	r0,clrit
#
#  time to jump to start of file & execute
	calls	$0,(r5)
	brw	start	# returned from execution - start all over
#
#
#  taper: movcTAPE (r6),tapa(fp),0
#
rew2 :
	bsbb	rew		# beginning of tape
taper0:
	bsbb	rrec	# advance 1 block; (we never want block 0)
taper :
	clrl	r0  #  SBI page no.
	cmpl	mtapa(fp),tapa(fp)	# current position .vs. desired
	bgtr	rew2
	blss	taper0
rloop :
	bsbb	rrec
	acbl	$1,$-BLKSIZ,r6,rloop
	rsb
#
#  subr to read 1 block from mag tape into page (r0)
#
rrec :
	movl	TM_ds(%rTM),r2
	bbc	$TM_pd,r2,rrec  #  TM02 & drive ready ?
	movl	$-BLKSIZ,M_bc(%rMBA)  #  byte count
	bisl3	$0x80000000,r0,M_map(%rMBA)  #  MBA map entry = valid bit + phys page no.
	clrl	M_var(%rMBA)  #  MBA virt addr reg = map reg no. + byte offset
	movl	$REDF+GO,TM_cs1(%rTM)  #  read forward
tmrdy :
	movl	TM_ds(%rTM),r2
	bbc	$TM_pd,r2,tmrdy  #  TM02 & device ready ?
	movl	TM_er(%rTM),r2
	bbc	$TM_pe,r2,donred  #  any read errors ?
	clrl	TM_ds(%rTM)  # clear status - try to recover
	mnegl	$1,TM_fc(%rTM)  # frame count for backspace
	movl	$SREV+GO,TM_cs1(%rTM)  #  space reverse
	brb	rrec
donred :
	incl	r0  #  next SBI page no.
	incl	mtapa(fp)	# mag tape block position
	rsb
#
#  subr to rewind mag tape
#
rew :
	movl	$RWND+GO,TM_cs1(%rTM)  # rewind
	clrl	mtapa(fp)  #  current position
	rsb
#
#  subroutines to do char i-o with main tty console
#
getc :
	mfpr	$RXCS,r0
	bbc	$RXCS_pd,r0,getc  #  receiver ready ?
	mfpr	$RXDB,r0
	cmpb	r0,$CR
	bneq	putc
	movb	$NL,r0
#
putc:
	mfpr	$TXCS,r2
	bbc	$TXCS_pr,r2,putc  #  transmitter ready ?
	cmpb	r0,$NL
	bneq	wbyt
	movb	$CR,r0
	bsbb	putc
	clrb	r0
	bsbb	putc
	movb	$NL,r0
wbyt :
	extzv	$0,$7,r0,r0
	mtpr	r0,$TXDB
	rsb
end :
