#
# RP06/RM03 disk boot program to load "/boot" from
# a UNIX filesystem (starting at block 1 on pack on
# drive 0) into low core and to execute that file.
# This program can only read regular small version 7 files
# from the root of a UNIX filesystem.
#
#
	.set	BLKSIZ,1024		# disc block size
	.set	RELOC,0x50000
	.set	HDRSIZ,040
	.set	INOSIZ,64		# no. bytes/inode entry
	.set	INOBLK,BLKSIZ/INOSIZ	# no. inodes/disc block
	.set	INOMSK,0xfffffff0	# changes with inode size
	.set	NAMSIZ,14		# no. bytes in name field of dir entry
	.set	ENTADR,024		# offset to entry addr in task header
	.set	DIRSIZ,16		# size of directory entry, bytes
	.set	ROOTINO,2		# root dir inode no.
	.set	NBOO,1
	.set	NSUP,1
	.set	SLASH,057		# '/'
# MBA registers
	.set	MBA0,0x20010000		# MBA 0 device reg's
	.set	M_cr,MBA0+4		# MBA control reg
	.set	M_var,MBA0+12		# MBA virt addr reg
	.set	M_bc,MBA0+16		# MBA byte count reg
	.set	M_map,MBA0+0x800	# start of MBA map reg's
	.set	MBAinit,1		# MBA init bit in MBA control reg
#
	.set	RM3SEC,32
	.set	RM3TRK,5
	.set	RP6typ,022
	.set	RM3typ,024
#
	.set	RP6TRK,19
	.set	RP6SEC,22
	.set	RP,MBA0+0x400		# start of RP06 drive 0  reg's
	.set	RP_cr,RP+0		# RP06 control reg, drive 0
	.set	RP_sr,RP+4		# status reg
	.set	RP_stk,RP+0x14		# desired track/sector reg
	.set	RP_dt,RP+0x18		# drive type reg
	.set	RP_off,RP+0x24		# RP offset reg
	.set	RP_cyl,RP+0x28  #  desired cyl reg
#  RP06 function codes, status bits 
	.set	RP_GO,1  #  GO bit
	.set	RP_RED,070  # read data
	.set	RP_DC,010  #  drive clear function code
	.set	RP_RIP,020  #  Read-In Preset function code
	.set	RP_FMT,0x1000  #  format bit for offset reg
	.set	RP_MOL,0x1000  #  medium online in status reg
	.set	RP_DRY,0200  #  drive ready, staus reg
	.set	RP_ERR,040000  #  composite error, staus reg
	.set	RP_pDRY,7  #  bit position of RP_DRY
	.set	RP_pERR,14  #  bit position of RP_ERR
#
#
init :
#  system initialization - executed once per boot load
	halt  #  entry mask required by DEC monitor software
	halt
	movl	$MBAinit,*$M_cr  #  MBA initialize
	movl	$RP_RIP+RP_GO,*$RP_cr  # read-in preset
	movl	$RP_FMT,*$RP_off  #  format bit in RP offset reg
#
	movl	*$RP_dt,r0
	cmpb	$RP6typ,r0
	bneq	rm3
	movl	$RP6SEC,*$rpsec
	movl	$RP6TRK,*$rptrk
	brb	domul
rm3:
	movl	$RM3SEC,*$rpsec
	movl	$RM3TRK,*$rptrk
domul:
	mull3	*$rpsec,*$rptrk,*$rpst
#  move boot image up to higher core
start :
	movl	$RELOC,sp
#  'uboot' must have no header
	movc3	$end,*$0,(sp)  #  move boot to relocated position
#
#  jump to re-located code at 'start2' - done once per boot load
	jmp	*$RELOC+start2
#
#  execution starts here only after boot has been re-located
start2 :
#
#  search inodes for pathname specified in 'names'
#
#  Must preserve r6  &  r7
#  'cmpc[35]' instruction uses reg's 0 - 3
#  'movc[35]' instruction uses reg's 0-5
	movl	$names+RELOC,r6  #  start of input filename
	movzbl	$ROOTINO,r0  #  start at root inode for search
nxti :
	clrw	*$bno  #  'bno' is index into 'iaddr[]' in inode entry
	bsbw	iget  #  get inode into core
	tstb	(r6)  #  r6 -> empty pathname component if end of pathname
	beql	getfil
get1b :
	bsbw	rmblk  #  read in 1 of blocks in 'addr[]' in inode entry
	beql start2  # file not found if zero cc set
#
	movl	$buf,r7  #  buffer holds directory block
nxtent :
	tstw	(r7)  #  null dir entry (inode # field = 0)
	beql	dirchk
	cmpc3	$NAMSIZ,(r6),2(r7)  #  compare 'names' against
#			dir entry - both null-filled
	bneq	dirchk  #  branch if dir entry is not the one
#  found component in a dir entry
	movzwl	(r7),r0  #  2-byte inode no. from 1st 2 bytes of dir entry
	addl2	$NAMSIZ,r6  #  point to next pathname component in 'names[]'
	brb	nxti  #  now go get inode whose no. is in r0
#
dirchk :		#  no pathname match
	acbl	$buf+BLKSIZ-1,$DIRSIZ,r7,nxtent  #  loop if
#			more dir entries in this buffer
	brb	get1b  #  get another dir block into buffer
#
#  here if inode for desired file has been read in
getfil :
	clrl	bufptr  #  new buffer ptr is low core
getlop :
	bsbb	rmblk  #  get a block in the file
	beql clear  #  branch if no more file blocks to read
#  skip above branch if more file blocks to read in
	addl2	$BLKSIZ,bufptr  #  next page in low core
	brb	getlop  #  go get next block of input file
#
#  clear core
#
clear :
	movl	*$size,r3
clrcor :
	clrq	(r3)
	acbl	$RELOC,$8,r3,clrcor
#
#  go execute file
#
	calls	$0,*$0
	brw	start2
#
#
#  subroutine to get the inode whose no. is in r0
#
iget :
	addl3	$(INOBLK*(NBOO+NSUP))-1,r0,r8  #  have to add in 2 blocks (boot and super)
#			worth of dummy inodes to calculate proper inode block
	divl3	$INOBLK,r8,r4  #  calculate inode block , put in r4
	bsbw	rblk  #  read in block containing desired inode
	bicl2	$INOMSK,r8
	mull2	$INOSIZ,r8  #  x inode size = offset in block
	addl2	$buf,r8  #  buffer loc of inode entry
#  move inode entry to separate buffer
	movc3	$time-inode,(r8),*$inode
	rsb
#
#  subr to read in 1 of blocks (bno) in 'addr[]'
#
rmblk :
	movzwl	*$bno,r0
	addw2	$3,*$bno  #  index into 'addr[]' array in inode entry
	addl2	$addr,r0
#  this boot assumes only small files(<11 blocks)
	extzv	$0,$24,(r0),r4  #  get low 2 bytes of block no. -> r4
	bneq	rblk  #  read in block if non-zero
#  zero cc is set on return
	rsb
#
#  subr to read disc block no. specified in r4 from RP06, drive 0
#
rblk :
	mull2	$BLKSIZ/512,r4
	clrl	r5
	ediv	*$rpst,r4,*$RP_cyl,r1
#  (r4 = block no.)/(no. blocks per cyl) -> cyl no.
#  r1 = remainder = block offset with in cylinder
	clrl	r2
	ediv	*$rpsec,r1,r1,r0
#  r1 = track no. ; ; sector no. in low part RP_stk
	insv	r1,$8,$5,r0  #  move track no. into RP_stk
	movl	r0,*$RP_stk  #  track-sector
	movl	$-BLKSIZ,*$M_bc  #  byte count
	ashl	$-9,bufptr,r0  #  start page no. of buffer
	bisl3	$0x80000000,r0,*$M_map  #  MBA map reg =
#			valid bit + phys page no.
	incl	r0
	bisl3	$0x80000000,r0,*$(M_map+4)
	clrl	*$M_var  #  buffer has been 512-byte aligned + map reg 0
	movl	$RP_RED+RP_GO,*$RP_cr  #  disc read function code
rprdy :
	movl	*$RP_sr,r0
	bbc	$RP_pDRY,r0,rprdy  #  loop unitl ready
	bbs	$RP_pERR,r0,rperr  #  branch if error
	bicpsw	$2  #  set zero cc for caller success
	rsb	#  normal return
  rperr :	#  disc i/o error
	halt		#  halt on error
#
#
bufptr :  .long  buf
names :
	.byte	'b,'o,'o,'t,0,0,0,0,0,0,0,0,0,0
	.byte  0
#
end:
#
	.set	buf,RELOC-1536
	.set	inode,RELOC-512
	.set	mode,inode
	.set	nlink,mode+2
	.set	uid,nlink+2
	.set	gid,uid+2
	.set	size,gid+2
	.set	addr,size+4
	.set	time,addr+40
	.set	bno,time+12
	.set	rptrk,bno+4
	.set	rpsec,rptrk+4
	.set	rpst,rpsec+4
