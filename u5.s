/ u5 -- unix

mget:
	mov	*u.fofp,mq / file offset in mq
	clr	ac / later to be high sig
	mov	$-8,lsh   / divide ac/mq by 256.
	mov	mq,r2
	bit	$10000,i.flgs / lg/sm is this a large or small file
	bne	4f / branch for large file
	bit	$!17,r2
	bne	3f / branch if r2 greater than or equal to 16
	bic	$!16,r2 / clear all bits but bits 1,2,3
	mov	i.dskp(r2),r1 / r1 has physical block number
	bne	2f / if physical block num is zero then need a new block
		   / for file
	jsr	r0,alloc / allocate a new block
	mov	r1,i.dskp(r2) / physical block number stored in i-node
	jsr	r0,setimod / set inode modified byte (imod)
	jsr	r0,clear / zero out disk/drum block just allocated
2:
	rts	r0
3: / adding on block which changes small file to a large file
	jsr	r0,alloc / allocate a new block for this file; block number
	                 /in r1
	jsr	r0,wslot / set up I/O buffer for write, r5 points to first
			 / data word in buffer
	mov	$8.,r3 / next 6 instructions transfer old physical block
		       / pointers
	mov	$i.dskp,r2 / into new indirect block for the new large file
1:
	mov	(r2),(r5)+
	clr	(r2)+
	dec	r3
	bgt	1b
	mov	$256.-8.,r3 / clear rest of data buffer
1:
	clr	(r5)+
	dec 	r3
	bgt	1b
	jsr	r0,dskwr / write new indirect block on disk
	mov	r1,i.dskp / put pointer to indirect block in i-node
	bis	$10000,i.flgs / set large file bit in i.flgs word of i-node
	jsr	r0,setimod / set i-node modified flag
	br	mget
4: / large file
	mov	$-8,lsh / divide byte number by 256.
	bic	$!776,r2 / zero all bits but 1,2,3,4,5,6,7,8; gives offset
			 / in indirect block
	mov	r2,-(sp) / save on stack
	mov	mq,r2 / calculate offset in i-node for pointer to proper
       		      / indirect block
	bic	$!16,r2
	mov	i.dskp(r2),r1
	bne	2f / if no indirect block exists
	jsr	r0,alloc / allocate a new block
	mov	r1,i.dskp(r2) / put block number of new block in i-node
	jsr	r0,setimod / set i-node modified byte
	jsr	r0,clear / clear new block
2:
	jsr	r0,dskrd / read in indirect block
	mov	(sp)+,r2 / get offset
	mov	r1,-(sp) / save block number of indirect block on stack
	add	r5,r2 / r5 points to first word in indirect block, r2
	              / points to location of inter
	mov	(r2),r1 / put physical block no of block in file
	                / sought in r1
	bne	2f / if no block exists
	jsr	r0,alloc / allocate a new block
	mov	r1,(r2) / put new block number into proper location in
	                / indirect block
	mov	(sp)+,r1 / get block number of indirect block
	mov	(r2),-(sp) / save block number of new block
	jsr	r0,wslot
	jsr	r0,dskwr / write newly modified indirect block back out
	                 / on disk
	mov	(sp),r1 / restore block number of new block
	jsr	r0,clear / clear new block
2:
	tst	(sp)+ / bump stack pointer
	rts	r0

alloc:
	mov	r2,-(sp) / save r2, r3 on stack
	mov	r3,-(sp)
	mov	$systm,r2 / start of inode and free storage map for drum
	tst	cdev
	beq	1f / drum is device
	mov	$_mount,r2 / disk or tape is device, start of inode and free
	                  / storage map
1:
	mov	(r2)+,r1 / first word contains number of bytes in free
			 / storage map
	asl	r1 / multiply r1 by eight gives, number of blocks in device
	asl	r1
	asl	r1
	mov	r1,-(sp) / save # of blocks in device on stack
	clr	r1 / r1 contains bit count of free storage map
1:
	mov	(r2)+,r3 / word of free storage map in r3
	bne	1f / branch if any free blocks in this word
	add	$16.,r1
	cmp	r1 ,(sp) / have we examined all free storage bytes
	blo	1b
	jmp	panic / found no free storage
1:
	asr	r3 / find a free block
	bcs	1f / branch when free block found; bit for block k is in
		   / byte k/8 / in bit k (mod 8)
	inc	r1 / increment bit count in bit k (mod8)
	br	1b
1:
	tst	(sp)+ / bump sp
	jsr	r0,3f / have found a free block
	bic	r3,(r2) / set bit for this block i.e. assign block
	br	2f

free:
	mov	r2,-(sp) / save r2, r3
	mov	r3,-(sp)
	jsr	r0,3f / set up bit mask and word no. in free storage map
		      / for block
	bis	r3,(r2) / set free storage block bit; indicates free block
2:
	mov	(sp)+,r3 / restore r2, r3
	mov	(sp)+,r2
	tst	cdev / cdev = 0, block structured, drum; cdev = 1
		     / mountable device
	bne	1f
	incb	smod / set super block modified for drum
	rts	r0
1:
	incb	mmod / set super block modified for mountable device
	rts	r0
3:
	mov	r1,r2 / block number, k, = 1
	bic	$!7,r2 / clear all bits but 0,1,2; r2 = (k) mod (8)
	clr	r3
	bisb	2f(r2),r3 / use mask to set bit in r3 corresponding to
			  / (k) mod 8
	mov	r1,r2 / divide block number by 16
	asr	r2
	asr	r2
	asr	r2
	asr	r2
	bcc	1f / branch if bit 3 in r1 was 0 i.e., bit for block is in
		   / lower half of word
	swab	r3 / swap bytes in r3; bit in upper half of word in free
		   / storage map
1:
	asl	r2 / multiply block number by 2; r2 = k/8
	add	$systm+2,r2 / address of word of free storage map for drum
	    		    / with block bit in it
	tst	cdev
	beq	1f / cdev = 0 indicates device is drum
	add	$_mount-systm,r2 / address of word of free storage map for
				/ mountable device with bit of block to be
				/ freed
1:
	rts	r0 / return to 'free'
2:
	.byte	1,2,4,10,20,40,100,200 / masks for bits 0,...,7

access:
	jsr	r0,iget / read in i-node for current directory (i-number
			/ passed in r1)
	mov	i.flgs,r2
	cmpb	i.uid,u.uid / is user same as owner of file
	bne	1f / no, then branch
	asrb	r2 / shift owner read write bits into non owner
		   / read/write bits
	asrb	r2
1:
	bit	r2,(r0)+ / test read-write flags against argument in
			 / access call
	bne	1f
	tstb	u.uid
	beq	1f
	jmp	error
1:
	rts	r0

setimod:
	movb	$1,imod / set current i-node modified bytes
	mov	s.time,i.mtim / put present time into file modified time
	mov	s.time+2,i.mtim+2
	rts	r0

imap: / get the byte that has the allocation bit for the i-number contained
      / in r1
	mov	$1,mq / put 1 in the mq
	mov	r1,r2 / r2 now has i-number whose byte in the map we
 		      / must find
	sub	$41.,r2 / r2 has i-41
	mov	r2,r3 / r3 has i-41
	bic	$!7,r3 / r3 has (i-41) mod 8 to get the bit position
	mov	r3,lsh / move the 1 over (i-41) mod 8 positions to the left
		       / to mask the correct bit
	asr	r2
	asr	r2
	asr	r2 / r2 has (i-41) base 8 of the byte no. from the start of
		   / the map
	mov	r2,-(sp) / put (i-41) base 8 on the stack
	mov	$systm,r2 / r2 points to the in-core image of the super
			  / block for drum
	tst	cdev / is the device the disk
	beq	1f / yes
	add	$_mount-systm,r2 / for mounted device, r2 points to 1st word
				/ of its super block
1:
	add	(r2)+,(sp) / get byte address of allocation bit
	add	(sp)+,r2 / ?
	add	$2,r2 / ?
	rts	r0

iget:
	cmp	r1,ii / r1 = i-number of current flle
	bne	1f
	cmp	idev,cdev / is device number of i-node = current device
	beq	2f
1:
	tstb	imod / has i-node of current file been modified i.e.,
	       	     / imod set
	beq	1f
	clrb	imod / if it has, we must write the new i-node out on disk
	mov	r1,-(sp)
	mov	cdev,-(sp)
	mov	ii,r1
	mov	idev,cdev
	jsr	r0,icalc; 1
	mov	(sp)+,cdev
	mov	(sp)+,r1
1:
	tst	r1 / is new i-number non zero
	beq	2f / branch if r1=0
	tst	cdev / is the current device number non zero (i.e., device
	       	     / =/ drum)
	bne	1f / branch 1f cdev =/ 0
	cmp	r1,mnti / mnti is the i-number of the cross devlce
			 / file (root directory of mounted devlce)
	bne	1f
	mov	mntd,cdev / make mounted device the current device
	mov	rootdir,r1
1:
	mov	r1,ii
	mov	cdev,idev
	jsr	r0,icalc; 0 / read in i-node ii
2:
	mov	ii,r1
	rts	r0

icalc: / i-node i is located in block (i+31.)/16. and begins 32.*
       / (i+31)mod16 bytes from its start
	add	$31.,r1 / add 31. to i-number
	mov	r1,-(sp) / save i+31. on stack
	asr	r1 / divide by 16.
	asr	r1
	asr	r1
	asr	r1 / r1 contains block number of block in which
		   / i-node exists
	jsr	r0,dskrd / read in block containing i-node i.
	tst	(r0)
	beq	1f / branch to wslot when argument in icalc call = 1
	jsr	r0,wslot / set up data buffer for write (will be same buffer
			 / as dskrd got)
1:
	bic	$!17,(sp) / zero all but last 4 bits; gives (i+31.) mod 16
	mov	(sp)+,mq / calculate offset in data buffer; 32.*(i+31.)mod16
	mov	$5,lsh / for i-node i.
	add	mq,r5 / r5 points to first word in i-node i.
	mov	$inode,r1 / inode is address of first word of current i-node
	mov	$16.,r3
	tst	(r0)+ / branch to 2 fwhen argument in icalc call = 0
	beq	2f / r0 now contains proper return address for rts r0
1:
	mov	(r1)+,(r5)+ / over write old i-node
	dec	r3
	bgt	1b
	jsr	r0,dskwr / write inode out on device
	rts	r0
2:
	mov	(r5)+,(r1)+ / read new i-node into "inode" area of core
	dec	r3
	bgt	2b
	rts	r0

itrunc:
	jsr	r0,iget
	mov	$i.dskp,r2 / address of block pointers in r2
1:
	mov	(r2)+,r1 / move physical block number into r1
	beq	5f
	mov	r2,-(sp)
	bit	$10000,i.flgs / test large file bit?
	beq	4f / if clear, branch
	mov	r1,-(sp) / save block number of indirect block
	jsr	r0,dskrd / read in block, 1st data word pointed to by r5
	mov	$256.,r3 / move word count into r3
2:
	mov	(r5)+,r1 / put 1st data word in r1; physical block number
	beq	3f / branch if zero
	mov	r3,-(sp) / save r3, r5 on stack
	mov	r5,-(sp)
	jsr	r0,free / free block in free storage map
	mov	(sp)+,r5
	mov	(sp)+,r3
3:
	dec	r3 / decrement word count
	bgt	2b / branch if positive
	mov	(sp)+,r1 / put physical block number of indirect block
4:
	jsr	r0,free / free indirect block
	mov	(sp)+,r2
5:
	cmp	r2,$i.dskp+16.
	bne	1b / branch until all i.dskp entries check
	bic	$10000,i.flgs / clear large file bit
	clr	i.size / zero file size
	jsr	r0,copyz; i.dskp; i.dskp+16. / zero block pointers
	jsr	r0,setimod / set i-node modified flag
	mov	ii,r1
	rts	r0


