/ u6 -- unix

readi:
	clr	u.nread / accumulates number of bytes transmitted
	tst	u.count / is number of bytes to be read greater than 0
	bgt	1f / yes, branch
	rts	r0 / no, nothing to read; return to caller
1:
	mov	r1,-(sp) / save i-number on stack
	cmp	r1,$40. / want to read a special file (i-nodes 1,...,40 are
		    / for special files)
	ble	1f / yes, branch
	jmp	dskr / no, jmp to dskr; read file with i-node number (r1)
		     / starting at byte ((u.fofp)), read in u.count bytes
1:
	asl	r1 / multiply inode number by 2
	jmp	*1f-2(r1)
1:
	rtty / tty; r1=2
	rppt / ppt; r1=4
	rmem / mem; r1=6
	rrf0 / rf0
	rrk0 / rk0
	rtap / tap0
	rtap / tap1
	rtap / tap2
	rtap / tap3
	rtap / tap4
	rtap / tap5
	rtap / tap6
	rtap / tap7
	rcvt / tty0
	rcvt / tty1
	rcvt / tty2
	rcvt / tty3
	rcvt / tty4
	rcvt / tty5
	rcvt / tty6
	rcvt / tty7
	rcrd/ crd

rtty: / read from console tty
	mov	tty+[8*ntty]-8+6,r5 / r5 is the address of the 4th word of
				    / of the control and status block
	tst	2(r5) / for the console tty; this word points to the console
		      / tty buffer
	bne	1f / 2nd word of console tty buffer contains number
	           / of chars. Is this number non-zero?
	jsr	r0,canon; ttych / if 0, call 'canon' to get a line
			    / (120 chars.)
1:
	tst	2(r5) / is the number of characters zero
	beq	ret1 / yes, return to caller via 'ret1'
	movb	*4(r5),r1 / no, put character in r1
	inc	4(r5) / 3rd word of console tty buffer points to byte which
		      / contains the next char.
	dec	2(r5) / decrement the character count
	jsr	r0,passc / move the character to core (user)
	br	1b / get next character

ret1:
	jmp	ret / return to caller via 'ret'

rppt: / read paper tape
	jsr	r0,pptic / gets next character in clist for ppt input and
			 / places
		br ret / it in r1; if there 1s no problem with reader, it
		       / also enables read bit in prs
	jsr	r0,passc / place character in users buffer area
	br	rppt

rmem: / transfer characters from memory to a user area of core
	mov	*u.fofp,r1 / save file offset which points to the char to
		           / be transferred to user
	inc	*u.fofp / increment file offset to point to 'next' char in
		        / memory file
	movb	(r1),r1 / get character from memory file, put it in r1
	jsr	r0,passc / move this character to the next byte of the
		         / users core area
	br	 rmem / continue
1:
rcrd:
	jmp	error / see 'error' routine

dskr:
	mov	(sp),r1 / i-number in r1
	jsr	r0,iget / get i-node (r1) into i-node section of core
	mov	i.size,r2 / file size in bytes in r2
	sub	*u.fofp,r2 / subtract file offset
	blos	ret
	cmp	r2,u.count / are enough bytes left in file to carry out read
	bhis	1f
	mov	r2,u.count / no, just read to end of file
1 :
	jsr	r0,mget / returns physical block number of block in file
		        / where offset points
	jsr	r0,dskrd / read in block, r5 points to 1st word of data in
		         / buffer
	jsr	r0,sioreg
2:
	movb	(r2)+,(r1)+ / move data from buffer into working core
		            / starting at u.base
	dec	r3
	bne	2b / branch until proper number of bytes are transferred
	tst	u.count / all bytes read off disk
	bne	dskr
	br	ret

passc:
	movb	r1,*u.base / move a character to the next byte of the
		           / users buffer
	inc	u.base / increment the pointer to point to the next byte
		       / in users buffer
	inc	u.nread / increment the number of bytes read
	dec	u.count / decrement the number of bytes to be read
	bne	1f / any more bytes to read?; yes, branch
	mov	(sp)+,r0 / no, do a non-local return to the caller of
		         / 'readi' by:
ret: / (1) pop the return address off the stack into r0
	mov	(sp)+,r1 / (2) pop the i-number off the stack into r1
1:
	clr	*$ps / clear processor status
	rts	r0 / return to address currently on top of stack

writei:
	clr	u.nread / clear the number of bytes transmitted during
		        / read or write calls
	tst	u.count / test the byte count specified by the user
	bgt	1f / any bytes to output; yes, branch
	rts	r0 / no, return - no writing to do
1:
	mov	r1 ,-(sp) / save the i-node number on the stack
	cmp	r1,$40. / does the i-node number indicate a special file?
	bgt	dskw / no, branch to standard file output
	asl	r1 / yes, calculate the index into the special file
	jmp	*1f-2(r1) / jump table and jump to the appropriate routine
1:
	wtty	/ tty
	wppt	/ ppt
	wmem	/ mem
	wrf0	/ rf0
	wrk0	/ rk0
	wtap	/ tap0
	wtap	/ tap1
	wtap	/ tap2
	wtap	/ tap3
	wtap	/ tap4
	wtap	/ tap5
	wtap	/ tap6
	wtap	/ tap7
	xmtt	/ tty0
	xmtt	/ tty1
	xmtt	/ tty2
	xmtt	/ tty3
	xmtt	/ tty4
	xmtt	/ tty5
	xmtt	/ tty6
	xmtt	/ tty7
/	w1pr / lpr

wtty:
	jsr	r0,cpass / get next character from user buffer area; if
		         / none go to return address in syswrite
	tst	r1 / is character = null
	beq	wtty / yes, get next character
1 :
	mov	$240,*$ps / no, set processor priority to five
	cmpb	cc+1,$20. / is character count for console tty greater
		          / than 20
	bhis	2f / yes; branch to put process to sleep
	jsr	r0,putc; 1 / find place in freelist to assign to console
		           / tty and
		br 2f / place character in list; if none available
		      / branch to put process to sleep
	jsr	r0,startty / attempt to output character on tty
	br	wtty
2:
	mov	r1,-(sp) / place character on stack
	jsr	r0,sleep; 1 / put process to sleep
	mov	(sp)+,r1 / remove character from stack
	br	1b / try again to place character in clist and output

wppt:
	jsr	r0,cpass / get next character from user buffer area,
		         / if none return to writei's calling routine
	jsr	r0,pptoc / output character on ppt
	br	wppt
/wlpr:
/	jsr	r0,cpass
/	cmp	r0,$'a
/	blo	1f
/	cmp	r1,$'z
/	bhi	1f
/	sub	$40,r1
/1:
/	jsr	r0,lptoc
/	br	wlpr

wmem: / transfer characters from a user area of core to memory file
	jsr	r0,cpass / get next character from users area of core and
		         / put it in r1
	mov	r1,-(sp) / put character on the stack
	mov	*u.fofp,r1 / save file offset in r1
	inc	*u.fofp / increment file offset to point to next available
		        / location in file
	movb	(sp)+,(r1) / pop char off stack, put in memory loc assigned
		           / to it
	br	wmem / continue
1:
	jmp	error / ?

dskw: / write routine for non-special files
	mov	(sp),r1 / get an i-node number from the stack into r1
	jsr	r0,iget / write i-node out (if modified), read i-node 'r1'
		        / into i-node area of core
	mov	 *u.fofp,r2 / put the file offset [(u.off) or the offset in
		            / the fsp entry for this file] in r2
	add	 u.count,r2 / no. of bytes to be written + file offset is
		            / put in r2
	cmp	 r2,i.size / is this greater than the present size of
		           / the file?
	blos	 1f / no, branch
	 mov	r2,i.size / yes, increase the f11e size to file offset +
		           / no. of data bytes
	 jsr	r0,setimod / set imod=1 (i.e., core inode has been
		           / modified), stuff tlme of modification into
		           / core image of i-node
1:
	jsr	r0,mget / get the block no. in which to write the next data
		        / byte
	bit	*u.fofp,$777 / test the lower 9 bits of the file offset
	bne	2f / if its non-zero, branch; if zero, file offset = 0,
		   / 512, 1024,...(i.e., start of new block)
	cmp	u.count,$512. / if zero, is there enough data to fill an
		              / entire block? (i.e., no. of
	bhis	3f / bytes to be written greater than 512.? Yes, branch.
		   / Don't have to read block
2: / in as no past info. is to be saved (the entire block will be
   / overwritten).
	jsr	r0,dskrd / no, must retain old info.. Hence, read block 'r1'
		         / into an I/O buffer
3:
	jsr	r0,wslot / set write and inhibit bits in I/O queue, proc.
		         / status=0, r5 points to 1st word of data
	jsr	r0,sioreg / r3 = no. of bytes of data, r1 = address of data,
		          / r2 points to location in buffer in which to
		          / start writing data
2:
	movb	(r1 )+,(r2)+ / transfer a byte of data to the I/O buffer
	dec	r3 / decrement no. of bytes to be written
	bne	2b / have all bytes been transferred? No, branch
	jsr	r0,dskwr / yes, write the block and the i-node
	tst	u.count / any more data to write?
	bne	1b / yes, branch
	jmp	ret / no, return to the caller via 'ret'

cpass: / get next character from user area of core and put it in r1
	tst	u.count / have all the characters been transferred (i.e.,
		        / u.count, # of chars. left
	beq	1f / to be transferred = 0?) yes, branch
	dec	u.count / no, decrement u.count
	movb	*u.base,r1 / take the character pointed to by u.base and
		           / put it in r1
	inc	u.nread / increment no. of bytes transferred
	inc	u.base / increment the buffer address to point to the
	rts	r0 / next byte
1: 
	mov	(sp)+,r0 / put return address of calling routine into r0
	mov	(sp)+,r1 / i-number in r1
	rts	r0 / non-local return

sioreg:
	mov	*u.fofp,r2 / file offset (in bytes) is moved to r2
	mov	r2,r3 / and also to r3
	bis	$177000,r3 / set bits 9,...,15. of file offset in r3
	bic	$!777,r2 / calculate file offset mod 512.
	add	r5,r2 / r2 now points to 1st byte in system buffer where
		      / data is to be placed
	mov	u.base,r1 / address of data is in r1
	neg	r3 / 512 - file offset (mod512.) in r3 (i.e., the number
		   / of free bytes in the file block
	cmp	r3,u.count / compare this with the number of data bytes to
		           / be written to the file
	blos	2f / if less than branch. Use the number of free bytes
		   / in the file block as the number to be written
	mov	u.count,r3 / if greater than, use the number of data bytes
		           / as the number to be written
2:
	add	r3,u.nread / r3 + number of bytes xmitted during write is
		           / put into u.nread
	sub	r3,u.count / u.count = no. of bytes that still must be
		           / written or read
	add	r3,u.base /  u.base points to the 1st of the remaining data
		          / bytes
	add	r3,*u.fofp / new file offset = number of bytes done + old
		           / file offset
	rts	r0

