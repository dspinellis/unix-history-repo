/ u2 -- unix

syslink: / name1, name2
	jsr	r0,arg2 / u.namep has 1st arg u.off has 2nd
	jsr	r0,namei / find the i-number associated with the 1st 
                         / path name
	br	error9 / cannot be found
	jsr	r0,iget / get the i-node into core
	mov	(sp)+,u.namep / u.namep points to 2nd name
	mov	r1,-(sp) / put i-number of name1 on the stack (a link
			 / to this file is to be created)
	mov	cdev,-(sp) / put i-nodes device on the stack
	jsr	r0,isdir / is it a directory
	jsr	r0,namei / no, get i-number of name2
		br .+4   / not found so r1-i-number of current directory
			 /              ii = i-number of current directory
	br	error9 / file already exists., error
	cmp	(sp)+,cdev / u.dirp now points to end of current directory
	bne	error9
	mov	(sp),u.dirbuf / i-number of name1 into u.dirbuf
	jsr	r0,mkdir / make directory entry for name2 in current 
                         /directory
	mov	(sp)+,r1 / r1 has i-number of name1
	jsr	r0,iget / get i-node into core
	incb	i.nlks / add 1 to its number of links
	jsr	r0,setimod / set the i-node modified flag
	
sysret9:
	jmp	sysret / see 'sysret' routine
error9:
	jmp	error / see 'error' routine

isdir: / if the i-node whose i-number is in r1 is a directory there is an
       / error unless super user made the call
	tstb	u.uid / super user
	beq	1f / yes, don't care
	mov	ii,-(sp) / put current i-number on stack
	jsr	r0,iget / get i-node into core (i-number in r1)
	bit	$40000,i.flgs / is it a directory
	bne	error9 / yes, error
	mov	(sp)+,r1 / no, put current i-number in r1 (ii)
	jsr	r0,iget / get it back in
1:
	rts	r0

sysunlink: / name - remove link name
	jsr	r0,arg; u.namep / u.namep points to name
	jsr	r0,namei / find the i-number associated with the path name
		br error9 / not found
	mov	r1,-(sp) / put its i-number on the stack
	jsr	r0,isdir / is it a directory
	clr	u.dirbuf / no, clear the location that will get written
	                / into the i-number portion of the entry
	sub	$10.,u.off / move u.off back 1 directory entry
	jsr	r0,wdir / free the directory entry
	mov	(sp)+,r1 / get i-number back
	jsr	r0,iget / get i-node
	jsr	r0,setimod / set modified flag
	decb	i.nlks / decrement the number of links
	bgt	sysret9 / if this was not the last link to file return
	jsr	r0,anyi / if it was, see if anyone has it open.  Then
			/ free contents of file and destroy it.
	br	sysret9

mkdir:
	jsr	r0,copyz; u.dirbuf+2; u.dirbuf+10. / clear this
	mov	u.namep,r2 / r2 points to name of directory entry
	mov	$u.dirbuf+2,r3 / r3 points to u.dirbuf+2
1: / put characters in the directory name in u.dirbuf+2 - u.dirbuf+10
	movb	 (r2)+,r1 / move character in name to r1
	beq	1f / if null, done
	cmp	r1,$'/ / is it a "/"?
	beq	error9 / yes, error
	cmp	r3,$u.dirbuf+10. / have we reached the last slot for
				 / a char?
	beq	1b / yes, go back
	movb	r1,(r3)+ / no, put the char in the u.dirbuf
	br	1b / get next char
1:
	mov	u.dirp,u.off / pointer to empty current directory slot to 
			     /u.off
			     
wdir:
	mov	$u.dirbuf,u.base / u.base points to created file name
	mov	$10.,u.count / u.count = 10
	mov	ii,r1 / r1 has i-number of current directory
	jsr	r0,access; 1 / get i-node and set its file up for writing
	jsr	r0,writei / write into directory
	rts	r0
	
sysexec:
	jsr	r0,arg2 / arg0 in u.namep,arg1 on top of stack
	jsr	r0,namei / namei returns i-number of file named in 
			 / sysexec call in r1
		br error9
	jsr	r0,iget / get i-node for file to be executed
	bit	$20,i.flgs / is file executable
	beq	error9
	jsr	r0,iopen / gets i-node for file with i-number given in 
			 / r1 (opens file)
	bit	$40,i.flgs / test user id on execution bit
	beq	1f
	tstb	u.uid / test user id
	beq	1f / super user
	movb	i.uid,u.uid / put user id of owner of file as process 
			    / user id
1:
	mov	(sp)+,r5 / r5 now contains address of list of pointers to 
			 / arguments to be passed
	mov	$1,u.quit / u.quit determines handling of quits;
			  / u.quit = 1 take quit
	mov	$1,u.intr / u.intr determines handling of interrupts;
			  / u.intr = 1 take interrupt
	mov	$rtssym,30 / emt trap vector set to take system routine
	mov	$fpsym,*10 / reserved instruction trap vector set to take 
			   / system routine
	mov	$sstack,sp / stack space used during swapping
	mov	r5,-(sp) / save arguments pointer on stack
	mov	$ecore,r5 / r5 has end of core
	mov	$core,r4 / r4 has start of users core
	mov	r4,u.base / u.base has start of users core
	mov	(sp),r2 / move arguments list pointer into r2
1:
	tst	(r2)+ / argument char = "nul"
	bne	1b
	tst	-(r2) / decrement r2 by 2; r2 has addr of end of arguent 
		      / pointer list
1: / move arguments to bottom of users core
	mov	-(r2),r3 / (r3) last non zero argument ptr
	cmp	r2,(sp) / is r2 = beginning of argument ptr list
	blo	1f / branch to 1f when all arguments are moved
2:
	tstb	(r3)+
	bne	2b / scan argument for \0 (nul)
2:
	movb	-(r3),-(r5) / move argument char by char starting at 
			    / "ecore"
	cmp	r3,(r2) / moved all characters in this argument
	bhi	2b / branch 2b if not
	mov	r5,(r4)+ / move r5 into top of users core; r5 has 
			 / pointer to nth arg
	br	1b / string
1:
	clrb	-(r5)
	bic	$1,r5 / make r5 even, r5 points to last word of argument 
		      / strings
	mov	$core,r2
1: / move argument pointers into core following argument strings
	cmp	r2,r4
	bhis	1f / branch to 1f when all pointers are moved
	mov	(r2)+,-(r5)
	br	1b
1:
	sub	$core,r4 / gives number of arguments *2
	asr	r4 / divide r4 by 2 to calculate the number of args stored
	mov	r4,-(r5) / save number of arguments ahead of the argument 
			 / pointers
	clr	-(r5) / popped into ps when rti in sysrele is executed
	mov	$core,-(r5) / popped into pc when rti in sysrele 
			    / is executed
	mov	r5,0f / load second copyz argument
	tst	-(r5) / decrement r5
	mov	r5,u.r0 /
	sub	$16.,r5 / skip 8 words
	mov	r5,u.sp / assign user stack pointer value, effectively
		        / zeroes all regs when sysrele is executed
	jsr	r0,copyz; core; 0:0 / zero user's core
	clr	u.break
	mov	r5,sp / point sp to user's stack
	mov	$14,u.count
	mov	$u.off,u.fofp
	clr	u.off / set offset in file to be read to zero
	jsr	r0,readi / read in first six words of user's file, starting 
			 / at $core
	mov	sp,r5 / put users stack address in r5
	sub	$core+40.,r5 / subtract $core +40, from r5 (leaves
			     / number of words less 26 available for
			     / program in user core
	mov	r5,u.count /

	/ 0407 binary support added may 2008.
	br	1f
bsz:	0	/ XXX is there a reg that I can use over a call to readi?
1:
	cmp	core,$407
	bne	e407
	add	$4,u.off / skip last two header words
	mov	$core,r4
	mov	r4,u.base / continue reading at core.
	mov	core+2,r5
	add	core+4,r5 / r5 = text+data size
	mov	core+6,bsz / save bss size, we're going to overwrite core
	/ XXX fix me, I dont quite understand what to do here or
	/ what is done in the similar code below e407:
	/ cmp	r5, u.count / see if theres enough room
	/ bgt	1f
	mov	r5,u.count / read text+data into core
	jsr	r0,readi
	mov	u.nread,u.break / break = core + nread + bss
	add	$core,u.break
	add	bsz,u.break
	jsr	r0,iclose
	br	sysret3
e407:
	/ end 0407 support

	cmp	core,$405 / br .+14 is first instruction if file is
			  / standard a.out format
	bne	1f / branch, if not standard format
	mov	core+2,r5 / put 2nd word of users program in r5; number of
			  / bytes in program text
	sub	$14,r5 / subtract 12
	cmp	r5,u.count /
	bgt	1f / branch if r5 greater than u.count
	mov	r5,u.count
	jsr	r0,readi / read in rest of user's program text
	add	core+10,u.nread / add size of user data area to u.nread
	br	2f
1:
	jsr	r0,readi / read in rest of file
2:
	mov	u.nread,u.break / set users program break to end of 
				/ user code
	add	$core+14,u.break / plus data area
	jsr	r0,iclose / does nothing
	br	sysret3 / return to core image at $core
	
sysfstat: / set status of open file
	jsr	r0,arg; u.off / put buffer address in u.off
	mov	u.off,-(sp) / put buffer address on the stack
	mov	*u.r0,r1 / put file descriptor in r1
	jsr	r0,getf / get the files i-number
	tst	r1 / is it 0?
	beq	error3 / yes, error
	bgt	1f / if i-number is negative (open for writing)
	neg	r1 / make it positive, then branch
	br	1f / to 1f
	
sysstat: / ; name of file; buffer - get files status
	jsr	r0,arg2 / get the 2 arguments
	jsr	r0,namei / get the i-number for the file
		br error3 / no such file, error
1:
	jsr	r0,iget / get the i-node into core
	mov	(sp)+,r3 / move u.off to r3 (points to buffer)
	mov	r1,(r3)+ / put i-number in 1st word of buffer
	mov	$inode,r2 / r2 points to i-node
1:
	mov	(r2)+,(r3)+ / move rest of i-node to buffer
	cmp	r2,$inode+32 / done?
	bne	1b / no, go back
	br	sysret3 / return through sysret
	
error3:
	jmp	error / see 'error' routine
sysret3:
	jmp	sysret / see 'sysret' routine

getf: / get the device number and the i-number of an open file
	cmp	r1,$10. / user limited to 10 open files
	bhis	error3 / u.fp is table of users open files, index in 
		       / fsp table
	movb	u.fp(r1),r1 / r1 contains number of entry in fsp table
	beq	1f / if its zero return
	asl	r1
	asl	r1 / multiply by 8 to get index into fsp table entry
	asl	r1
	add	$fsp-4,r1 / r1 is pointing at the 3rd word in the fsp entry
	mov	r1,u.fofp / save address of 3rd word in fsp entry in u.fofp
	mov	-(r1),cdev / remove the device number  cdev
	mov	-(r1),r1 / and the i-number  r1
1:
	rts	r0

namei:
	mov	u.cdir,r1 / put the i-number of current directory in r1
	mov	u.cdev,cdev / device number for users directory into cdev
	cmpb	*u.namep,$'/ / is first char in file name a /
	bne	1f
	inc	u.namep / go to next char
	mov	rootdir,r1 / put i-number of rootdirectory in r1
	clr	cdev / clear device number
1:
	tstb	*u.namep / is the character in file name a nul
	beq	nig / yes, end of file name reached; branch to "nig"
1:
	jsr	r0,access; 2 / get i-node with i-number r1
	bit	$40000,i.flgs / directory i-node?
	beq	error3 / no, got an error
	mov	i.size,u.dirp / put size of directory in u.dirp
	clr	u.off / u.off is file offset used by user
	mov	$u.off,u.fofp / u.fofp is a pointer to the offset portion 
			      / of fsp entry
2:
	mov	$u.dirbuf,u.base / u.dirbuf holds a file name copied from 
				 / a directory
	mov	$10.,u.count / u.count is byte count for reads and writes
	jsr	r0,readi / read 10. bytes of file with i-number (r1);
			 / i.e. read a directory entry
	tst	u.nread
	ble	nib / gives error return
	tst	u.dirbuf /
	bne	3f / branch when active directory entry (i-node word in 
		   / entry non zero)
	mov	u.off,u.dirp
	sub	$10.,u.dirp
	br	2b
3:
	mov	u.namep,r2 / u.namep points into a file name string
	mov	$u.dirbuf+2,r3 / points to file name of directory entry
3:
	movb	(r2)+,r4 / move a character from u.namep string into r4
	beq	3f / if char is nul, then the last char in string has been 
		   / moved
	cmp	r4,$'/ / is char a </>
	beq	3f
	cmp	r3,$u.dirbuf+10. / have I checked all 8 bytes of file name
	beq	3b
	cmpb	(r3)+,r4 / compare char in u.namep string to file name 
			 / char read from
	beq	3b / directory; branch if chars match
	br	2b / file names do not match go to next directory entry
3:
	cmp	r3,$u.dirbuf+10. / if equal all 8 bytes were matched
	beq	3f
	tstb	(r3)+ /
	bne	2b
3:
	mov	r2,u.namep / u.namep points to char following a / or nul
	mov	u.dirbuf,r1 / move i-node number in directory entry to r1
	tst	r4 / if r4 = 0 the end of file name reached, if r4 = </>
		   / then go to next directory
	bne	1b

nig:
	tst	(r0)+ / gives non-error return
nib:
	rts	r0

syschdir: / makes the directory specified in the argument the current 
	  / directory
	jsr	r0,arg; u.namep / u.namep points to path name
	jsr	r0,namei / find its i-number
		br error3
	jsr	r0,access; 2 / get i-node into core
	bit	$40000,i.flgs / is it a directory?
	beq	error3 / no error
	mov	r1,u.cdir / move i-number to users current directory
	mov	cdev,u.cdev / move its device to users current device
	br	sysret3
	
isown:
	jsr	r0,arg2 / u.namep points to file name
	jsr	r0,namei / get its i-number
		br error3
	jsr	r0,iget / get i-node into core
	tstb	u.uid / super user?
	beq	1f / yes, branch
	cmpb	i.uid,u.uid / no, is this the owner of the file
	beq	1f / yes
	jmp	error3 / no, error
1:
	jsr	r0,setimod / indicates i-node has been modified
	mov	(sp)+,r2 / mode is put in r2 (u.off put on stack with 
			 / 2nd arg)
	rts	r0

syschmod: / name; mode
	jsr	r0,isown / get the i-node and check user status
	bit	$40000,i.flgs / directory?
	beq	2f / no
	bic	$60,r2 / su & ex / yes, clear set user id and 
				 / executable modes
2:
	movb	r2,i.flgs / move remaining mode to i.flgs
	br	1f

syschown: / name; owner
	jsr	r0,isown / get the i-node and check user status
	tstb	u.uid / super user
	beq	2f / yes, 2f
	bit	$40,i.flgs / no, set userid on execution?
	bne	3f / yes error, could create Trojan Horses
2:
	movb	r2,i.uid / no, put the new owners id in the i-node
1:
	jmp	sysret4
3:
	jmp	error

arg:
	mov	u.sp,r1
	mov	*18.(r1),*(r0)+ / put argument of system call into 
				/ argument of arg2
	add	$2,18.(r1) / point pc on stack to next system argument
	rts	r0
	
arg2:
	jsr	r0,arg; u.namep / u.namep contains value of first arg in 
				/ sys call
	jsr	r0,arg; u.off / u.off contains value of second arg in 
			      / sys call
	mov	r0,r1 / r0 points to calling routine
	mov	(sp),r0 / put operation code back in r0
	mov	u.off,(sp) / put pointer to second argument on stack
	jmp	(r1) / return to calling routine

systime: / get time of year
	mov	s.time,4(sp)
	mov	s.time+2,2(sp) / put the present time on the stack
	br	sysret4

sysstime: / set time
	tstb	u.uid / is user the super user
	bne	error4 / no, error
	mov	4(sp),s.time
	mov	2(sp),s.time+2 / set the system time
	br	sysret4
	
sysbreak: / set the program break
	mov	u.break,r1 / move users break point to r1
	cmp	r1,$core / is it the same or lower than core?
	blos	1f / yes, 1f
	cmp	r1,sp / is it the same or higher than the stack?
	bhis	1f / yes, 1f
	bit	$1,r1 / is it an odd address
	beq	2f / no, its even
	clrb	(r1)+ / yes, make it even
2: / clear area between the break point and the stack
	cmp	r1,sp / is it higher or same than the stack
	bhis	1f / yes, quit
	clr	(r1)+ / clear word
	br	2b / go back
1:
	jsr	r0,arg; u.break / put the "address" in u.break (set new 
				/ break point)
	br	sysret4 / br sysret
	
maknod: / r1 contains the mode
	bis	$100000,r1 / allocate flag set
	mov	r1,-(sp) / put mode on stack
	mov	ii,r1 / move current i-number to r1
	jsr	r0,access; 1 / get its i-node into core
	mov	r1,-(sp) / put i-number on stack
	mov	$40.,r1 / r1 = 40
1: / scan for a free i-node (next 4 instructions)
	inc	r1 / r1 = r1 + 1
	jsr	r0,imap / get byte address and bit position in inode map in 
			/ r2 & m
	bitb	mq,(r2) / is the i-node active
	bne	1b / yes, try the next one
	bisb	mq,(r2) / no, make it active (put a 1 in the bit map)
	jsr	r0,iget / get i-node into core
	tst	i.flgs / is i-node already allocated
	blt	1b / yes, look for another one
	mov	r1,u.dirbuf / no, put i-number in u.dirbuf
	mov	(sp)+,r1 / get current i-number back
	jsr	r0,iget / get i-node in core
	jsr	r0,mkdir / make a directory entry in current directory
	mov	u.dirbuf,r1 / r1 = new inode number
	jsr	r0,iget / get it into core
	jsr	r0,copyz; inode; inode+32. / 0 it out
	mov	(sp)+,i.flgs / fill flags
	movb	u.uid,i.uid / user id
	movb	$1,i.nlks / 1 link
	mov	s.time,i.ctim / time created
	mov	s.time+2,i.ctim+2 / time modified
	jsr	r0,setimod / set modified flag
	rts	r0 / return
	
sysseek: / moves read write pointer in an fsp entry
	jsr	r0,seektell / get proper value in u.count
	add	u.base,u.count / add u.base to it
	mov	u.count,*u.fofp / put result into r/w pointer
	br	sysret4
	
systell: / get the r/w pointer
	jsr	r0,seektell
	br	error4
	
error4:
	jmp	error / see 'error' routine
sysret4:
	jmp	sysret / see 'sysret' routine
	
seektell:
	jsr	r0,arg; u.base / puts offset in u.base
	jsr	r0,arg; u.count / put ptr name in u.count
	mov	*u.r0,r1 / file descriptor in r1 (index in u.fp list)
	jsr	r0,getf / u.fofp points to 3rd word in fsp entry
	mov	r1,-(sp) / r1 has i-number of file, put it on the stack
	beq	error4 / if i-number is 0, not active so error
	bgt	.+4 / if its positive jump
	neg	r1 / if not make it positive
	jsr	r0,iget / get its i-node into core
	cmp	u.count,$1 / is ptr name =1
	blt	2f / no its zero
	beq	1f / yes its 1
	mov	i.size,u.count /  put number of bytes in file in u.count
	br	2f
1: / ptr name =1
	mov	*u.fofp,u.count / put offset in u.count
2: / ptrname =0
	mov	(sp)+,r1 / i-number on stack  r1
	rts	r0
	
sysintr: / set interrupt handling
	jsr	r0,arg; u.intr / put the argument in u.intr
	br	1f / go into quit routine
sysquit:
	jsr	r0,arg; u.quit / put argument in u.quit
1:
	mov	u.ttyp,r1 / move pointer to control tty buffer to r1
	beq	sysret4 / return to user
	clrb	6(r1) / clear the interrupt character in the tty buffer
	br	sysret4 / return to user
	
syssetuid: / set process id
	movb	*u.r0,r1 / move process id (number) to r1
	cmpb	r1,u.ruid / is it equal to the real user id number
	beq	1f / yes
	tstb	u.uid / no, is current user the super user?
	bne	error4 / no, error
1:
	movb	r1,u.uid / put process id in u.uid
	movb	 r1,u.ruid / put process id in u.ruid
	br	sysret4 / system return
	
sysgetuid:
	movb	u.ruid,*u.r0 / move the real user id to (u.r0)
	br	sysret4 / systerm return, sysret
	
fclose:
	mov	r1,-(sp) / put r1 on the stack (it contains the index 
			 / to u.fp list)
	jsr	r0,getf / r1 contains i-number, cdev has device =, u.fofp 
			/ points to 3rd word of fsp entry
	tst	r1 / is inumber 0?
	beq	1f / yes, i-node not active so return
	tst	(r0)+ / no, jump over error return
	mov	r1,r2 / move i-number to r2
	mov	(sp),r1 / restore value of r1 from the stack which is 
			/ index to u.fp
	clrb	u.fp(r1) / clear that entry in the u.fp list
	mov	u.fofp,r1 / r1 points to 3rd word in fsp entry
	decb	2(r1) / decrement the number of processes that have opened 
		      / the file
	bge	1f / if all processes haven't closed the file, return
	mov	r2,-(sp) / put r2 on the stack (i-number)
	clr	-4(r1) / clear 1st word of fsp entry
	tstb	3(r1) / has this file been deleted
	beq	2f / no, branch
	mov	r2,r1 / yes, put i-number back into r1
	jsr	r0,anyi / free all blocks related to i-number
			/ check if file appears in fsp again
2:
	mov	(sp)+,r1 / put i-number back into r1
	jsr	r0,iclose / check to see if its a special file
1:
	mov	(sp)+,r1 / put index to u.fp back into r1
	rts	r0
	
anyi: / r1 contains an i-number
	mov	$fsp,r2 / move start of fsp table to r2
1:
	cmp	r1,(r2) / do i-numbers match?
	beq	1f / yes, 1f
	neg	r1 / no complement r1
	cmp	r1,(r2) / do they match now?
	beq	1f / yes, transfer
		   / i-numbers do not match
	add	$8,r2 / no, bump to next entry in fsp table
	cmp	r2,$fsp+[nfiles*8] / are we at last entry in the table
	blt	1b / no, check next entries i-number
	tst	r1 / yes, no match
	bge	.+4
	neg	r1 / make i-number positive
	jsr	r0,imap / get address of allocation bit in the i-map in r2
	bicb	mq,(r2) / clear bit for i-node in the imap
	jsr	r0,itrunc / free all blocks related to i-node
	clr	i.flgs / clear all flags in the i-node
	rts	r0 / return
1: / i-numbers match
	incb	7(r2) / increment upper byte of the 4th word
	rts	r0 / in that fsp entry (deleted flag of fsp entry)
