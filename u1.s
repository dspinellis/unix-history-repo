/ u1 -- unix

unkni: / used for all system calls
sysent:
	incb	sysflg / indicate a system routine is
	beq	1f / in progress
	jmp	panic / called if trap inside system
1:
	mov	$s.syst+2,clockp
	mov	r0,-(sp) / save user registers
	mov	sp,u.r0 / pointer to bottom of users stack in u.r0
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	r3,-(sp)
	mov	r4,-(sp)
	mov	r5,-(sp)
	mov	ac,-(sp) / "accumulator" register for extended
		         / arithmetic unit
	mov	mq,-(sp) / "multiplier quotient" register for the
		         / extended arithmetic unit
	mov	sc,-(sp) / "step count" register for the extended
		         / arithmetic unit
	mov	sp,u.sp / u.sp points to top of users stack
	mov	18.(sp),r0 / store pc in r0
	mov	-(r0),r0 / sys inst in r0      10400xxx
	sub	$sys,r0 / get xxx code
	asl	r0 / multiply by 2 to jump indirect in bytes
	cmp	r0,$2f-1f / limit of table (35) exceeded
	bhis	badsys / yes, bad system call
	bic	$341,20.(sp) / set users processor priority to 0 and clear
		             / carry bit
	jmp	*1f(r0) / jump indirect thru table of addresses
		        / to proper system routine.
1:
	sysrele / 0
	sysexit / 1
	sysfork / 2
	sysread / 3
	syswrite / 4
	sysopen / 5
	sysclose / 6
	syswait / 7
	syscreat / 8
	syslink / 9
	sysunlink / 10
	sysexec / 11
	syschdir / 12
	systime / 13
	sysmkdir / 14
	syschmod / 15
	syschown / 16
	sysbreak / 17
	sysstat / 18
	sysseek / 19
	systell / 20
	sysmount / 21
	sysumount / 22
	syssetuid / 23
	sysgetuid / 24
	sysstime / 25
	sysquit / 26
	sysintr / 27
	sysfstat / 28
	sysemt / 29
	sysmdate / 30
	sysstty / 31
	sysgtty / 32
	sysilgins / 33
2:

error:
	mov	u.sp,r1
	bis	$1,20.(r1) / set c bit in processor status word below
		           / users stack

sysret:
	tstb	u.bsys / is a process about to be terminated because
	bne	sysexit / of an error? yes, go to sysexit
	mov	u.sp,sp / no point stack to users stack
	clr	r1 / zero r1 to check last mentioned i-node
	jsr	r0,iget / if last mentioned i-node has been modified
		        / it is written out
	tstb	smod / has the super block been modified
	beq	1f / no, 1f
	clrb	smod / yes, clear smod
	bis	$1000,sb0 / set write bit in I/O queue for super block
		          / output
	jsr	r0,ppoke / write out modified super block to disk
1:
	tstb	mmod / has the super block for the dismountable file
		     / system
	beq	1f / been modified?  no, 1f
	clrb	mmod / yes, clear mmod
	movb	mntd,sb1 / set the I/O queue
	bis	$1000,sb1 / set write bit in I/O queue for detached sb
	jsr	r0,ppoke / write it out to its device
1:
	tstb	uquant / is the time quantum 0?
	bne	1f / no, don't swap it out

sysrele:
	jsr	r0,tswap / yes, swap it out
1:
	mov	(sp)+,sc / restore user registers
	mov	(sp)+,mq
	mov	(sp)+,ac
	mov	(sp)+,r5
	mov	(sp)+,r4
	mov	(sp)+,r3
	mov	(sp)+,r2
	mov	(sp)+,r1
	mov	(sp)+,r0
	mov	$s.chrgt+2,clockp
	decb	sysflg / turn system flag off
	jsr	r0,isintr / is there an interrupt from the user
		br intract / yes, output gets flushed, take interrupt
		           / action
	rti	/ no return from interrupt

badsys:
	incb	u.bsys / turn on the user's bad-system flag
	mov	$3f,u.namep / point u.namep to "core\0\0"
	jsr	r0,namei / get the i-number for the core image file
		br 1f / error
	neg	r1 / negate the i-number to open the core image file
		   / for writing
	jsr	r0,iopen / open the core image file
	jsr	r0,itrunc / free all associated blocks
	br	2f
1:
	mov	$17,r1 / put i-node mode (17) in r1
	jsr	r0,maknod / make an i-node
	mov	u.dirbuf,r1 / put i-nodes number in r1
2:
	mov	$core,u.base / move address core to u.base
	mov	$ecore-core,u.count / put the byte count in u.count
	mov	$u.off,u.fofp / more user offset to u.fofp
	clr	u.off / clear user offset
	jsr	r0,writei / write out the core image to the user
	mov	$user,u.base / pt. u.base to user
	mov	$64.,u.count / u.count = 64
	jsr	r0,writei / write out all the user parameters
	neg	r1 / make i-number positive
	jsr	r0,iclose / close the core image file
	br	sysexit /
3:
	<core\0\0>

sysexit: / terminate process
	clr	u.intr / clear interrupt control word
	clr	r1 / clear r1
1: / r1 has file descriptor (index to u.fp list)  Search the whole list
	jsr	r0,fclose / close all files the process opened
		br .+2 / ignore error return
	inc	r1 / increment file descriptor
	cmp	r1,$10. / end of u.fp list?
	blt	1b / no, go back
	movb	u.uno,r1 / yes, move dying process's number to r1
	clrb	 p.stat-1(r1) / free the process
	asl	r1 / use r1 for index into the below tables
	mov	p.pid-2(r1),r3 / move dying process's name to r3
	mov	p.ppid-2(r1),r4 / move its parents name to r4
	clr	r2
	clr	r5 / initialize reg
1: / find children of this dying process, if they are zombies, free them
	add	$2,r2 / search parent process table for dying process's name
	cmp	p.ppid-2(r2),r3 / found it?
	bne	3f / no
	asr	r2 / yes, it is a parent
	cmpb	p.stat-1(r2),$3 / is the child of this dying process a
		                / zombie
	bne	2f / no
	clrb	p.stat-1(r2) / yes, free the child process
2:
	asl	r2
3: / search the process name table for the dying process's parent
	cmp	p.pid-2(r2),r4 / found it?
	bne	3f / no
	mov	r2,r5 / yes, put index to p.pid table (parents
		      / process # x2) in r5
3:
	cmp	r2,$nproc+nproc / has whole table been searched?
	blt	1b / no, go back
	mov	r5,r1 / yes, r1 now has parents process # x2
	beq	2f / no parent has been found. The process just dies
	asr	r1 / set up index to p.stat
	movb	p.stat-1(r1),r2 / move status of parent to r2
	beq	2f / if its been freed, 2f
	cmp	r2,$3 / is parent a zombie?
	beq	2f / yes, 2f
	movb	u.uno,r3 / move dying process's number to r3
	movb	$3,p.stat-1(r3) / make the process a zombie
	cmp	r2,$2 / is the parent waiting for this child to die
	bne	2f / yes, notify parent not to wait any more
	decb	p.stat-1(r1) / awaken it by putting it (parent)
	mov	$runq+4,r2 / on the runq
	jsr	r0, putlu
2: / the process dies
	clrb	u.uno / put zero as the process number, so "swap" will
	jsr	r0,swap / overwrite process with another process
	0	/ and thereby kill it; halt?

intract: / interrupt action
	cmp	*(sp),$rti / are you in a clock interrupt?
	bne	1f / no, 1f
	cmp	(sp)+,(sp)+ / pop clock pointer
1: / now in user area
	mov	r1,-(sp) / save r1
	mov	u.ttyp,r1 / pointer to tty buffer in control-to r1
	cmpb	6(r1),$177 / is the interrupt char equal to "del"
	beq	1f / yes, 1f
	clrb	6(r1) / no, clear the byte (must be a quit character)
	mov	(sp)+,r1 / restore r1
	clr	u.quit / clear quit flag
	bis	$20,2(sp) / set trace for quit (sets t bit of ps-trace trap)
	rti	          / return from interrupt
1: / interrupt char = del
	clrb	6(r1) / clear the interrupt byte in the buffer
	mov	(sp)+,r1 / restore r1
	cmp	u.intr,$core / should control be transferred to loc core?
	blo	1f
	jmp	*u.intr / user to do rti yes, transfer to loc core
1:
	sys	1 / exit

syswait: / wait for a process to die
	movb	u.uno,r1 / put parents process number in r1
	asl	r1 / x2 to get index into p.pid table
	mov	p.pid-2(r1),r1 / get the name of this process
	clr	r2
	clr	r3 / initialize reg 3
1:
	add	$2,r2 / use r2 for index into p.ppid table / search table
		      / of parent processes for this process name
	cmp	p.ppid-2(r2),r1 / r2 will contain the childs process number
	bne	3f / branch if no match of parent process name
	inc	r3 / yes, a match, r3 indicates number of children
	asr	r2 / r2/2 to get index to p.stat table
	cmpb	p.stat-1(r2),$3 / is the child process a zombie?
	bne	2f / no, skip it
	clrb	p.stat-1(r2) / yes, free it
	asl	r2 / r2x2 to get index into p.pid table
	mov	p.pid-2(r2),*u.r0 / put childs process name in (u.r0)
	br	sysret1 / return cause child is dead
2:
	asl	r2 / r2x2 to get index into p.ppid table
3:
	cmp	r2,$nproc+nproc / have all processes been checked?
	blt	1b / no, continue search
	tst	r3 / one gets here if there are no children or children
		   / that are still active
	beq	error1 / there are no children, error
	movb	u.uno,r1 / there are children so put parent process number
		         / in r1
	incb	p.stat-1(r1) / it is waiting for other children to die
	jsr	r0,swap / swap it out, because it's waiting
	br	syswait / wait on next process

error1:
	jmp	error / see 'error' routine
sysret1:
	jmp	sysret / see 'sysret' routine

sysfork: / create a new process
	clr	r1
1: / search p.stat table for unused process number
	inc	r1
	tstb	p.stat-1(r1) / is process active, unused, dead
	beq	1f / it's unused so branch
	cmp	r1,$nproc / all processes checked
	blt	1b / no, branch back
	add	$2,18.(sp) / add 2 to pc when trap occured, points
		           / to old process return
	br	error1 / no room for a new process
1:
	movb	u.uno,-(sp) / save parent process number
	movb	r1,u.uno / set child process number to r1
	incb	p.stat-1(r1) / set p.stat entry for child process to
		             / active status
	mov	u.ttyp,r2 / put pointer to parent process' control tty
		          / buffer in r2
	beq	2f / branch, if no such tty assigned
	clrb	6(r2) / clear interrupt character in tty buffer
2:
	mov	$runq+4,r2
	jsr	r0,putlu / put child process on lowest priority run queue
	asl	r1 / multiply r1 by 2 to get index into p.pid table
	inc	mpid / increment m.pid; get a new process name
	mov	mpid,p.pid-2(r1) / put new process name in child process'
		                 / name slot
	movb	(sp),r2 / put parent process number in r2
	asl	r2 / multiply by 2 to get index into below tables
	mov	p.pid-2(r2),r2 / get process name of parent process
	mov	r2,p.ppid-2(r1) / put parent process name in parent
		                / process slot for child
	mov	r2,*u.r0 / put parent process name on stack at location
		         / where r0 was saved
	mov	$sysret1,-(sp) /
	mov	sp,u.usp / contents of sp at the time when user is
		         / swapped out
	mov	$sstack,sp / point sp to swapping stack space
	jsr	r0,wswap / put child process out on drum
	jsr	r0,unpack / unpack user stack
	mov	u.usp,sp / restore user stack pointer
	tst	(sp)+ / bump stack pointer
	movb	(sp)+,u.uno / put parent process number in u.uno
	mov	mpid,*u.r0 / put child process name on stack where r0
		           / was saved
	add	$2,18.(sp) / add 2 to pc on stack; gives parent
		           / process return
	clr	r1
1: / search u.fp list to find the files opened by the parent process
	movb	u.fp(r1),r2 / get an open file for this process
	beq	2f / file has not been opened by parent, so branch
	asl	r2 / multiply by 8
	asl	r2 / to get index into fsp table
	asl	r2
	incb	fsp-2(r2) / increment number of processes using file,
		          / because child will now be using this file
2:
	inc	r1 / get next open file
	cmp	r1,$10. / 10. files is the maximum number which can be
		        / opened
	blt	1b / check next entry
	br	sysret1

sysread:
	jsr	r0,rw1 / get i-number of file to be read into r1
	tst	r1 / negative i-number?
	ble	error1 / yes, error 1 to read it should be positive
	jsr	r0,readi / read data into core
	br	1f

syswrite:
	jsr	r0,rw1 / get i-number in r1 of file to write
        tst    r1 / positive i-number ?
        bge    error1 / yes, error 1 negative i-number means write
        neg    r1 / make it positive
        jsr    r0,writei / write data
1:
        mov    u.nread,*u.r0 / put no. of bytes transferred into (u.r0)
        br     sysret1

rw1:
        jsr    r0,arg; u.base / get buffer pointer
        jsr    r0,arg; u.count / get no. of characters
        mov    *u.r0,r1 / put file descriptor (index to u.fp table) in r1
        jsr    r0,getf / get i-number of the file in r1
        rts    r0

sysopen:
        jsr    r0,arg2 / get sys args into u.namep and on stack
        jsr    r0,namei / i-number of file in r1
        br     error2 / file not found
        tst    (sp) / is mode = 0 (2nd arg of call; 0 means, open for read)
        beq    1f / yes, leave i-number positive
        neg    r1 / open for writing so make i-number negative
1:
        jsr    r0,iopen / open file whose i-number is in r1
        tst    (sp)+ / pop the stack and test the mode
        beq    op1 / is open for read op1

op0:
        neg    r1 / make i-number positive if open for writing
op1:
        clr    r2 / clear registers
        clr    r3
1: / scan the list of entries in fsp table
        tstb   u.fp(r2) / test the entry in the u.fp list
        beq    1f / if byte in list is 0 branch
        inc    r2 / bump r2 so next byte can be checked
        cmp    r2,$10. / reached end of list?
        blt    1b / no, go back
        br     error2 / yes, error (no files open)
1:
        tst    fsp(r3) / scan fsp entries
        beq    1f / if 0 branch
        add    $8.,r3 / add 8 to r3 to bump it to next entry mfsp table
        cmp    r3,$[nfiles*8.] / done scanning
        blt    1b / no, back
        br     error2 / yes, error
1: / r2 has index to u.fp list; r3, has index to fsp table
        mov    r1,fsp(r3) / put i-number of open file into next available
        mov    cdev,fsp+2(r3) / entry in fsp table, put # of device in
                              / next word
        clr    fsp+4(r3)
        clr    fsp+6(r3) / clear the next two words
        asr    r3
        asr    r3 / divide by 8 to get number of the fsp entry-1
        asr    r3
        inc    r3 / add 1 to get fsp entry number
        movb   r3,u.fp(r2) / move entry number into next available slot
                           / in u.fp list
        mov    r2,*u.r0 / move index to u.fp list into r0 loc on stack
        br     sysret2

error2:
        jmp    error / see 'error' routine
sysret2:
        jmp    sysret / see 'sysret' routine

syscreat: / name; mode
        jsr    r0,arg2 / put file name in u.namep put mode on stack
        jsr    r0,namei / get the i-number
               br  2f / if file doesn't exist 2f
        neg    r1 / if file already exists make i-number negative
                  / (open for writing)
        jsr    r0,iopen /
        jsr    r0,itrunc / truncate to 0 length
        br     op0
2: / file doesn't exist
        mov    (sp)+,r1 / put the mode in r1
        bic    $!377,r1 / clear upper byte
        jsr    r0,maknod / make an i-node for this file
        mov    u.dirbuf,r1 / put i-number for this new file in r1
        br     op0 / open the file

sysmkdir: / make a directory
        jsr    r0,arg2 / point u.namep to the file name
        jsr    r0,namei / get the i-number
               br .+4 / if file not found branch around error
        br     error2 / directory already exists (error)
        tstb   u.uid / is user the super user
        bne    error2 / no, not allowed
        mov    (sp)+,r1 / put the mode in r1
        bic    $!317,r1 / all but su and ex
        bis    $40000,r1 / directory flag
        jsr    r0,maknod / make the i-node for the directory
        br     sysret2 /

sysclose: / close the file
        mov    *u.r0,r1 / move index to u.fp list into r1
        jsr    r0,fclose / close the file
               br error2 / unknown file descriptor
        br     sysret2

sysemt:
        jsr    r0,arg; 30 / put the argument of the sysemt call in loc 30
        cmp    30,$core / was the argument a lower address than core
        blo    1f / yes, rtssym
        cmp    30,$ecore / no, was it higher than "core" and less than
                         / "ecore"
        blo    2f / yes, sysret2
1:
        mov    $rtssym,30
2:
        br     sysret2
sysilgins: / calculate proper illegal instruction trap address
        jsr    r0,arg; 10 / take address from sysilgins call     , put
                          / it in loc 8.,
        cmp    10,$core / making it the illegal instruction trap address
        blo    1f / is the address a user core address?  yes, go to 2f
        cmp    10,$ecore
        blo    2f
1:
        mov    $fpsym,10 / no, make 'fpsum' the illegal instruction trap
                         / address for the system
2:
        br     sysret2 / return to the caller via 'sysret'

sysmdate: / change the modification time of a file
        jsr    r0,arg; u.namep / point u.namep to the file name
        jsr    r0,namei / get its i-number
               br error2 / no, such file
        jsr    r0,iget / get i-node into core
        cmpb   u.uid,i.uid / is user same as owner
        beq    1f / yes
        tstb   u.uid / no, is user the super user
        bne    error2 / no, error
1:
        jsr    r0,setimod / fill in modification data, time etc.
        mov    4(sp),i.mtim / move present time to
        mov    2(sp),i.mtim+2 / modification time
        br     sysret2

sysstty: / set mode of typewriter; 3 consequtive word arguments
        jsr    r0,_gtty / r1 will have offset to tty block, r2 has source
        mov    r2,-(sp)
        mov    r1,-(sp) / put r1 and r2 on the stack
1: / flush the clist wait till typewriter is quiescent
        mov    (sp),r1 / restore r1 to tty block offset
        movb   tty+3(r1),0f / put cc offset into getc argument
        mov    $240,*$ps / set processor priority to 5
        jsr    r0,getc; 0:../ put character from clist in r1
               br .+4 / list empty, skip branch
        br     1b / get another character until list is empty
        mov    0b,r1 / move cc offset to r1
        inc    r1 / bump it for output clist
        tstb   cc(r1) / is it 0
        beq    1f / yes, no characters to output
        mov    r1,0f / no, put offset in sleep arg
        jsr    r0,sleep; 0:.. / put tty output process to sleep
        br     1b / try to calm it down again
1:
        mov    (sp)+,r1
        mov    (sp)+,r2 / restore registers
        mov    (r2)+,r3 / put reader control status in r3
        beq    1f / if 0, 1f
        mov    r3,rcsr(r1) / move r.c. status to reader control status
                           / register
1:
        mov    (r2)+,r3 / move pointer control status to r3
        beq    1f / if 0 1f
        mov    r3,tcsr(r1) / move p.c. status to printer control status reg
1:
        mov    (r2)+,tty+4(r1) / move to flag byte of tty block
        jmp     sysret2 / return to user

sysgtty: / get status of typewriter; 3 consequtive word arguments
        jsr    r0,_gtty / r1 will have offset to tty block, r2 has
                       / destination
        mov    rcsr(r1),(r2)+ / put reader control status in 1st word
                              / of dest
        mov    tcsr(r1),(r2)+ / put printer control status in 2nd word
                              / of dest
        mov    tty+4(r1),(r2)+ / put mode in 3rd word
        jmp    sysret2 / return to user

_gtty:
        jsr    r0,arg; u.off / put first arg in u.off
        mov    *u.r0,r1 / put file descriptor in r1
        jsr    r0,getf / get the i-number of the file
        tst    r1 / is it open for reading
        bgt    1f / yes
        neg    r1 / no, i-number is negative, so make it positive
1:
        sub    $14.,r1 / get i-number of tty0
        cmp    r1,$ntty-1 / is there such a typewriter
        bhis   error9 / no, error
        asl    r1 / 0%2
        asl    r1 / 0%4 / yes
        asl    r1 / 0%8 / multiply by 8 so r1 points to tty block
        mov    u.off,r2 / put argument in r2
        rts    r0 / return
