/ u8 -- unix

rtap: / read from the dec tape
	asr	r1 / divide the i-number by 2
	sub	$4.,r1 / (i-number/2)-4  r1
	mov	r1,cdev / cdev now has device number
	jsr	r0,bread; 578. / read in block thats in *u.fofp

wtap:
	asr	r1 / divide i-number by 2
	sub	$4.,r1 / r1 = i-number minus 4
	mov	r1,cdev / this is used as the device number
	jsr	r0,bwrite; 578. / write block (u.fofp) on dec tape
				/ Maximum

rrk0:
	mov	$1,cdev / set current device to i., disk
	jsr	r0,bread; 4872. / read block from disk (maximum block
				/ number allowed on device is 4872.)
				/ - (u.fofp) contains block number

wrk0:
	mov	$1,cdev / set current device to 1; disk
	jsr	r0,bwrite; 4872. / write block (u.fofp) on disk

rrf0:
	clr	cdev / set current device to 0., fixed head disk
	jsr	r0,bread; 1024. / read block (u.fofp) from fixed head
				/ disk (max. block number allowed on
				/ device is 1024.)

wrf0:
	clr	cdev / set current device to 0., fixed head disk
	jsr	r0,bwrite; 1024. / write block '(u.fofp)' on fixed head
				 / disk

bread: / read a block from a block structured device
	jsr	r0,tstdeve / error on special file I/O (only works on
			   / tape)
	mov	*u.fofp,r1 / move block number to r1
	mov	$2.-cold,-(sp) / "2-cold" to stack
1:
	cmp	r1,(r0)	/ is this block # greater than or equal to
			/ maximum block # allowed on device
	bhis	1f / yes, 1f (error)
	mov	r1,-(sp) / no, put block # on stack
	jsr	r0,preread / read in the block into an I/O buffer
	mov	(sp)+,r1 / return block # to r1
	inc	r1 / bump block # to next consecutive block
	dec	(sp) / "2-1-cold" on stack
	bgt	1b / 2-1-cold = 0?  No, go back and read in next block
1:
	tst	(sp)+ / yes, pop stack to clear off cold calculation
	mov	*u.fofp,r1 / restore r1 to initial value of the
			   / block #
       cmp     r1,(r0)+ / block # greater than or equal to maximum
       	                / block number allowed
       bhis    error10 / yes, error
       inc     *u.fofp / no, *u.fofp has next block number
       jsr     r0,preread / read in the block whose number is in r1
       bis     $40000,(r5) / set bit 14 of the 1st word of the I/O
       	                   / buffer
1:
       bit     $22000,(r5) / are 10th and 13th bits set (read bits)
       beq     1f / no
       cmp     cdev,$1 / disk or drum?
       ble     2f / yes
       tstb    uquant / is the time quantum = 0?
       bne     2f / no, 2f
       mov     r5,-(sp) / yes, save r5 (buffer address)
       jsr     r0,sleep; 31. / put process to sleep in channel 31 (tape)
       mov     (sp)+,r5 / restore r5
       br      1b / go back
2: / drum or disk
       jsr     r0,idle; s.wait+2 / wait
       br      1b
1: / 10th and 13th bits not set
       bic     $40000,(r5) / clear bit 14
       jsr     r0,tstdeve / test device for error (tape)
       add     $8,r5 / r5 points to data in I/O buffer
       jsr     r0,dioreg / do bookkeeping on u.count etc.
1: / r5 points to beginning of data in I/O buffer, r2 points to beginning
   / of users data
       movb    (r5)+,(r2)+ / move data from the I/O buffer
       dec     r3 / to the user's area in core starting at u.base
       bne     1b
       tst     u.count / done
       beq     1f / yes, return
       tst     -(r0) / no, point r0 to the argument again
       br      bread / read some more
1:
       mov     (sp)+,r0 / jump to routine that called readi
       jmp     ret

bwrite: / write on block structured device
       jsr     r0,tstdeve / test the device for an error
       mov     *u.fofp,r1 / put the block number in r1
       cmp     r1,(r0)+ / does block number exceed maximum allowable #
       bhis    error10 / yes, error
       inc     *u.fofp / no, increment block number
       jsr     r0,wslot / get an I/O buffer to write into
       jsr     r0,dioreg / do the necessary bookkeeping
1: / r2 points to the users data; r5 points to the I/O buffers data area
       movb    (r2)+,(r5)+ / ; r3, has the byte count
       dec     r3 / area to the I/O buffer
       bne     1b
       jsr     r0,dskwr / write it out on the device
       tst     u.count / done
       beq     1f / yes, 1f
       tst     -(r0) / no, point r0 to the argument of the call
       br      bwrite / go back and write next block
1:
       mov     (sp)+,r0 / return to routine that called writei
       jmp     ret
tstdeve: / check whether permanent error has occured on special file
         / I/O
       mov     cdev,r1 / only works on tape; r1 has device #
       tstb    deverr(r1) / test error bit of device
       bne     1f / error
       rts     r0 / device okay
1:
       clrb    deverr(r1) / clear error

error10:
       jmp     error / see 'error' routine

dioreg:
       mov     u.count,r3 / move char count to r3
       cmp     r3,$512. / more than 512. char?
       blos    1f / no, branch
       mov     $512.,r3 / yes, just take 512.
1:
       mov     u.base,r2 / put users base in r2
       add     r3,u.nread / add the number to be read to u.nread
       sub     r3,u.count / update count
       add     r3,u.base / update base
       rts     r0 / return

preread:
       jsr     r0,bufaloc / get a free I/O buffer (r1 has block number)
       br      1f / branch if block already in a I/O buffer
       bis     $2000,(r5) / set read bit (bit 100 in I/O buffer)
       jsr     r0,poke / perform the read
1:
       clr     *$ps / ps = 0
       rts     r0

dskrd:
       jsr     r0,bufaloc / shuffle off to bufaloc; get a free I/O buffer
               br 1f
       bis     $2000,(r5) / set bit 10 of word 1 of I/O queue entry
                          / for buffer
       jsr     r0,poke / just assigned in bufaloc, bit 10=1 says read
1:
       clr     *$ps
       bit     $22000,(r5) / if either bits 10, or 13 are 1; jump to idle
       beq     1f
       jsr     r0,idle; s.wait+2
       br      1b
1:
       add     $8,r5 / r5 points to first word of data in block just read
                     / in
       rts     r0

wslot:
       jsr     r0,bufaloc / get a free I/O buffer; pointer to first
               br 1f / word in buffer in r5
1:
       bit     $22000,(r5) / check bits 10, 13 (read, waiting to read)
       	                   / of I/O queue entry
       beq     1f / branch if 10, 13 zero (i.e., not reading, or waiting
       	          / to read)
       jsr     r0,idle; s.wait+2 / if buffer is reading or writing to read,
       	                         / idle
       br      1b / till finished
1:
       bis     $101000,(r5) / set bits 9, 15 in 1st word of I/O queue
                            / (write, inhibit bits)
       clr     *$ps / clear processor status
       add     $8,r5 / r5 points to first word in data area for this
       	             / block
       rts     r0

dskwr:
       bic     $100000,*bufp / clear bit 15 of I/O queue entry at
                             / bottom of queue

ppoke:
       mov     $340,*$ps
       jsr     r0,poke
       clr     *$ps
       rts     r0
poke:
       mov     r1,-(sp)
       mov     r2,-(sp)
       mov     r3,-(sp)
       mov     $bufp+nbuf+nbuf+6,r2 / r2 points to highest priority I/O
                                    / queue pointer
1:
       mov     -(r2),r1 / r1 points to an I/O queue entry
       bit     $3000,(r1) / test bits 9 and 10 of word 1 of I/O queue
                          / entry
       beq     2f / branch to 2f if both are clear
       bit     $130000,(r1) / test bits 12, 13, and 15
       bne     2f / branch if any are set
       movb    (r1),r3 / get device id
       tstb    deverr(r3) / test for errors on this device
       beq     3f / branch if no errors
       mov     $-1,2(r1) / destroy associativity
       clrb    1(r1) / do not do I/O
       br      2f
3:
       cmpb    r3,$1 / device id = 1; device is disk
       blt     prf / device id = 0; device is drum
       bgt     ptc / device id greater than or equal to 1; device is
       	           / dec tape
       bit     $2,active / test disk busy bit
       bne     2f / branch if bit is set
       bis     $2,active / set disk busy bit
       mov     r1,rkap / rkap points to current I/O queue entry for disk
       mov     2(r1),mq / put physical block number in mq
       mov     $12.,div / divide physical block number by 12.
       mov     $rkda+2,r3 /
       mov     ac,-(sp) / put remainder from divide on stack; gives
                        / sector number
       mov     $4,lsh / shift quotient 4 bits, to align with cyl and surf
                      / bits in rkda
       bis     mq,(sp) / or mq with sector; gives total disk address
       br      3f
prf: / drum
       bit     $1,active / test drum busy bit
       bne     2f / branch if bit is set
       bis     $1,active / set drum busy bit
       mov     r1,rfap / rfap points to current I/O queue entry for drum
       mov     $dae+2,r3
       clr     -(sp)
       movb    2(r1),1(sp) / move low byte of physical block number into
                           / high byte of stack
       clr     -(sp) / word
       movb    3(r1),(sp) / move high byte of physical block number into
                          / low byte of stack
       mov     (sp)+,-(r3) / load dae with high byte of physical block
                           / number
3:
       mov     (sp)+,-(r3) / load rkda register; load dar register
       mov     6(r1),-(r3) / load bus address register
       mov     4(r1),-(r3) / load word count register
       mov     $103,-(sp) / 103 indicates write operation when loaded
                          / in csr
       bit     $2000,(r1) / if bit 10 of word 1 of I/O queue entry is
                          / a one
       beq     3f / then read operation is indicated
       mov     $105,(sp) / 105 indicates read operation
3:
       mov     (sp)+,-(r3) / load csr with interrupt enabled, command, go
       br      seta
ptc: / tape I/O
       bit     $4,active
       bne     2f
       mov     tccm,r3
       swab    r3
       bic     $!7,r3
       add     $2,r3
       cmpb    r3,(r1)
       beq     3f
       movb    $1,tccm / stop transport if not same unit
3:
       bis     $4,active
       mov     r1,tcap
       mov     $20.,tcerrc
       mov     $tape1,tcstate
       movb    (r1),r3 / device
       sub     $2,r3 / now unit
       swab    r3
       bis     $103,r3 / now rbn,for,unit,ie
       mov     r3,tccm
 seta: / I/O queue bookkeeping; set read/write waiting bits.
       mov     (r1),r3 / move word 1 of I/O queue entry into r3
       bic     $!3000,r3 / clear all bits except 9 and 10
       bic     $3000,(r1) / clear only bits 9 and 10
       rol     r3
       rol     r3
       rol     r3
       bis     r3,(r1) / or old value of bits 9 and 10 with bits 12
       	               / and 13
2:
       cmp     r2,$bufp / test to see if entire I/O queue has been
                        / scanned
       bhi    1b
       mov    (sp)+,r3
       mov    (sp)+,r2
       mov    (sp)+,r1
       rts    r0

bufaloc:
       mov    r2,-(sp) / save r2 on stack
       mov    $340,*$ps / set processor priority to 7
1:
       clr    -(sp) / vacant buffer
       mov    $bufp,r2 / bufp contains pointers to I/O queue entrys
                       / in buffer area
2:
       mov    (r2)+,r5 / move pointer to word 1 of an I/O queue entry
                       / into r5
       bit    $173000,(r5) / lock+keep+active+outstanding
       bne    3f / branch when any of bits 9,10,12,13,14,15 are set
       	         / (i.e., buffer busy)
       mov    r2,(sp) / save pointer to last non-busy buffer found
       	              / points to word 2 of I/O queue entry)
3:
       cmpb   (r5),cdev / is device in I/O queue entry same as current
                        / device
       bne    3f
       cmp    2(r5),r1 / is block number in I/O queue entry, same as
       	               / current block number
       bne    3f
       tst    (sp)+ / bump stack pointer
       br     1f / use this buffer
3:
       cmp    r2,$bufp+nbuf+nbuf
       blo    2b / go to 2b if r2 less than bufp+nbuf+nbuf (all
                 / buffers not checked)
       mov    (sp)+,r2 / once all bufs are examined move pointer to
       	               / last free block
       bne    2f / if (sp) is non zero, i.e., if a free buffer is
       	         / found branch to 2f
       jsr    r0,idle; s.wait+2 / idle if no free buffers
       br     1b
2:
       tst    (r0)+ / skip if warmed over buffer
1:
       mov    -(r2),r5 / put pointer to word 1 of I/O queue entry in r5
       movb   cdev,(r5) / put current device number in I/O queue entry
       mov    r1,2(r5) / move block number into word 2 of I/O queue
/ entry
1:
       cmp     r2,$bufp / bump all entrys in bufp and put latest assigned
       blos    1f / buffer on the top (this makes if the lowest priority)
       mov     -(r2),2(r2) / job for a particular device
       br      1b
1:
       mov     r5,(r2)
       mov     (sp)+,r2 / restore r2
       rts     r0

tape: / dec tape interrupt
       jsr     r0,setisp / save registers and clockp on stack
       mov     tcstate,r3 / put state of dec tape in r3
       jsr     r0,trapt; tccm; tcap; 4 / busy bit
       mov     r3,pc / device control status register
       / if no errors, go to device state (an address)

taper: / dec tape error
       dec     tcerrc / decrement the number of errors
       bne     1f / if more than 1 branch
       movb    1(r2),r3 / r2+1 points to command register upper byte
       bic     $!7,r3 / clear all but bits 8-10 (Unit Selection)
       incb    deverr+2(r3) / set error bit for this tape unit
       br      tape3
1: / more than 1 error
       bit     $4000,(r2) / direction of tape
       beq     1f / if forward go to 1f
       bic     $4000,(r2) / reverse, set to forward
       mov     $tape1,tcstate / put tape 1 in the state
       br      0f
1: / put tape in reverse
       bis     $4000,(r2) / set tape to reverse direction
       mov     $tape2,tcstate / put tape 2 as the state
0:
       bis     $4,active / check active bit of tape
       movb    $103,(r2) / set read function and interrupt enable
       br      4f / go to retisp
tape1: / read bn forward
       mov     $tcdt,r0 / move address of data register to r0
       cmp     (r0),2(r1) / compare block addresses
       blt     0b / if lt, keep moving
       bgt     taper / if gt, reverse
       mov     6(r1),-(r0) / put bus address in tcba
       mov     4(r1),-(r0) / put word count in tcwc
       mov     $115,-(sp) / put end interrupt enable
       bit     $20000,(r1) / is "waiting to read bit" of I/O queue set?
       beq     1f / no,  1f
       mov     $105,(sp) / yes, put and interrupt enable
1:
       movb    (sp)+,(r2) / move function into command register (tccm)
       bis     $4,active / set active bit
       mov     $tape3,tcstate / get ready for I/O transfer
       br      4f / go to retisp (rti)

tape2: / read bn bakasswards
       mov     tcdt,r0 / r0 has contents of data register
       add     $3,r0 / overshoot
       cmp     r0,2(r1)
       bgt     0b / if gt keep reading
       br      taper / else reverse

tape3: / I/O transfer
       bic     $30000,(r1) / clear bits 12 and 13 of I/O queue entry
       jsr     r0,poke / do the I/O
       bit     $4,active / still busy see if pick up r-ahead, w-behind
       bne     1f / yes
       movb    $1,(r2) / no, indicate too bad
1:
       jsr     r0,wakeup; runq; 31. / wait up
       br      4f / retisp

drum: / interrupt handler
       jsr     r0,setisp / save r1,r2,r3, and clockp on the stack
       jsr     r0,trapt; dcs; rfap; 1 / check for stray interrupt or
                                      / error
               br 3f / no, error
       br      2f / error

disk:
       jsr     r0,setisp / save r1,r2,r3, and clockp on the stack
       jmp     *$0f
0:
       jsr     r0,trapt; rkcs; rkap; 2
       	       br 3f / no, errors
       mov     $115,(r2) / drive reset, errbit was set
       mov     $1f,0b-2 / next time jmp *$0f is executed jmp will be
                        / to 1f
       br      4f
1:
       bit     $20000,rkcs
       beq     4f / wait for seek complete
       mov     $0b,0b-2
       mov     rkap,r1
2:
       bit     $3000,(r1) / are bits 9 or 10 set in the 1st word of
                          / the disk buffer
       bne     3f / no, branch ignore error if outstanding
       inc     r1
       asr     (r1)
       asr     (r1)
       asr     (r1) / reissue request
       dec     r1
3:
       bic     $30000,(r1) / clear bits 12 and 13 in 1st word of buffer
       mov     ac,-(sp)
       mov     mq,-(sp) / put these on the stack
       mov     sc,-(sp)
       jsr     r0,poke
       mov     (sp)+,sc
       mov     (sp)+,mq / pop them off stack
       mov     (sp)+,ac
4:
       jmp     retisp / u4-3

trapt:                                   / r2 points to the
       mov     (r0)+,r2 / device control register
       mov     *(r0)+,r1 / transaction pointer points to buffer
       tst     (sp)+
       tstb    (r2) / is ready bit of dcs set?
       bge     4b / device still active so branch
       bit     (r0),active / was device busy?
       beq     4b / no, stray interrupt
       bic     (r0)+,active / yes, set active to zero
       tst     (r2) / test the err(bit is) of dcs
       bge     2f / if no error jump to 2f
       tst     (r0)+ / skip on error
 2:
       jmp     (r0)
