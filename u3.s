/ u3 -- unix

tswap:
       movb    u.uno,r1 / move users process number to r1
       mov     $runq+4,r2 / move lowest priority queue address to r2
       jsr     r0,putlu / create link from last user on Q to u.uno's user
swap:
       mov     $300,*$ps / processor priority = 6
       mov     $runq,r2 / r2 points to runq table
1: / search runq table for highest priority process
       tst     (r2)+ / are there any processes to run in this Q entry
       bne     1f / yes, process 1f
       cmp     r2,$runq+6 / if zero compare address to end of table
       bne     1b / if not at end, go back
       jsr     r0,idle; s.idlet+2 / wait for interrupt; all queues
                                  / are empty
       br      swap
1:
       tst     -(r2) / restore pointer to right Q entry
       mov     r2,u.pri / set present user to this run queue
       movb    (r2)+,r1 / move 1st process in queue to r1
       cmpb    r1,(r2)+ / is there only 1 process in this Q to be run
       beq     1f / yes
       tst     -(r2) / no, pt r2 back to this Q entry
       movb    p.link-1(r1),(r2) / move next process in line into
                                 / run queue
       br      2f
1:
       clr     -(r2) / zero the entry; no processes on the Q
2: / write out core to appropriate disk area and read in new process if
   / required
       clr     *$ps / clear processor status
       cmpb    r1,u.uno / is this process the same as the process in core?
       beq     2f / yes, don't have to swap
       mov     r0,-(sp) / no, write out core; save r0 (address in rout.
                        / that called swap)
       mov     sp,u.usp / save stack pointer
       mov     $sstack,sp / move swap stack pointer to the stack pointer
       mov     r1,-(sp) / put r1 (new process #) on the stack
       tstb    u.uno / is the process # = 0
       beq     1f / yes, kill process by overwriting
       jsr     r0,wswap / write out core to disk
1: 
       mov     (sp)+,r1 / restore r1 to new process number
       jsr     r0,rswap / read new process into core
       jsr     r0,unpack / unpack the users stack from next to his program
                         / to its normal
       mov     u.usp,sp / location; restore stack pointer to new process
                        / stack
       mov     (sp)+,r0 / put address of where the process that just got
                        / swapped in, left off., i.e., transfer control
                        / to new process
2:
       movb    $30.,uquant / initialize process time quantum
       rts     r0 / return

wswap:
       mov     *$30,u.emt / determines handling of emts
       mov     *$10,u.ilgins / determines handling of illegal instructions
       mov     u.break,r2 / put process program break address in r2
       inc     r2 / add 1 to it 
       bic     $1,r2 / make it even
       mov     r2,u.break / set break to an even location
       mov     u.usp,r3 / put users stack pter at moment of swap in r3
       cmp     r2,$core / is u.break less than $core
       blos    2f / yes
       cmp     r2,r3 / no, is (u.break) greater than stack pointer
       bhis    2f / yes
1:
       mov     (r3)+,(r2)+ / no, pack stack next to users program
       cmp     r3,$ecore / has stack reached end of core
       bne     1b / no, keep packing
       br      1f / yes
2: 
       mov     $ecore,r2 / put end of core in r2 
1:
       sub     $user,r2 / get number of bytes to write out (user up
                        / to end of stack gets written out)
       neg     r2 / make it negative
       asr     r2 / change bytes to words (divide by 2)
       mov     r2,swp+4 / word count
       movb    u.uno,r1 / move user process number to r1
       asl     r1 / x2 for index
       mov     r2,p.break-2(r1) / put negative of word count into the
       	                        / p.break table
       mov     p.dska-2(r1),r1 / move disk address of swap area for
                               / process to r1
       mov     r1,swp+2 / put processes dska address in swp +2 (block
                        / number)
       bis     $1000,swp / set it up to write (set bit 9)
       jsr     r0,ppoke / write process out on swap area of disk
1:
       tstb    swp+1 / is lt done writing?
       bne     1b / no, wait
       rts     r0 / yes, return to swap

rswap:
       asl     r1 / process number x2 for index
       mov     p.break-2(r1), swp+4 / word count
       mov     p.dska-2(r1),swp+2 / disk address
       bis     $2000,swp / read
       jsr     r0,ppoke / read it in 
1:
       tstb    swp+1 / done
       bne     1b / no, wait for bit 15 to clear (inhibit bit)
       mov     u.emt,*$30 / yes move these
       mov     u.ilgins,*$10 / back
       rts     r0 / return

unpack: / move stack back to its normal place
       mov     u.break,r2 / r2 points to end of user program

       cmp     r2,$core / at beginning of user program yet?
       blos    2f / yes, return
       cmp     r2,u.usp / is break_above the "stack pointer before
                        / swapping"
       bhis    2f / yes, return
       mov     $ecore,r3 / r3 points to end of core
       add     r3,r2
       sub     u.usp,r2 / end of users stack is in r2
1:
       mov     -(r2),-(r3) / move stack back to its normal place
       cmp     r2,u.break / in core
       bne     1b
2:
       rts     r0

putlu: / r1 = user process no.; r2 points to lowest priority queue
       tstb    (r2)+ / is queue empty?
       beq     1f / yes, branch
       movb    (r2),r3 / no, save the "last user" process number in r3
       movb    r1,p.link-1(r3) / put pointer to user on "last users" link
       br      2f /
1:
       movb    r1,-1(r2) / user is only user; put process no. at beginning
                         / and at end
2: 
       movb    r1,(r2) / user process in r1 is now the last entry on
                       / the queue
       dec     r2 / restore r2
       rts     r0

copyz:
       mov     r1,-(sp) / put r1 on stack
       mov     r2,-(sp) / put r2 on stack
       mov     (r0)+,r1
       mov     (r0)+,r2
1:
       clr     (r1)+ / clear all locations between r1 and r2
       cmp     r1,r2 
       blo     1b
       mov     (sp)+,r2 / restore r2
       mov     (sp)+,r1 / restore r1
       rts     r0 

idle:
       mov	*$ps,-(sp) / save ps on stack
       clr	*$ps / clear ps
       mov	clockp,-(sp) / save clockp on stack
       mov	(r0)+,clockp / arg to idle in clockp
       1 / wait for interrupt
       mov      (sp)+,clockp / restore clockp, ps
       mov	(sp)+,*$ps
       rts	r0

clear:
       jsr       r0,wslot / get an I/O buffer set bits 9 and 15 in first
                          / word of I/O queue r5 points to first data word
       
       
                               / in buffer
       mov     $256.,r3
1: 
       clr     (r5)+ / zero data word in buffer
       dec     r3
       bgt     1b / branch until all data words in buffer are zero
       jsr	r0,dskwr / write zeroed buffer area out onto physical
                         / block specified
       rts     r0 / in r1


