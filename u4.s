/ u4 -- unix

setisp:
       mov     r1,-(sp)
       mov     r2,-(sp)
       mov     r3,-(sp)
       mov     clockp,-(sp)
       mov     $s.syst+2,clockp
       jmp     (r0)

clock: / interrupt from 60 cycle clock
       mov     r0,-(sp) / save r0
       tst     *$lks / restart clock?
       mov     $s.time+2,r0 / increment the time of day
       inc     (r0)
       bne     1f
       inc     -(r0)
1:
       mov     clockp,r0 / increment appropriate time category
       inc     (r0)
       bne     1f
       inc     -(r0)
1:
       mov     $uquant,r0 / decrement user time quantum
       decb    (r0)
       bge     1f / if less than 0
       clrb    (r0) / make it 0
1: / decrement time out counts return now if priority was not 0
       cmp     4(sp),$200 / ps greater than or equal to 200
       bge     2f / yes, check time outs
       tstb    (r0) / no, user timed out?
       bne     1f / no
       cmpb    sysflg,$-1 / yes, are we outside the system?
       bne     1f / no, 1f
       mov     (sp)+,r0 / yes, put users r0 in r0
       sys     0 / sysrele
       rti
2: / priority is high so just decrement time out counts
       mov     $toutt,r0 / r0 points to beginning of time out table
2:
       tstb    (r0) / is the time out?
       beq     3f / yes, 3f (get next entry)
       decb    (r0) / no, decrement the time
       bne     3f / isit zero now?
       incb    (r0) / yes, increment the time
3:
       inc     r0 / next entry
       cmp     r0,$touts / end of toutt table?
       blo     2b / no, check this entry
       mov     (sp)+,r0 / yes, restore r0
       rti / return from interrupt
1: / decrement time out counts; if 0 call subroutine
       mov     (sp)+,r0 / restore r0
       mov     $240,*$ps / set processor priority to 5
       jsr     r0,setisp / save registers
       mov     $touts-toutt-1,r0 / set up r0 as index to decrement thru
                                 / the table
1:
       tstb    toutt(r0) / is the time out for this entry
       beq     2f / yes
       decb    toutt(r0) / no, decrement the time
       bne     2f / is the time 0, now
       asl     r0 / yes, 2 x r0 to get word index for tout entry
       jsr     r0,*touts(r0) / go to appropriate routine specified in this
       asr     r0 / touts entry; set r0 back to toutt index
2:
       dec     r0 / set up r0 for next entry
       bge     1b / finished? , no, go back
       br      retisp / yes, restore registers and do a rti
ttyi: / console tty input interrupt routine
       jsr     r0,setisp / save reg r1, r2, r3
       mov     *$tkb,r1 / r1 = char in tty reader buffer
       inc     *$tks / set the reader enable bit
       bic     $!177,r1 / clear upper 9 bits of the character (strip off
                        / 8th bit of char)
       cmp     r1,$'a-40 / is character upper case A,..., upper case Z.
                         / note that
       blt     1f / lower case a is represented by 141, upper case by
       cmp     r1,$'z-40 / 101; and lower case z by 172, upper
                         / case Z by 132.
       bgt     1f / if not upper case, branch
       add     $40,r1 / if upper case, calculate the representation of its
                      / lower case counter part
1:
       cmp     r1,$175 / char = "}"? Note: may be quit char (fs)
       beq     2f / yes 2f
       cmp     r1,$177 / char = "del" ?
       beq     2f / yes, 2f
       jsr     r0,putc; 0 / put char in r1 on clist entry
               br 1f
       movb    r1,ttyoch / put char in ttyoch
       jsr     r0,startty / load char in tty output data buffer
       cmp     r1,$4 / r1 = "eot"
       beq     1f / yes, 1f
       cmp     r1,$12 / r1 = "lf"
       beq     1f / yes 1f
       cmpb    cc+0,$15. / are there less than 15 chars on the input list
       blo     retisp / yes, return
1:
       jsr     r0,wakeup; runq; 0 / no, wakeup the input process
       br      retisp / return
2: / r1 = "}" or "delete" to get here
       mov     tty+[ntty*8]-8+6,r2 / move console tty buffer address to r2
       beq     2f / if 0, wakeall
       movb    r1,6(r2) / move "}" or del into "interrupt char"
                        / byte of buffer
2:
       jsr     r0,wakeall / wakeup all sleeping processes
       br      retisp / return
       
       
wakeall:
       mov     $39.,0f / flll arg2 of wakeup call wlth 39
1:
       jsr     r0,wakeup; runq+4; 0:.. / wakeup the processes in the
       dec     0b                      / wait list; decrement arg2
       bge     1b / if not done, go back
       rts     r0

ttyo: / console typewriter output interrupt routine
       jsr     r0,setisp / save registers
       jsr     r0,startty / put a char on the console tty output buffer
       br      retisp / restore registers

retisp:
       mov     (sp)+,clockp / pop values before interrupt off the stack
       mov     (sp)+,r3
       mov     (sp)+,r2
       mov     (sp)+,r1
       mov     (sp)+,r0
       rti     / return from interrupt

ppti: / paper tape lnput interrupt routine
       jsr     r0,setisp / save registers
       movb    pptiflg,r1 / place "pptiflg" in r1
       jmp     *1f(r1) / jump to location speclfled by value of "pptiflg"
1:
       retisp / file not open
       1f / file just opened
       2f / file normal
       retisp / file not closed

1: / file just opened
       tstb    *$prs+1 / is error bit set in prs
       bge     1f / no
       jsr     r0,pptito / place 10 in toutt entry for ppt input
       br      retisp
1:
       movb    $4,pptiflg / change "pptiflg" to indicate file "normal"
2:
       jsr     r0,wakeup; runq+2; 2 / wakeup process for ppt input entry
                                    / in wlist
       tstb    *$prs+1 / is error bit set
       blt     1f / yes
       mov     *$prb,r1 / place contents ppt read buffer in r1
       jsr     r0,putc; 2 / place character in clist area for ppt input
               br .+2 / temp / if no space in clist character lost
       cmpb    cc+2,$50. / character count in clist area for ppt lnput
                         / greater than or equal to 50
       bhis    retisp / yes
       inc     *$prs / no, set reader enable bit in prs
       br      retisp
1:
       movb    $6,pptiflg / set pptiflg to 6 to indicate error bit set
       br      retisp

/lpto:
       
       
/      jsr     r0,setisp
/      jsr     r0,starlpt
/      br      retisp
ppto: / paper tape output interrupt routine
       jsr r0,setisp / save registers
       jsr r0,starppt / get next character from clist, and output
       / if possible
       br retisp / pop register values from stack

/ starlpt:
/       cmpb   cc+5.,$100.
/       bhi    1f
/       jsr    r0,wakeup; runq+2; 5
/1:
/       tstb   *$lps
/       bge    1f
/       jsr    r0,getc; 5
/              br 1f
/       mov    r1,*$lpb
/       br     starlpt
/1:
/       rts    r0

startty: / start or restart console tty output
       cmpb    cc+1,$5.
       bhi     1f / branch to 1f when character count on tty (? input,
                  / output) list is greater than 5.
       jsr     r0,wakeup; runq+2; 1
1:
       tstb    *$tps / test console output ready bit
       bge     2f / branch if ready bit is clear
       tstb    toutt+0 / is toutt for console a zero
       bne     2f / if not; branch to 2f
       movb    ttyoch,r1 / put character to be output in r1
       bne     1f
       jsr     r0,getc; 1 / if char is nul, get a char from console
                          / output list
               br 2f / if console output list is empty, branch to 2f
1:
       clrb    ttyoch
       mov     r1,*$tpb / put character in console output register
       cmp     r1,$12 / is char a line feed
       bne     1f
       movb    $15,ttyoch / put a cr in ttyoch
1:
       cmp     r1,$11 / char = ht
       bne     1f
       movb    $15.,toutt+0 / set time out to 15 clock tics
1:
       cmp     r1,$15 / char = cr
       bne     2f
       movb    $15.,toutt+0 / set time out to 15 clock ticks
2:
       rts     r0
pptito: / paper tape input touts subrouting
       cmpb    pptiflg,$2 / does "pptiflg" indicate file just opened
       bne     1f / no, do nothing pyf
       movb    $10.,toutt+1 / yes, place 10 in tout entry for tty input
       tstb    *$prs+1 / is error bit set
       blt     1f / yes, return
       inc     *$prs / no, set read enable bit
1:
       rts     r0

starppt: / start ppt output
       cmpb    cc+3,$10. / is character count for ppt output greater
                         / than 10.
       bhi     1f / yes, branch
       jsr     r0,wakeup; runq+2; 3 / no, wakeup process in wlist
                                    / entry for ppt input
1:
       tstb    *$pps / is ready bit set in punch status word
       bge     1f / no, branch
       jsr     r0,getc; 3 / yes, get next char in clist for pptout and
       	                  / place in r1
       br      1f / if none, branch
       mov     r1,*$ppb / place character in ppt buffer
1:
       rts r0

wakeup: / wakeup processes waiting for an event by linking them to the
       / queue
       mov     r1,-(sp) / put char on stack
       mov     (r0)+,r2 / r2 points to a queue
       mov     (r0)+,r3 / r3 = wait channel number
       movb    wlist(r3),r1 / r1 contains process number in that wait
                            / channel that was sleeping
       beq     2f / if 0 return, nothing to wakeup
       cmp     r2,u.pri / is runq greater than or equal to users process
       	                / priority
       bhis    1f / yes, don't set time quantum to zero
       clrb    uquant / time quantum = 0
1:
       clrb    wlist(r3) / zero wait channel entry
       jsr     r0,putlu / create a link from the last user on the Q
                        / to this process number that got woken
2:
       mov     (sp)+,r1 / restore r1
       rts     r0

sleep: / wait for event
       jsr     r0,isintr / check to see if interrupt or quit from user
               br 2f / something happened / yes, his interrupt so return
                     / to user
       mov     (r0)+,r1 / put number of wait channel in r1
       movb    wlist(r1),-(sp) / put old process number in there, on
                               / the stack
       movb    u.uno,wlist(r1) / put process number of process to put
                              / to sleep in there
       mov     cdev,-(sp) / nothing happened in isintr so
       jsr     r0,swap / swap out process that needs to sleep
       mov     (sp)+,cdev / restore device
       jsr     r0,isintr / check for interrupt of new process
               br      2f / yes, return to new user
       movb    (sp)+,r1 / no, r1 = old process number that was originally
                        / on the wait channel
       beq     1f / if 0 branch
       mov     $runq+4,r2 / r2 points to lowest priority queue
       mov     $300,*$ps / processor priority = 6
       jsr     r0,putlu / create link to old process number
       clr     *$ps / clear the status; process priority = 0
1:
       rts     r0 / return
2:
       jmp     sysret / return to user

isintr:
       mov     r1,-(sp) / put number of wait channel on the stack
       mov     r2,-(sp) / save r2
       mov     u.ttyp,r1 / r1 = pointer to buffer of process control
                         / typewriter
       beq     1f / if 0, do nothing except skip return
       movb    6(r1),r1 / put interrupt char in the tty buffer in r1
       beq     1f / if its 0 do nothing except skip return
       cmp     r1,$177 / is interrupt char = delete?
       bne     3f / no, so it must be a quit (fs)
       tst     u.intr / yes, value of u.intr determines handling
                      / of interrupts
       bne     2f / if not 0, 2f. If zero do nothing.
1:
       tst     (r0)+ / bump r0 past system return (skip)
4:
       mov     (sp)+,r2 / restore r1 and r2
       mov     (sp)+,r1
       rts     r0
3: / interrupt char = quit (fs)
       tst     u.quit / value of u.quit determines handling of quits
       beq     1b / u.quit = 0 means do nothing
2: / get here because either u.intr <> 0 or u.qult <> O
       mov     $tty+6,r1 / move pointer to tty block into r1
1: / find process control tty entry in tty block
       cmp     (r1),u.ttyp / is this the process control tty buffer?
       beq     1f / block found go to 1f
       add     $8,r1 / look at next tty block
       cmp     r1,$tty+[ntty*8]+6 / are we at end of tty blocks
       blo     1b / no
       br      4b / no process control tty found so go to 4b
1:
       mov     $240,*$ps / set processor priority to 5
       movb    -3(r1),0f / load getc call argument; character llst
                          / identifier
       inc     0f / increment
1:
       jsr     r0,getc; 0:.. / erase output char list for control
               br 4b / process tty. This prevents a line of stuff
                     / being typed out after you hit the interrupt
                     / key
       br      1b
