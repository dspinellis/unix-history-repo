/ u7 -- unix

canon:
        mov    r5,r1 / move tty buffer address to r1
        add    $10.,r1 / add 10 to get start of data
        mov    r1,4(r5) / canp = 10(r5) / move buffer addr + 10 to 3rd
                        / word in buffer (char. pointer)
        clr    2(r5) / ncan / clear 2nd word in buffer, 0 char. count
1:
        jsr    r0,*(r0) / jump to arg get char off Q of characters, sleep
                        / if none
        jsr    r0,cesc; 100 / test for @ (kill line)
               br canon / character was @ so start over
        jsr    r0,cesc; 43 / test for # (erase last char. typed)
               br 1b / character was #, go back
        cmp    r1,$4 / is char eot?
        beq    1f / yes, reset and return
        movb   r1,*4(r5) / no, move char to address in 3rd word of buffer
                         / (char. pointer)
        inc    2(r5) / increment 2nd word (char. count)
        inc    4(r5) / increment 3rd word (char. pointer)
        cmp    r1,$'\n / is char = newline
        beq    1f / yes, 1f
        cmp    2(r5),$120. / is byte count greater than or equal to 120
        bhis   1f / yes, 1f
        br     1b / no, get another char off the Q
1: / get here if line is full, a new line has been received or an eot
   / has been received
        mov    r5,r1 / move buffer address to r1
        add    $10.,r1 / add 10
        mov    r1,4(r5) / canp = 10(r5) / reset char pointer
        tst    (r0)+ / skip over argument
        rts    r0 / return

cesc: / test for erase or kill char
        cmp    r1,(r0)+ / char in r1 = erase or kill character?
        bne    1f / no, skip return
        tst    2(r5) / yes, is char. count = 0
        beq    2f / yes, don't skip return
        dec    2(r5) / no, decrement char count
        dec    4(r5) / decrement character pointer
        cmpb   *4(r5),$'\\/ was previous character a "\"
        bne    2f / no, don't skip
1:
        tst    (r0)+ / yes, skip
2:
        rts    r0 / return

ttych: / get characters from Q of characters inputted to tty
        mov    $240,*$ps / set processor priority to 5
        jsr    r0,getc; 0 / takes char. off clist and puts it in r1
               br 1f / list is empty, go to sleep
        clr    *$ps / clear process priority
        rts    r0 / return
1: / list is empty
        mov    r5,-(sp) / save r5
        jsr    r0,sleep; 0 / put process to sleep in input wait channel
        mov    (sp)+,r5 / restore r5
        br     ttych / try again

pptic: / paper tape input control
        mov    $240,*$ps / set processor priority to five
        cmpb   cc+2,$30. / is character count for paper tape input in
                         / clist greater than or equal to 30
        bhis   1f / yes, branch
        bit    *$prs,$104200 / is there either an error, an unread char
                             / in buffer, or reader busy
        bne    1f / yes, don't enable reader
        inc    *$prs / set reader enable bit
1:
        jsr    r0,getc; 2 / get next character in clist for ppt input and
               br 1f / place in r1; if no char in clist for ppt input
                     / branch
        tst    (r0)+ / pop stack so that return will be four locations past
                     / subroutine call
2:
        clr    *$ps / set process priority equal to zero
        rts    r0 / return
1:                                                     
        cmpb   pptiflg,$6 / does pptiflg indicate file "not closed"
        beq    2b / yes, return to calling routine at instruction
                  / immediately following jsr
        jsr    r0,sleep; 2 / no, all characters to be read in not yet in
                           / clist, put process to sleep
        br     pptic

pptoc: / paper tape output control
        mov    $240,*$ps / set processor priority to five
        cmpb   cc+3,$50. / is character count for paper tape output in
                         / clist greater than or equal to 50
        bhis   1f / yes
        jsr    r0,putc; 3 / find place in freelist to assign ppt output
                          / and place
               br 1f / character in list; if none available branch to put
                     / process to sleep
        jsr    r0,starppt / try to output character
        clr    *$ps / clear processor priority
        rts    r0 / return
1:
        mov    r1,-(sp) / place character on stack
        jsr    r0,sleep; 3 / put process to sleep
        mov    (sp)+,r1 / place character in r1
        br     pptoc / try again to place character in clist and output

/lptoc: / line printer output control
/       mov    $240,*$ps / set processor priority to five
/       cmpb   cc+5,$200. / is character count for printer greater than or
                          / equal to 200
/       bhis   1f / yes
/       jsr    r0,putc; 5 / find place in freelist to assign to printer
                          / and place

               br 1f / char in list, if none available branch to put
                     / process to sleep
/       jsr    r0,starlpt / try to output character
/       clr    *$ps / set processor priority = 0
/       rts    r0 / return
/1:
/       mov    r1,-(sp) / place character on stack
/       jsr    r0,sleep; 5 / put process to sleep
/       mov    (sp)+,r1 / place character on stack
/       br     lptoc

getc: / get a character off character list
        mov    (r0)+,r1 / put argument in getc call in r1 (char list id)
        jsr    r0,get
               br 1f / empty char list return
        decb   cc(r1) / decrement number of char in char list
        mov    $-1,r1 / load minus 1 in r1
        jsr    r0,put / put char back on free list
        movb   clist-2(r2),r1 / put char in r1
        tst    (r0)+ / bump r0 for non blank char list return
1:
        rts    r0

putc:
        mov    r1,-(sp) / save char on stack
        mov    $-1,r1 / put free list list id in r1
        jsr    r0,get / take char off free list / clist slot taken
                      / identified by r2
               br 1f / branch when no chars in free list
        mov    (r0)+,r1 / put putc call arg in r1 (i.e., list identifier)
        incb   cc(r1) / increment character count for list (r1)
        jsr    r0,put / put clist entry on list
        movb   (sp),clist-2(r2) / put character in new entry
1:
        tst    (r0)+
        mov    (sp)+,r1
        rts    r0

get:
        movb   cf+1(r1),r2 / move current first char offset to r2
        beq    2f / no characters in char list
        tst    (r0)+ / bump r0, second return
        cmpb   r2,cl+1(r1) / r2 equal to last char offset
        beq    1f / yes, (i.e., entire char list scanned), branch to 1f
        bic    $!377,r2 / clear bits 8-15 in r2
        asl    r2 / multiply r2 by 2 to get offset in clist
        movb   clist-1(r2),cf+1(r1) / move next char in list pointer to
                                    / first char offset ptr
        br     2f
1:
        clrb   cf+1(r1) / clear first char clist offset
        clrb   cl+1(r1) / clear last char clist offset
        bic    $!377,r2 / zero top half of r2
        asl    r2 / multiply r2 by 2
2:
        rts    r0

put:
        asr    r2 / divide r2 by 2; r2 is offset in clist
        mov    r2,-(sp) / save r2 on stack
        movb   cl+1(r1),r2 / move offset of last char in list (r1) into r2
        beq    1f / offset = 0 then go to 1f (i.e., start a new list)
        bic    $!377,r2 / zero top half of r2
        asl    r2 / multiply offset by 2, r2 now has offset in clist
        movb   (sp),clist-1(r2) / link new list entry to current last
                                / entry in list (r1)
        br     2f
1:
        movb   (sp),cf+1(r1) / put new list entry offset into first char
                             / offset of list (r1)
2:
        mov    (sp)+,r2 / pop stack into r2; offset of new list
                        / entry in r2
        movb   r2,cl+1(r1) / make new list entry the last entry in list
                           / (r1)
        asl    r2 / multiply r2 by 2; r2 has clist offset for new
                  / list entry
        rts    r0

iopen: / open file whose i-number is in r1
        tst    r1 / write or read access?
        blt    2f / write, go to 2f
        jsr    r0,access; 2 / get inode into core with read access
        cmp    r1,$40. / is it a special file
        bgt    3f / no. 3f
        mov    r1,-(sp) / yes, figure out
        asl    r1
        jmp    *1f-2(r1) / which one and transfer to it
1:
        otty   / tty
        oppt   / ppt
        sret   / mem
        sret    / rf0
        sret   / rk0
        sret   / tap0
        sret   / tap1
        sret   / tap2
        sret   / tap3
        sret   / tap4
        sret   / tap5
        sret   / tap6
        sret   / tap7
        ocvt   / tty0
        ocvt   / tty1
        ocvt   / tty2
        ocvt   / tty3
        ocvt   / tty4
        ocvt   / tty5
        ocvt   / tty6
        ocvt   / tty7
        error / crd

2: / check open write access
        neg   r1 / make inode number positive
        jsr    r0,access; 1 / get inode in 0 core
        bit    $40000,i.flgs / is it a directory?
        bne    2f / yes, transfer (error)
        cmp    r1,$40. / no, is it a special file?
        bgt    3f / no, return
        mov    r1,-(sp) / yes
        asl    r1
        jmp    *1f-2(r1) / figure out which special file it is
                         / and transfer
1:
        otty   / tty
        leadr  / ppt
        sret   / mem
        sret   / rf0
        sret / rk0
        sret   / tap0
        sret   / tap1
        sret   / tap2
        sret   / tap3
        sret   / tap4
        sret   / tap5
        sret   / tap6
        sret   / tap7
        ocvt   / tty0
        ocvt   / tty1
        ocvt   / tty2
        ocvt   / tty3
        ocvt   / tty4
        ocvt   / tty5
        ocvt   / tty6
        ocvt   / tty7
/       ejec / lpr

otty: / open console tty for reading or writing
        mov    $100,*$tks / set interrupt enable bit (zero others) in
                          / reader status reg
        mov    $100,*$tps / set interrupt enable bit (zero others) in
                          / punch status reg
        mov    tty+[ntty*8]-8+6,r5 / r5 points to the header of the
                                   / console tty buffer
        incb   (r5) / increment the count of processes that opened the
                    / console tty
        tst    u.ttyp / is there a process control tty (i.e., has a tty
                      / buffer header
        bne    sret / address been loaded into u.ttyp yet)?  yes, branch
        mov    r5,u.ttyp / no, make the console tty the process control
                         / tty
        br     sret / ?

sret:
        clr    *$ps / set processor priority to zero
        mov    (sp)+,r1 / pop stack to r1
3:
        rts    r0

oppt: / open paper tape for reading or writing
        mov    $100,*$prs / set reader interrupt enable bit
        tstb   pptiflg / is file already open
        bne    2f / yes, branch
1:
        mov    $240,*$ps / no, set processor priority to 5
        jsr    r0,getc; 2 / remove all entries in clist
               br .+4 / for paper tape input and place in free list
        br     1b
        movb   $2,pptiflg / set pptiflg to indicate file just open
        movb   $10.,toutt+1 / place 10 in paper tape input tout entry
        br     sret
2:
        jmp    error / file already open

iclose: / close file whose i-number is in r1
        tst    r1 / test i-number
        blt    2f / if neg., branch
        cmp    r1,$40. / is it a special file
        bgt    3b / no, return
        mov    r1,-(sp) / yes, save r1 on stack
        asl    r1
        jmp    *1f-2(r1) / compute jump address and transfer
1:
        ctty   / tty
        cppt   / ppt
        sret   / mem
        sret   / rf0
        sret   / rk0
        sret   / tap0
        sret   / tap1
        sret   / tap2
        sret   / tap3
        sret   / tap4
        sret   / tap5
        sret   / tap6
        sret   / tap7
        ccvt   / tty0
        ccvt   / tty1
        ccvt   / tty2
        ccvt   / tty3
        ccvt   / tty4
        ccvt   / tty5
        ccvt   / tty6
        ccvt   / tty7
        error / crd

2: / negative i-number
        neg    r1 / make it positive
        cmp    r1,$40. / is it a special file
        bgt    3b / no. return
        mov    r1,-(sp)
        asl    r1 / yes. compute jump address and transfer
        jmp    *1f-2(r1)
1:
        ctty   / tty
        leadr  / ppt
        sret   / mem
        sret   / rf0
        sret   / rk0
        sret   / tap0
        sret   / tap1
        sret   / tap2
        sret   / tap3
        sret   / tap4
        sret   / tap5
        sret   / tap6
        sret   / tap7
        ccvt   / tty0
        ccvt   / tty1
        ccvt   / tty2
        ccvt   / tty3
        ccvt   / tty4
        ccvt   / tty5
        ccvt   / tty6
        ccvt   / tty7
/       ejec / lpr

ctty: / close console tty
        mov    tty+[ntty*8]-8+6,r5 / point r5 to the console tty buffer
        decb   (r5) / dec number of processes using console tty
        br     sret / return via sret

cppt: / close paper tape
        clrb   pptiflg / set pptiflg to indicate file not open
1:
        mov    $240,*$ps /set process or priority to 5
        jsr    r0,getc; 2 / remove all ppt input entries from clist
                          / and assign to free list
               br sret
        br     1b

/ejec:
/       mov    $100,*$lps / set line printer interrupt enable bit
/       mov    $14,r1 / 'form feed' character in r1 (new page).
/       jsr    r0,lptoc / space the printer to a new page
/       br     sret / return to caller via 'sret'

leadr: / produce paper tape leader
        mov    $100,*$pps / set paper tape punch interrupt enable
        mov    $100.,-(sp) / 101. characters of 'nul' will be output as
                           / leader
1:
        clr    r1 / r1 contains a 'nul' character
        jsr    r0,pptoc / output the 'nul' character
        dec    (sp)
        bge    1b / last leader character output?  no, branch
        tst    (sp)+ / bump stack pointer
        br     sret / return to caller via 'sret'

sysmount: / mount file system; args special; name

        jsr    r0,arg2 / get arguments special and name
        tst    mnti / is the i-number of the cross device file zero?
        bne    errora / no, error
        jsr    r0,getspl / get special files device number in r1
        mov    (sp)+,u.namep / put the name of file to be placed on the
                             / device
        mov    r1,-(sp) / save the device number
        jsr    r0,namei / get the i-number of the file
               br errora
        mov    r1,mnti / put it in mnti
1:
        tstb   sb1+1 / is 15th bit of I/O queue entry for dismountable
                     / device set?
        bne    1b / (inhibit bit) yes, skip writing
        mov    (sp),mntd / no, put the device number in mntd
        movb   (sp),sb1 / put the device number in the lower byte of the
                        / I/O queue entry
        mov    (sp)+,cdev / put device number in cdev
        bis    $2000,sb1 / set the read bit
        jsr    r0,ppoke / read in entire file system
1:
        tstb   sb1+1 / done reading?
        bne    1b / no, wait
        br     sysreta / yes

sysumount: / special dismount file system
        jsr    r0,arg; u.namep / point u.namep to special
        jsr    r0,getspl / get the device number in r1
        cmp    r1,mntd / is it equal to the last device mounted?
        bne    errora / no error
1:
        tstb   sb1+1 / yes, is the device still doing I/O (inhibit
                     / bit set)?
        bne    1b / yes, wait
        clr    mntd / no, clear these
        clr    mnti
        br     sysreta / return

getspl: / get device number from a special file name
        jsr    r0,namei / get the i-number of the special file
               br errora / no such file
        sub    $4,r1 / i-number-4 rk=1,tap=2+n
        ble    errora / less than 0?  yes, error
        cmp    r1,$9. / greater than 9  tap 7
        bgt    errora / yes, error
        rts    r0 / return with device number in r1

errora:
        jmp    error / see 'error' routine

sysreta:
        jmp    sysret / see 'sysret' routine

