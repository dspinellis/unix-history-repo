" S1

.. = 0
t = 0
orig:
   hlt
   jmp pibreak

. = orig+7
   -1

. = orig+020
   1f
   iof
   dac u.ac
   lac 020
   dac 1f
   lac 1f-1
   dac 020
   lac u.ac
   jmp 1f+1
   1f
1: 0
   iof
   dac u.ac
   lacq
   dac u.mq
   lac 8
   dac u.rq
   lac 9
   dac u.rq+1
   jms copy; 10; u.rq+2; 6
   lac 1b
   dac u.rq+8
   -1
   dac .savblk
   dac .insys
   lac uquant
   jms betwen; d0; maxquant
      jms swap
   ion
   -1
   tad u.rq+8
   jms laci
   jms betwen; o20001; swn
      jmp badcal
   tad swp
   dac .+1
   jmp .. i

. = orig+0100
   jmp coldentry
   jms halt

okexit:
   dzm u.ac
sysexit:
   ion
   lac .savblk
   sza
   jmp 1f
   jms copy; sysdata; dskbuf; 64
   cla
   jms dskio; 07000
1:
   dzm .insys
   jms chkint
      skp
   jmp .save
   jms copy; u.rq+2; 10; 6
   lac u.rq+1
   dac 9
   lac u.rq
   dac 8
   lac u.mq
   lmq
   lac u.ac
   jmp u.rq+8 i

swap: 0
   ion
1:
   jms lookfor; 3 " out/ready
      jmp 1f
   jms lookfor; 1 " in/ready
      skp
   jmp 1b
   dzm maxquant
   jmp 3f
1:
   dac 9f+t
   jms lookfor; 2 " in/notready
      jmp 1f
   jms lookfor; 1 " in/ready
      jmp 1f
   jmp 2f
1:
   lac swap
   dac u.swapret
   iof
   lac o200000
   tad u.ulistp i
   dac u.ulistp i
   ion
   jms dskswap; 07000
   lac u.dspbuf
   sna
   jmp 2f
   law dspbuf
   jms movdsp
2:
   iof
   lac o600000
   tad 9f+t i
   dac 9f+t i
   ion
   jms dskswap; 06000
   lac u.swapret
   dac swap
   lac o20
   dac maxquant
   lac u.dspbuf
   sza
   jms movdsp
3:
   dzm uquant
   iof
   jmp swap i
t = t+1

swp:
   jmp .
   .save; .getuid; .open; .read; .write; .creat; .seek; .tell
   .close; .link; .unlink; .setuid; .rename; .exit; .time; .intrp
   .chdir; .chmod; .chown; badcal; .sysloc; badcal; .capt; .rele
   .status; badcal; .smes; .rmes; .fork
swn:
   .-swp-1 i

.intrp:
   lac u.ac
   dac u.intflg
   jmp okexit

.sysloc:
   lac u.ac
   and o17777
   jms betwen; d1; locn
      jms error
   tad locsw
   dac .+1
   lac ..
   dac u.ac
   jmp sysexit

locsw:
   lac .
   iget; inode; userdata; sysdata; copy; copyz; betwen; dskrd
   dskwr; dskbuf; dpdata; namei; pbsflgs; alloc; free; dspdata
   crdata
locn:
   .-locsw-1

chkint: 0
   lac .insys
   sza
   jmp chkint i
   lac .int1
   sna
   jmp 1f
   sad u.ofiles+2
   jmp 2f
1:
   lac .int2
   sna
   jmp chkint i
   sad u.ofiles+2
   skp
   jmp chkint i
   dzm .int2
   jmp 1f
2:
   dzm .int1
1:
   lac u.intflg
   sza
   jmp chkint i
   -1
   dac .insys
   ion
   isz chkint
   jmp chkint i

