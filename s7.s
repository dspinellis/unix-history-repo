" s7

pibreak:

   dpsf
   jmp 1f

   dpcf
   dprs
   dac dpstat
   sma ral
   jmp 2f
   dprc
   dac dpchar
   -1
   dac dpread
   lac dpstat
   ral
2:
   sma
   jmp piret
   -1
   dac dpwrite

1: clsf
   jmp 1f

   lpb
   dac pbsflgs
   isz s.tim+1
   skp
   isz s.tim
   isz uquant
cnop:
   nop
   -1
   dac 7
   clon
   lac ttydelay
   spa
   isz ttydelay
   skp
   jms ttyrestart
   sna
   jmp piret
   isz .dsptm
   skp
   jmp dsprestart
   sad d3
   jmp piret
   isz .dspb
   jmp piret

1: dssf
   jmp 1f

   -1
   dac .dskb


   dsrs
   dac .dske
   dscs
   jmp piret

   sma ral
   jmp 1f
   cdf
   lac .dspb
   sna
   jmp piret
   tad dm3
   sna
   jmp dsprestart
   dac .dspb
   jmp piret
dsprestart:
   lac d1
   dac .dspb
   lac dspbufp
   beg
   -10
   dac .dsptm
   jmp piret

1: sna ral
   jmp .+3
   dpcf
   jmp piret
   sma
   jmp 1f
   lda
   dac .lpba
   rlpd
   jmp piret

1: ksf
   jmp 1f

   lac ttydelay
   sma
   isz ttydelay
   krb
   dac char
   sad o375
   jmp intrp1
   lac d1
   jms putchar
      dzm char
   lac sfiles+0
   jms wakeup
   dac sfiles+0
   lac char
   sad o212
   skp
   jmp piret
   lac sfiles+1
   sma
   xor o400000
   dac sfiles+1


   jms putcr
   jms ttyrestart
   jmp piret

1: tsf
   jmp 1f

   tcf
   jms ttyrestart
   jmp piret

ttyrestart: 0
   lac ttydelay
   spa
   jmp ttyrestart i
   lac nttychar
   dzm nttychar
   sza
   jmp 3f
   isz ttydelay
   lac d2
   jms getchar
      jmp 2f
3:
   tls
   sad o12
   jms putcr
   sad o15
   skp
   jmp ttyrestart i
   lac ttydelay
   tad o20
   rcr
   cma
   dac ttydelay
   jmp ttyrestart i
2:
   lac sfiles+1
   jms wakeup
   dac sfiles+1

   jmp 1f

   cck
   lck
   dac char
   sad o33
   jmp intrp2
   lac d3
   jms putchar
      nop
   lac sfiles+2
   jms wakeup
   dac sfiles+2
   jmp piret

1: rsf
   jmp 1f



   lac npptchar
   sna
   jmp .+5
   dac char
   rrb
   dac npptchar
   jmp .+3
   rrb
   dac char
3:
   lac char
   sna
   jmp 2f
   lac d4
   jms putchar
      jmp 3f
   lac char
   sad d4
   jmp 4f
2:
   lac npptchar
   sna
   jmp .+4
   dac char
   dzm npptchar
   jmp 3b
   rsa
   lac sfiles+3
   jms wakeup
   xor o400000
   dac sfiles+3
   jmp piret
3:
   lac char
   dac npptchar
4:
   lac sfiles+3
   jms wakeup
   dac sfiles+3
   jmp piret

1: psf
   jmp 1f

   pcf
   lac d5
   jms getchar
   jmp .+3
   psa
   jmp piret
   lac sfiles+4
   jms wakeup
   dac sfiles+4
   jmp piret

   jmp 1f

   cpb
   lpb
   dac pbsflgs+1


   and o2000
   sna
   jmp piret
   jms dspinit
   lac sfiles+6
   jms wakeup
   dac sfiles+6
   cla
   wbl

1: crsf
   jmp 1f

   crrb
   dac crchar
   -1
   dac crread
   jmp piret

1: crrb

piret:
   lac 0
   ral
   lac .ac
   ion
   jmp 0 i

wakeup: 0
   dac 9f+t
   -mnproc
   dac 9f+t+1
   lac tadu
   dac 2f
   lac dacu
   dac 2f+1
1:
   lac 9f+t
   ral
   dac 9f+t
   sma
   jmp 2f+2
   lac o700000
2: tad ..
   dac ..
   lac 2b
   tad d4
   dac 2b
   lac 2b+1
   tad d4
   dac 2b+1
   isz 9f+t+1
   jmp 1b
   cla
   jmp wakeup i
t = t+2

putcr: 0
   lac o15
   dac nttychar


   cla
   jmp putcr i

intrp1:
   lac d6
   dac .int1
   lac d1
   jms getchar
      skp
   jmp .-3
   lac d2
   jms getchar
      skp
   jmp .-3
   lac sfiles+0
   jms wakeup
   dac sfiles+0
   lac sfiles+1
   jms wakeup
   dac sfiles+1
   jms chkint
      jmp piret
   jmp 1f
intrp2:
   lac d7
   dac .int2
   lac d3
   jms getchar
      skp
   jmp .-3
   lac sfiles+2
   jms wakeup
   dac sfiles+2
   lac sfiles+6
   jms wakeup
   dac sfiles+6
   jms chkint
      jmp piret
1:
   lac 0
   dac 020
   lac .ac
   jmp 021
