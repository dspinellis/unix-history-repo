/
/

/ as9 -- PDP-11 assembler pass 2

eae = 0

	.data
symtab:

/ special variables

dotrel: 02; dot:000000 /.
 01; dotdot:000000 /..

/ register

24;000000 /r0
24;000001 /r1
24;000002 /r2
24;000003 /r3
24;000004 /r4
24;000005 /r5
24;000006 /sp
24;000007 /pc


.if eae
/eae & switches

01;177570 /csw
01;177300 /div
01;177302 /ac
01;177304 /mq
01;177306 /mul
01;177310 /sc
01;177311 /sr
01;177312 /nor
01;177314 /lsh
01;177316 /ash

.endif

/ system calls

01;0000001 /exit
01;0000002 /fork
01;0000003 /read
01;0000004 /write
01;0000005 /open
01;0000006 /close
01;0000007 /wait
01;0000010 /creat
01;0000011 /link
01;0000012 /unlink
01;0000013 /exec
01;0000014 /chdir
01;0000015 /time
01;0000016 /makdir
01;0000017 /chmod
01;0000020 /chown
01;0000021 /break
01;0000022 /stat
01;0000023 /seek
01;0000024 /tell
01;0000025 /mount
01;0000026 /umount
01;0000027 /setuid
01;0000030 /getuid
01;0000031 /stime
01;0000034 /fstat
01;0000036 /mdate
01;0000037 /stty
01;0000040 /gtty
01;0000042 /nice
01;0000060 /signal

/ double operand

13;0010000 /mov
13;0110000 /movb
13;0020000 /cmp
13;0120000 /cmpb
13;0030000 /bit
13;0130000 /bitb
13;0040000 /bic
13;0140000 /bicb
13;0050000 /bis
13;0150000 /bisb
13;0060000 /add
13;0160000 /sub

/ branch

06;0000400 /br
06;0001000 /bne
06;0001400 /beq
06;0002000 /bge
06;0002400 /blt
06;0003000 /bgt
06;0003400 /ble
06;0100000 /bpl
06;0100400 /bmi
06;0101000 /bhi
06;0101400 /blos
06;0102000 /bvc
06;0102400 /bvs
06;0103000 /bhis
06;0103000 /bec
06;0103000 /bcc
06;0103400 /blo
06;0103400 /bcs
06;0103400 /bes

/ jump/ branch type

35;0000400 /jbr
36;0001000 /jne
36;0001400 /jeq
36;0002000 /jge
36;0002400 /jlt
36;0003000 /jgt
36;0003400 /jle
36;0100000 /jpl
36;0100400 /jmi
36;0101000 /jhi
36;0101400 /jlos
36;0102000 /jvc
36;0102400 /jvs
36;0103000 /jhis
36;0103000 /jec
36;0103000 /jcc
36;0103400 /jlo
36;0103400 /jcs
36;0103400 /jes

/ single operand

15;0005000 /clr
15;0105000 /clrb
15;0005100 /com
15;0105100 /comb
15;0005200 /inc
15;0105200 /incb
15;0005300 /dec
15;0105300 /decb
15;0005400 /neg
15;0105400 /negb
15;0005500 /adc
15;0105500 /adcb
15;0005600 /sbc
15;0105600 /sbcb
15;0005700 /tst
15;0105700 /tstb
15;0006000 /ror
15;0106000 /rorb
15;0006100 /rol
15;0106100 /rolb
15;0006200 /asr
15;0106200 /asrb
15;0006300 /asl
15;0106300 /aslb
15;0000100 /jmp
15;0000300 /swab

/ jsr

07;0004000 /jsr

/ rts

10;000200 /rts

/ simple operand

11;104400 /sys

/ flag-setting

01;0000241 /clc
01;0000242 /clv
01;0000244 /clz
01;0000250 /cln
01;0000261 /sec
01;0000262 /sev
01;0000264 /sez
01;0000270 /sen

/ floating point ops

01;170000 / cfcc
01;170001 / setf
01;170011 / setd
01;170002 / seti
01;170012 / setl
15;170400 / clrf
15;170700 / negf
15;170600 / absf
15;170500 / tstf
12;172400 / movf
14;177000 / movif
05;175400 / movfi
14;177400 / movof
05;176000 / movfo
14;172000 / addf
14;173000 / subf
14;171000 / mulf
14;174400 / divf
14;173400 / cmpf
14;171400 / modf
14;176400 / movie
05;175000 / movei
15;170100 / ldfps
15;170200 / stfps
24;000000 / fr0
24;000001 / fr1
24;000002 / fr2
24;000003 / fr3
24;000004 / fr4
24;000005 / fr5

/ 11/45 operations

30;072000 /als (ash)
30;073000 /alsc (ashc)
30;070000 /mpy
.if eae-1
30;070000/ mul
30;071000 / div
30;072000 / ash
30;073000 /ashc
.endif
30;071000 /dvd
07;074000 /xor
15;006700 /sxt
11;006400 /mark
31;077000 /sob

/ specials

16;000000 /.byte
20;000000 /.even
21;000000 /.if
22;000000 /.endif
23;000000 /.globl
25;000000 /.text
26;000000 /.data
27;000000 /.bss
32;000000 /.comm

start:
	cmp	(sp),$4
	bge	1f
	jmp	aexit
1:
	cmp	(sp)+,$5
	blt	1f
	mov	$40,defund		/ globalize all undefineds
1:
	tst	(sp)+
	mov	(sp)+,a.tmp1
	mov	(sp)+,a.tmp2
	mov	(sp)+,a.tmp3
	jsr	r5,ofile; a.tmp1
	movb	r0,txtfil
	jsr	r5,ofile; a.tmp2
	movb	r0,fbfil
	jsr	r5,ofile; a.tmp3
	movb	r0,symf
	movb	r0,fin
	sys	creat; a.out; 0
	bec	1f
	jsr	r5,filerr; a.outp
1:
	movb	r0,fout
	jmp	go

/ overlaid buffer
inbuf	= start
.	= inbuf+512.
