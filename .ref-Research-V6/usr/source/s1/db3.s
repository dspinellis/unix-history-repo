/ db3 -- debugger

psym:
	mov	$2,incdot
	jsr	pc,prints
	jsr	pc,pnl
	rts	pc

prints:
	mov	r0,-(sp)
	mov	$optab,r1
1:
	mov	(sp),r0
	bic	(r1)+,r0
	cmp	(r1)+,r0
	bne	2f
	mov	(r1)+,-(sp)
	jsr	pc,string
	jmp	*(sp)+
2:
	add	$8.,r1
	br	1b

optab:
	107777; 010000; double; <mov\0\0\0>
	107777; 020000; double; <cmp\0\0\0>
	107777; 030000; double; <bit\0\0\0>
	107777; 040000; double; <bic\0\0\0>
	107777; 050000; double; <bis\0\0\0>
	007777; 060000; double; <add\0\0\0>
	007777; 160000; double; <su\0\0\0\0>

	100077; 005000; single; <clr\0\0\0>
	100077; 005100; single; <com\0\0\0>
	100077; 005200; single; <inc\0\0\0>
	100077; 005300; single; <dec\0\0\0>
	100077; 005400; single; <neg\0\0\0>
	100077; 005500; single; <adc\0\0\0>
	100077; 005600; single; <sbc\0\0\0>
	100077; 005700; single; <tst\0\0\0>
	100077; 006000; single; <ror\0\0\0>
	100077; 006100; single; <rol\0\0\0>
	100077; 006200; single; <asr\0\0\0>
	100077; 006300; single; <asl\0\0\0>
	000077; 000100; single; <jmp\0\0\0>
	000077; 000300; single; <swab\0\0>
	000077; 170100; singlw; <ldfps\0>
	000077; 170200; singlw; <stfps\0>
	000077; 170300; singlw; <stst\0\0>
	000077; 170400; singlw; <clrf\0\0>
	000077; 170500; singlw; <tstf\0\0>
	000077; 170600; singlw; <absf\0\0>
	000077; 170700; singlw; <negf\0\0>
	000077; 006700; singlw; <sxt\0\0\0>
	000077; 006600; singlw; <mtpi\0\0>
	000077; 106600; singlw; <mtpd\0\0>
	000077; 006500; singlw; <mfpi\0\0>
	000077; 106500; singlw; <mfpd\0\0>
	000777; 070000; muldiv; <mul\0\0\0>
	000777; 071000; muldiv; <div\0\0\0>
	000777; 072000; muldiv; <ash\0\0\0>
	000777; 073000; muldiv; <ashc\0\0>

	000377; 000400; branch; <br\0\0\0\0>
	000377; 001000; branch; <bne\0\0\0>
	000377; 001400; branch; <beq\0\0\0>
	000377; 002000; branch; <bge\0\0\0>
	000377; 002400; branch; <blt\0\0\0>
	000377; 003000; branch; <bgt\0\0\0>
	000377; 003400; branch; <ble\0\0\0>
	000377; 100000; branch; <bpl\0\0\0>
	000377; 100400; branch; <bmi\0\0\0>
	000377; 101000; branch; <bhi\0\0\0>
	000377; 101400; branch; <blos\0\0>
	000377; 102000; branch; <bvc\0\0\0>
	000377; 102400; branch; <bvs\0\0\0>
	000377; 103000; branch; <bhis\0\0>
	000377; 103400; branch; <blo\0\0\0>

	000000; 000000; noaddr; <halt\0\0>
	000000; 000001; noaddr; <wait\0\0>
	000000; 000002; noaddr; <rti\0\0\0>
	000000; 000004; noaddr; <iot\0\0\0>
	000000; 000005; noaddr; <reset\0>

	000377; 171000; fltrev; <mulf\0\0>
	000377; 171400; fltrev; <modf\0\0>
	000377; 172000; fltrev; <addf\0\0>
	000377; 172400; fltrev; <movf\0\0>
	000377; 173000; fltrev; <subf\0\0>
	000377; 173400; fltrev; <cmpf\0\0>
	000377; 174000; fltnor; <movf\0\0>
	000377; 174400; fltrev; <divf\0\0>
	000377; 175000; fltnor; <movei\0>
	000377; 175400; fltnor; <movfi\0>
	000377; 176000; fltnor; <movfo\0>
	000377; 176400; fltrev; <movie\0>
	000377; 177000; fltrev; <movif\0>
	000377; 177400; fltrev; <movof\0>
	000000; 170000; noaddr; <cfcc\0\0>
	000000; 170001; noaddr; <setf\0\0>
	000000; 170002; noaddr; <seti\0\0>
	000000; 170011; noaddr; <setd\0\0>
	000000; 170012; noaddr; <setl\0\0>

	000777; 004000; specl1; <jsr\0\0\0>
	000777; 074000; specl1; <xor\0\0\0>
	000007; 000200; specl2; <rts\0\0\0>
	000017; 000240; specl3; <cflg\0\0>
	000017; 000260; specl3; <sflg\0\0>
	000377; 104000; specl4; <emt\0\0\0>
	000377; 104400; specl5; <sys\0\0\0>
	000077; 006400; specl7; <mark\0\0>
	000777; 077000; specl8; <sob\0\0\0>
	000007; 000230; specl9; <spl\0\0\0>
	177777; 000000; specl6; <oct\0\0\0>

fltrev:
	bic	$!377,(sp)
muldiv:
	jsr	pc,psp
	mov	(sp)+,r0
	asl	r0
	asl	r0
	swab	r0
	aslb	r0
	aslb	r0
	aslb	r0
	aslb	r0
	asr	r0
	asr	r0
	asr	r0
	asr	r0
	bic	$!7707,r0
	mov	r0,-(sp)
	br	fltnor1

fltnor:
	bic	$!377,(sp)
	br	1f

double:
	tst	(sp)
	bge	1f
	jsr	pc,pb
1:
	jsr	pc,psp
	mov	(sp),r0
fltnor1:
	als	$-6,r0
	jsr	r5,decodadr; dot
	add	r0,incdot
	add	dot,r0
	mov	r0,temp
	jsr	pc,pcom
	mov	(sp)+,r0
	jsr	r5,decodadr; temp
	add	r0,incdot
	rts	pc

single:
	tst	(sp)
	bge	singlw
	jsr	pc,pb
singlw:
	jsr	pc,psp
	mov	(sp)+,r0
	jsr	r5,decodadr; dot
	add	r0,incdot
	rts	pc

specl8: / sob
	jsr	pc,psp
	mov	(sp),r3
	als	$-6,r3
	jsr	pc,preg
	jsr	pc,pcom
	mov	(sp)+,r0
	bic	$!77,r0
	neg	r0
	br	1f

branch:
	jsr	pc,psp
	mov	(sp)+,r0
	bic	$!377,r0
	bit	$200,r0
	beq	1f
	bis	$177400,r0
1:
	inc	r0
	asl	r0
	add	dot,r0
	jsr	pc,pname
	rts	pc

noaddr:
	tst	(sp)+
	rts	pc

specl1: / jsr
	jsr	pc,psp
	mov	(sp),r3
	als	$-6,r3
	jsr	pc,preg
	jsr	pc,pcom
	mov	(sp)+,r0
	jsr	r5,decodadr; dot
	add	r0,incdot
	rts	pc

specl2: / rts
	jsr	pc,psp
	mov	(sp)+,r3
	jsr	pc,preg
	rts	pc

specl3: / opr
	jsr	pc,psp
	mov	(sp)+,r2
	mov	$3f,r1
1:
	bit	$17,r2
	beq	1f
	bit	$10,r2
	beq	2f
	movb	(r1),r0
	jsr	pc,putc
2:
	inc	r1
	asl	r2
	br	1b
1:
	rts	pc
3:
	<nzvc>

specl7: / mark
	bic	$!77,(sp)
	br	specl4

specl9: / spl
	bic	$!7,(sp)
	br	specl4

specl4: / emt
	jsr	pc,psp
	mov	(sp)+,r0
	bic	$!377,r0
8:
	jsr	pc,printo
	rts	pc

specl5: / sys
	jsr	pc,psp
	mov	(sp)+,r2
	bic	$!377,r2
	mov	r2,r0
	asl	r2
	asl	r2
	cmp	r2,$esystab-systab
	bhis	8b
	mov	systab(r2),r1
	jsr	pc,string
	mov	systab+2(r2),-(sp)
	mov	dot,temp1
1:
	dec	(sp)
	bge	2f
	tst	(sp)+
	rts	pc
2:
	jsr	r5,mesg; <;\0>
	add	$2,temp1
	add	$2,incdot
	jsr	r5,get; temp1
	jsr	pc,pname
	br	1b

systab:
	1f; 0; .data; 1:<rele\0>; .text
	1f; 0; .data; 1:<exit\0>; .text
	1f; 0; .data; 1:<fork\0>; .text
	1f; 2; .data; 1:<read\0>; .text
	1f; 2; .data; 1:<write\0>; .text
	1f; 2; .data; 1:<open\0>; .text
	1f; 0; .data; 1:<close\0>; .text
	1f; 0; .data; 1:<wait\0>; .text
	1f; 2; .data; 1:<creat\0>; .text
	1f; 2; .data; 1:<link\0>; .text
	1f; 1; .data; 1:<unlink\0>; .text
	1f; 2; .data; 1:<exec\0>; .text
	1f; 1; .data; 1:<chdir\0>; .text
	1f; 0; .data; 1:<time\0>; .text
	1f; 1; .data; 1:<makdir\0>; .text
	1f; 2; .data; 1:<chmod\0>; .text
	1f; 2; .data; 1:<chown\0>; .text
	1f; 1; .data; 1:<break\0>; .text
	1f; 2; .data; 1:<stat\0>; .text
	1f; 2; .data; 1:<seek\0>; .text
	1f; 2; .data; 1:<tell\0>; .text
	1f; 2; .data; 1:<mount\0>; .text
	1f; 1; .data; 1:<umount\0>; .text
	1f; 0; .data; 1:<setuid\0>; .text
	1f; 0; .data; 1:<getuid\0>; .text
	1f; 0; .data; 1:<stime\0>; .text
	1f; 1; .data; 1:<quit\0>; .text
	1f; 1; .data; 1:<intr\0>; .text
	1f; 1; .data; 1:<fstat\0>; .text
	1f; 1; .data; 1:<emt\0>; .text
	1f; 1; .data; 1:<smdate\0>; .text
	1f; 1; .data; 1:<stty\0>; .text
	1f; 1; .data; 1:<gtty\0>; .text
	1f; 1; .data; 1:<ilgins\0>; .text
	1f; 0; .data; 1:<hog\0>; .text
	1f; 0; .data; 1:<sleep\0>; .text
	1f; 0; .data; 1:<sync\0>; .text
	1f; 0; .data; 1:<kill\0>; .text
	1f; 0; .data; 1:<switches\0>; .text
	1f; 0; .data; 1:<boot\0>; .text
esystab:
.data
.even
.text

specl6: / unknown
	jsr	pc,psp
	mov	(sp)+,r0
	jsr	pc,printo
	rts	pc
