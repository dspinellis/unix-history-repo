LL0:
	.data
	.data
_sccsid:
	.long	0x76617309
	.long	0x9632e65
	.long	0x9312e34
	.long	0x302f3238
	.long	0x31312f35
	.long	0x9
	.comm	_filesize,4
	.text
	.align	1
	.globl	_save
_save:
	.word	L15
	jbr 	L17
L18:
	pushl	4(ap)
	calls	$1,_getcmd
	movl	r0,-4(fp)
	tstl	-4(fp)
	jgeq	L20
	mnegl	$1,r0
	ret
L20:
	pushl	$0
	pushl	8(ap)
	calls	$2,_open
	movl	r0,r9
	jleq	L22
	.data	1
L24:
	.ascii	"Can't use an existing file\12\0"
	.text
	pushl	$L24
	calls	$1,_printf
	pushl	r9
	calls	$1,_close
	mnegl	$1,r0
	ret
L22:
	pushl	$493
	pushl	8(ap)
	calls	$2,_creat
	movl	r0,r9
	cmpl	r0,$-1
	jneq	L27
	.data	1
L28:
	.ascii	"Cannot create %s\12\0"
	.text
	pushl	8(ap)
	pushl	$L28
	calls	$2,_printf
	mnegl	$1,r0
	ret
L27:
	pushl	$32
	subl3	$36,fp,r0
	pushl	r0
	pushl	-4(fp)
	calls	$3,_read
	clrl	-24(fp)
	clrl	-20(fp)
	movl	-36(fp),r0
	jbr 	L31
L32:
	movl	-32(fp),r11
	pushl	$0
	calls	$1,_sbrk
	subl2	r11,r0
	movl	r0,-28(fp)
	jbr 	L30
L33:
	bicl3	$1023,-32(fp),r0
	addl2	$1024,r0
	movl	r0,r11
	pushl	$0
	calls	$1,_sbrk
	subl2	r11,r0
	movl	r0,-28(fp)
	jbr 	L30
L34:
	clrl	r11
	pushl	$0
	calls	$1,_sbrk
	movl	r0,-28(fp)
	jbr 	L30
L35:
	movl	-32(fp),r11
	pushl	$0
	pushl	$1024
	pushl	-4(fp)
	calls	$3,_lseek
	jbr 	L30
L31:
	casel	r0,$263,$4
L37:
	.word	L32-L37
	.word	L33-L37
	.word	L34-L37
	.word	L38-L37
	.word	L35-L37
L38:
L30:
	tstl	-28(fp)
	jbr	L39
	mnegl	$1,r0
	ret
L39:
	addl3	$32,-32(fp),r0
	addl2	-28(fp),r0
	movl	r0,_filesize
	pushl	$32
	subl3	$36,fp,r0
	pushl	r0
	pushl	r9
	calls	$3,_write
	cmpl	-36(fp),$267
	jneq	L41
	pushl	$0
	pushl	$1024
	pushl	r9
	calls	$3,_lseek
L41:
	movl	-32(fp),-40(fp)
L42:
	cmpl	-40(fp),$512
	jleq	L43
	pushl	$512
	subl3	$552,fp,r0
	pushl	r0
	pushl	-4(fp)
	calls	$3,_read
	pushl	$512
	subl3	$552,fp,r0
	pushl	r0
	pushl	r9
	calls	$3,_write
	subl2	$512,-40(fp)
	jbr 	L42
L43:
	pushl	-40(fp)
	subl3	$552,fp,r0
	pushl	r0
	pushl	-4(fp)
	calls	$3,_read
	pushl	-40(fp)
	subl3	$552,fp,r0
	pushl	r0
	pushl	r9
	calls	$3,_write
	pushl	-28(fp)
	pushl	r11
	pushl	r9
	calls	$3,_write
	pushl	r9
	calls	$1,_close
	ret
	.set	L15,0xe00
L17:
	movab	-672(sp),sp
	jbr 	L18
	.data
	.text
	.align	1
	.globl	_getcmd
_getcmd:
	.word	L46
	jbr 	L48
L49:
	.data	1
L50:
	.ascii	"PATH\0"
	.text
	pushl	$L50
	calls	$1,_getenv
	movl	r0,-4(fp)
	jneq	L51
	.data	1
L52:
	.ascii	"\72/bin\72/usr/bin\0"
	.text
	moval	L52,-4(fp)
L51:
	.data	1
L54:
	.ascii	"\0"
	.text
	pushl	$47
	pushl	4(ap)
	calls	$2,_index
	tstl	r0
	jeql	L9999
	moval	L54,r0
	jbr	L9998
L9999:
	movl	-4(fp),r0
L9998:
	movl	r0,r11
L57:
	subl3	$132,fp,r0
	pushl	r0
	pushl	4(ap)
	pushl	r11
	calls	$3,_execat
	movl	r0,r11
	pushl	$0
	subl3	$132,fp,r0
	pushl	r0
	calls	$2,_open
	movl	r0,-136(fp)
	jleq	L58
	movl	-136(fp),r0
	ret
L58:
L56:
	tstl	r11
	jneq	L57
L55:
	.data	1
L59:
	.ascii	"Couldn't open %s\12\0"
	.text
	pushl	4(ap)
	pushl	$L59
	calls	$2,_printf
	mnegl	$1,r0
	ret
	ret
	.set	L46,0x800
L48:
	movab	-136(sp),sp
	jbr 	L49
	.data
	.text
	.align	1
_execat:
	.word	L60
	jbr 	L62
L63:
	movl	4(ap),r11
	movl	8(ap),r10
	movl	12(ap),r9
L64:
	tstb	(r11)
	jeql	L65
	cmpb	(r11),$58
	jeql	L65
	cmpb	(r11),$45
	jeql	L65
	movb	(r11)+,(r9)+
	jbr 	L64
L65:
	cmpl	12(ap),r9
	jeql	L66
	cvtlb	$47,(r9)+
L66:
L67:
	tstb	(r10)
	jeql	L68
	movb	(r10)+,(r9)+
	jbr 	L67
L68:
	clrb	(r9)
	tstb	(r11)
	jeql	L9997
	incl	r11
	movl	r11,r0
	jbr	L9996
L9997:
	clrl	r0
L9996:
	ret
	ret
	.set	L60,0xe00
L62:
	jbr 	L63
	.data
