	.stabs	"t.c",0144,0,0,LL0
LL0:
	.data
	.stabs	"int:t1=r1;-2147483648;2147483647;",0x80,0,0,0
	.stabs	"char:t2=r2;0;127;",0x80,0,0,0
	.stabs	"long:t3=r1;-2147483648;2147483647;",0x80,0,0,0
	.stabs	"short:t4=r1;-32768;32767;",0x80,0,0,0
	.stabs	"unsigned char:t5=r1;0;255;",0x80,0,0,0
	.stabs	"unsigned short:t6=r1;0;65535;",0x80,0,0,0
	.stabs	"unsigned long:t7=r1;0;-1;",0x80,0,0,0
	.stabs	"unsigned int:t8=r1;0;-1;",0x80,0,0,0
	.stabs	"float:t9=r1;4;0;",0x80,0,0,0
	.stabs	"double:t10=r1;8;0;",0x80,0,0,0
	.stabs	"void:t11=11",0x80,0,0,0
	.stabs	"_iobuf:T12=s20_cnt:1,0,32;_ptr:13=*2,32,32;_base:13,64,32;_bufsiz:1,96,32;_flag:4,128,16;_file:2,144,8;;",0x80,0,20,-1275
	.stabs	"_iob:G14=ar1;0;19;12",0x20,0,20,__iob
	.stabs	"Blah:t16=*15",0x80,0,0,-1275
	.stabs	"nest:T18=s4a:1,0,32;;",0x80,0,4,-1275
	.stabs	"recursive:T17=s12val:1,0,32;next:19=*17,32,32;n:18,64,32;;",0x80,0,12,-1275
	.stabs	"r:G17",0x20,0,12,_r
	.comm	_r,12
	.stabs	"Integer:t1",0x80,0,4,-1275
	.stabs	"blah:T20=s8x:1,0,32;y:1,32,32;;",0x80,0,8,-1275
	.stabs	"z:G20",0x20,0,8,_z
	.comm	_z,8
	.stabs	"array:G21=ar1;0;9;1",0x20,0,4,_array
	.comm	_array,40
	.stabs	"i:G1",0x20,0,4,_i
	.align	2
	.globl	_i
_i:
	.long	3
	.stabs	"main:F1",0x24,0,4,_main
	.stabs	"argc:p1",0xa0,0,4,4
	.stabs	"argv:p22=*13",0xa0,0,1,8
	.text
	.align	1
	.globl	_main
_main:
	.word	L22
	.stabd	0104,0,026
	jbr 	L24
L25:
	.stabd	0104,0,027
	.stabs	"p:20",0x80,0,8,-8
	.stabd	0104,0,030
	.stabs	"a:1",0x80,0,4,-12
	.stabs	"b:1",0x80,0,4,-16
	.stabd	0104,0,031
	.stabs	"color:T23=eRED:0,GREEN:1,BLUE:2,",0x80,0,4,-1275
	.stabs	"Color:t23",0x80,0,4,-1275
	.stabd	0104,0,032
	.stabs	"c:23",0x80,0,4,-20
	.stabd	0104,0,033
	.stabs	"x:10",0x80,0,8,-28
	.stabd	0104,0,034
	.stabs	"y:9",0x80,0,4,-32
	.stabd	0104,0,035
	.stabd	0104,0,036
	.stabd	0300,0,02
	.data	1
L27:
	.ascii	"testing %s\0"
	.text
	movl	8(ap),r0
	pushl	(r0)
	pushl	$L27
	calls	$2,_printf
	.stabd	0104,0,037
	cmpl	4(ap),$1
	jleq	L28
	.stabd	0104,0,040
	.data	1
L29:
	.ascii	" %s\0"
	.text
	movl	8(ap),r0
	pushl	4(r0)
	pushl	$L29
	calls	$2,_printf
	.stabd	0104,0,041
	.stabd	0104,0,042
L28:
	decl	__iob+20
	jlss	L9999
	movl	__iob+24,r0
	incl	__iob+24
	cvtlb	$10,(r0)
	cvtbl	(r0),r0
	jbr	L9998
L9999:
	pushl	$__iob+20
	pushl	$10
	calls	$2,__flsbuf
L9998:
	.stabd	0104,0,043
	movl	$2,-20(fp)
	.stabd	0104,0,044
	.data
	.align	2
L31:
	.double	0d3.50000000000000000000e+00
	.text
	movd	L31,-28(fp)
	.stabd	0104,0,045
	.data
	.align	2
L32:
	.double	0d4.60000000000000000000e+00
	.text
	cvtdf	L32,-32(fp)
	.stabd	0104,0,046
	calls	$0,_abort
	.stabd	0104,0,047
	.stabd	0340,0,02
	ret
	.set	L22,0x0
L24:
	subl2	$32,sp
	jbr 	L25
	.data
