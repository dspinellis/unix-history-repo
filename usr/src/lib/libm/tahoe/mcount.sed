s/.word	0x.*$/&\
	.data\
	.align 2\
1:	.long 0\
	.text\
	pushal	1b\
	callf	$8,mcount/
