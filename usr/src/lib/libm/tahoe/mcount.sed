s/.word	0x.*$/&\
	.data\
	.align 2\
9:	.long 0\
	.text\
	pushal	9b\
	callf	\$8,mcount/
