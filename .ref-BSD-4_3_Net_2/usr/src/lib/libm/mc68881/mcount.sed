s/^\(_[a-z_].*\):$/&\
	.data\
X\1:\
	.long	0\
	.text\
	movel	#X\1,a0\
	jsr	mcount/
