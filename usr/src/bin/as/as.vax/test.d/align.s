.text 0
	.align 2
_foo:	.word 0x00
	.ascii "This is a random string"
	.align 7
_bar:
	.ascii "bar should be aligned to 7"
.data 0
	.align 6
_data06:
	.ascii "this is junk"
.text 0
	.align 3
	.ascii "aligned to 3"
	ret
.data 2
	.align 2
_food2:	.word 0x00
	.ascii "This is a random string"
	.align 7
_bard2:
	.ascii "bar should be aligned to 7"
	.align 3
	.ascii "aligned to 3"
	ret
.text 2
	.align 2
_foot2:	.word 0x00
	.ascii "This is a random string"
	.align 7
_bart2:
	.ascii "bar should be aligned to 7"
	.align 3
	.ascii "aligned to 3"
	ret
