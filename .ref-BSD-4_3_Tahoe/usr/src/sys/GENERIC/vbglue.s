SCBVEC(hdintr0):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*0)
	pushl	$0
	callf	$8,_hdintr
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(hdintr1):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*1)
	pushl	$1
	callf	$8,_hdintr
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(hdintr2):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*2)
	pushl	$2
	callf	$8,_hdintr
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(vdintr0):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*3)
	pushl	$0
	callf	$8,_vdintr
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(vdintr1):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*4)
	pushl	$1
	callf	$8,_vdintr
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(vdintr2):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*5)
	pushl	$2
	callf	$8,_vdintr
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(vackint0):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*6)
	pushl	$0
	callf	$8,_vackint
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(vcmdrsp0):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*7)
	pushl	$0
	callf	$8,_vcmdrsp
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(vunsol0):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*8)
	pushl	$0
	callf	$8,_vunsol
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(vackint1):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*9)
	pushl	$1
	callf	$8,_vackint
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(vcmdrsp1):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*10)
	pushl	$1
	callf	$8,_vcmdrsp
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(vunsol1):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*11)
	pushl	$1
	callf	$8,_vunsol
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(exintr0):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*12)
	pushl	$0
	callf	$8,_exintr
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei


	.globl	_intrnames

	.globl	_eintrnames
	.data
_intrnames:
	.asciz	"clock"
	.asciz	"cnr"
	.asciz	"cnx"
	.asciz	"rmtr"
	.asciz	"rmtx"
	.asciz	"buserr"
	.asciz	"hd0"
	.asciz	"hd1"
	.asciz	"hd2"
	.asciz	"vd0"
	.asciz	"vd1"
	.asciz	"vd2"
	.asciz	"vack0"
	.asciz	"vcmdrsp0"
	.asciz	"vunsol0"
	.asciz	"vack1"
	.asciz	"vcmdrsp1"
	.asciz	"vunsol1"
	.asciz	"ex0"
_eintrnames:

	.globl	_intrcnt

	.globl	_eintrcnt
	.align 2
_intrcnt:
	.space	4 * 6
_fltintrcnt:
	.space	4 * 13
_eintrcnt:

	.text
