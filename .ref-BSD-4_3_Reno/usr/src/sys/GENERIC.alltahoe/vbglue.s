SCBVEC(vdintr0):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*0)
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
	incl	_fltintrcnt+(4*1)
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
	incl	_fltintrcnt+(4*2)
	pushl	$2
	callf	$8,_vdintr
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(cyintr0):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*3)
	pushl	$0
	callf	$8,_cyintr
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(cyintr1):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*4)
	pushl	$1
	callf	$8,_cyintr
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(vackint0):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*5)
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
	incl	_fltintrcnt+(4*6)
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
	incl	_fltintrcnt+(4*7)
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
	incl	_fltintrcnt+(4*8)
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
	incl	_fltintrcnt+(4*9)
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
	incl	_fltintrcnt+(4*10)
	pushl	$1
	callf	$8,_vunsol
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(mpintr0):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*11)
	pushl	$0
	callf	$8,_mpintr
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(mpdlintr0):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*12)
	pushl	$0
	callf	$8,_mpdlintr
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(mpintr1):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*13)
	pushl	$1
	callf	$8,_mpintr
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(mpdlintr1):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*14)
	pushl	$1
	callf	$8,_mpdlintr
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(acecint0):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*15)
	pushl	$0
	callf	$8,_acecint
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(acerint0):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*16)
	pushl	$0
	callf	$8,_acerint
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(acecint1):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*17)
	pushl	$1
	callf	$8,_acecint
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(acerint1):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*18)
	pushl	$1
	callf	$8,_acerint
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(enpintr0):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*19)
	pushl	$0
	callf	$8,_enpintr
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(enpintr1):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*20)
	pushl	$1
	callf	$8,_enpintr
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(drintr0):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*21)
	pushl	$0
	callf	$8,_drintr
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(ikintr0):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*22)
	pushl	$0
	callf	$8,_ikintr
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(hdintr0):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*23)
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
	incl	_fltintrcnt+(4*24)
	pushl	$1
	callf	$8,_hdintr
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(vackint2):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*25)
	pushl	$2
	callf	$8,_vackint
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(vcmdrsp2):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*26)
	pushl	$2
	callf	$8,_vcmdrsp
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(vunsol2):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*27)
	pushl	$2
	callf	$8,_vunsol
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(vackint3):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*28)
	pushl	$3
	callf	$8,_vackint
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(vcmdrsp3):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*29)
	pushl	$3
	callf	$8,_vcmdrsp
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(vunsol3):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*30)
	pushl	$3
	callf	$8,_vunsol
	incl	_cnt+V_INTR
	POPR
	REST_FPSTAT
	rei

SCBVEC(exintr0):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	incl	_fltintrcnt+(4*31)
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
	.asciz	"vd0"
	.asciz	"vd1"
	.asciz	"vd2"
	.asciz	"cy0"
	.asciz	"cy1"
	.asciz	"vack0"
	.asciz	"vcmdrsp0"
	.asciz	"vunsol0"
	.asciz	"vack1"
	.asciz	"vcmdrsp1"
	.asciz	"vunsol1"
	.asciz	"mp0"
	.asciz	"mpdl0"
	.asciz	"mp1"
	.asciz	"mpdl1"
	.asciz	"acec0"
	.asciz	"acer0"
	.asciz	"acec1"
	.asciz	"acer1"
	.asciz	"enp0"
	.asciz	"enp1"
	.asciz	"dr0"
	.asciz	"ik0"
	.asciz	"hd0"
	.asciz	"hd1"
	.asciz	"vack2"
	.asciz	"vcmdrsp2"
	.asciz	"vunsol2"
	.asciz	"vack3"
	.asciz	"vcmdrsp3"
	.asciz	"vunsol3"
	.asciz	"ex0"
_eintrnames:

	.globl	_intrcnt

	.globl	_eintrcnt
	.align 2
_intrcnt:
	.space	4 * 6
_fltintrcnt:
	.space	4 * 32
_eintrcnt:

	.text
