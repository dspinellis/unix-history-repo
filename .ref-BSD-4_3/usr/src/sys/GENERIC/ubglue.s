	.globl	_Xrkintr0
	.align	2
_Xrkintr0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*0)
	pushl	$0
	calls	$1,_rkintr
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xtmintr0
	.align	2
_Xtmintr0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*1)
	pushl	$0
	calls	$1,_tmintr
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xutintr0
	.align	2
_Xutintr0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*2)
	pushl	$0
	calls	$1,_utintr
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xtmscpintr0
	.align	2
_Xtmscpintr0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*3)
	pushl	$0
	calls	$1,_tmscpintr
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xupintr0
	.align	2
_Xupintr0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*4)
	pushl	$0
	calls	$1,_upintr
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xudintr0
	.align	2
_Xudintr0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*5)
	pushl	$0
	calls	$1,_udintr
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xidcintr0
	.align	2
_Xidcintr0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*6)
	pushl	$0
	calls	$1,_idcintr
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xrlintr0
	.align	2
_Xrlintr0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*7)
	pushl	$0
	calls	$1,_rlintr
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdhrint0
	.align	2
_Xdhrint0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*8)
	pushl	$0
	calls	$1,_dhrint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdhxint0
	.align	2
_Xdhxint0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*9)
	pushl	$0
	calls	$1,_dhxint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdmintr0
	.align	2
_Xdmintr0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*10)
	pushl	$0
	calls	$1,_dmintr
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdhrint1
	.align	2
_Xdhrint1:
	pushr	$0x3f
	incl	_fltintrcnt+(4*11)
	pushl	$1
	calls	$1,_dhrint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdhxint1
	.align	2
_Xdhxint1:
	pushr	$0x3f
	incl	_fltintrcnt+(4*12)
	pushl	$1
	calls	$1,_dhxint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdzrint0
	.align	2
_Xdzrint0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*13)
	pushl	$0
	calls	$1,_dzrint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdzxint0
	.align	2
_Xdzxint0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*14)
	movl	$0,r0
	jmp	dzdma

	.globl	_Xdzrint1
	.align	2
_Xdzrint1:
	pushr	$0x3f
	incl	_fltintrcnt+(4*15)
	pushl	$1
	calls	$1,_dzrint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdzxint1
	.align	2
_Xdzxint1:
	pushr	$0x3f
	incl	_fltintrcnt+(4*16)
	movl	$1,r0
	jmp	dzdma

	.globl	_Xdzrint2
	.align	2
_Xdzrint2:
	pushr	$0x3f
	incl	_fltintrcnt+(4*17)
	pushl	$2
	calls	$1,_dzrint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdzxint2
	.align	2
_Xdzxint2:
	pushr	$0x3f
	incl	_fltintrcnt+(4*18)
	movl	$2,r0
	jmp	dzdma

	.globl	_Xdzrint3
	.align	2
_Xdzrint3:
	pushr	$0x3f
	incl	_fltintrcnt+(4*19)
	pushl	$3
	calls	$1,_dzrint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdzxint3
	.align	2
_Xdzxint3:
	pushr	$0x3f
	incl	_fltintrcnt+(4*20)
	movl	$3,r0
	jmp	dzdma

	.globl	_Xdzrint4
	.align	2
_Xdzrint4:
	pushr	$0x3f
	incl	_fltintrcnt+(4*21)
	pushl	$4
	calls	$1,_dzrint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdzxint4
	.align	2
_Xdzxint4:
	pushr	$0x3f
	incl	_fltintrcnt+(4*22)
	movl	$4,r0
	jmp	dzdma

	.globl	_Xdzrint5
	.align	2
_Xdzrint5:
	pushr	$0x3f
	incl	_fltintrcnt+(4*23)
	pushl	$5
	calls	$1,_dzrint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdzxint5
	.align	2
_Xdzxint5:
	pushr	$0x3f
	incl	_fltintrcnt+(4*24)
	movl	$5,r0
	jmp	dzdma

	.globl	_Xdzrint6
	.align	2
_Xdzrint6:
	pushr	$0x3f
	incl	_fltintrcnt+(4*25)
	pushl	$6
	calls	$1,_dzrint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdzxint6
	.align	2
_Xdzxint6:
	pushr	$0x3f
	incl	_fltintrcnt+(4*26)
	movl	$6,r0
	jmp	dzdma

	.globl	_Xdzrint7
	.align	2
_Xdzrint7:
	pushr	$0x3f
	incl	_fltintrcnt+(4*27)
	pushl	$7
	calls	$1,_dzrint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdzxint7
	.align	2
_Xdzxint7:
	pushr	$0x3f
	incl	_fltintrcnt+(4*28)
	movl	$7,r0
	jmp	dzdma

	.globl	_Xtsintr0
	.align	2
_Xtsintr0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*29)
	pushl	$0
	calls	$1,_tsintr
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdmfsrint0
	.align	2
_Xdmfsrint0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*30)
	pushl	$0
	calls	$1,_dmfsrint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdmfsxint0
	.align	2
_Xdmfsxint0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*31)
	pushl	$0
	calls	$1,_dmfsxint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdmfdaint0
	.align	2
_Xdmfdaint0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*32)
	pushl	$0
	calls	$1,_dmfdaint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdmfdbint0
	.align	2
_Xdmfdbint0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*33)
	pushl	$0
	calls	$1,_dmfdbint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdmfrint0
	.align	2
_Xdmfrint0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*34)
	pushl	$0
	calls	$1,_dmfrint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdmfxint0
	.align	2
_Xdmfxint0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*35)
	pushl	$0
	calls	$1,_dmfxint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdmflint0
	.align	2
_Xdmflint0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*36)
	pushl	$0
	calls	$1,_dmflint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdmzrinta0
	.align	2
_Xdmzrinta0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*37)
	pushl	$0
	calls	$1,_dmzrinta
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdmzxinta0
	.align	2
_Xdmzxinta0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*38)
	pushl	$0
	calls	$1,_dmzxinta
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdmzrintb0
	.align	2
_Xdmzrintb0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*39)
	pushl	$0
	calls	$1,_dmzrintb
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdmzxintb0
	.align	2
_Xdmzxintb0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*40)
	pushl	$0
	calls	$1,_dmzxintb
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdmzrintc0
	.align	2
_Xdmzrintc0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*41)
	pushl	$0
	calls	$1,_dmzrintc
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdmzxintc0
	.align	2
_Xdmzxintc0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*42)
	pushl	$0
	calls	$1,_dmzxintc
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdhurint0
	.align	2
_Xdhurint0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*43)
	pushl	$0
	calls	$1,_dhurint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdhuxint0
	.align	2
_Xdhuxint0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*44)
	pushl	$0
	calls	$1,_dhuxint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xlpintr0
	.align	2
_Xlpintr0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*45)
	pushl	$0
	calls	$1,_lpintr
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xenxint0
	.align	2
_Xenxint0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*46)
	pushl	$0
	calls	$1,_enxint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xenrint0
	.align	2
_Xenrint0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*47)
	pushl	$0
	calls	$1,_enrint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xencollide0
	.align	2
_Xencollide0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*48)
	pushl	$0
	calls	$1,_encollide
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xecrint0
	.align	2
_Xecrint0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*49)
	pushl	$0
	calls	$1,_ecrint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xeccollide0
	.align	2
_Xeccollide0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*50)
	pushl	$0
	calls	$1,_eccollide
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xecxint0
	.align	2
_Xecxint0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*51)
	pushl	$0
	calls	$1,_ecxint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xdeintr0
	.align	2
_Xdeintr0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*52)
	pushl	$0
	calls	$1,_deintr
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xilrint0
	.align	2
_Xilrint0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*53)
	pushl	$0
	calls	$1,_ilrint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xilcint0
	.align	2
_Xilcint0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*54)
	pushl	$0
	calls	$1,_ilcint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei

	.globl	_Xexcdint0
	.align	2
_Xexcdint0:
	pushr	$0x3f
	incl	_fltintrcnt+(4*55)
	pushl	$0
	calls	$1,_excdint
	popr	$0x3f
	incl	_cnt+V_INTR
	rei


	.globl	_intrnames

	.globl	_eintrnames
	.data
_intrnames:
	.asciz	"clock"
	.asciz	"cnr"
	.asciz	"cnx"
	.asciz	"tur"
	.asciz	"tux"
	.asciz	"mba0"
	.asciz	"mba1"
	.asciz	"mba2"
	.asciz	"mba3"
	.asciz	"uba0"
	.asciz	"uba1"
	.asciz	"uba2"
	.asciz	"uba3"
	.asciz	"rk0"
	.asciz	"tm0"
	.asciz	"ut0"
	.asciz	"tmscp0"
	.asciz	"up0"
	.asciz	"ud0"
	.asciz	"idc0"
	.asciz	"rl0"
	.asciz	"dhr0"
	.asciz	"dhx0"
	.asciz	"dm0"
	.asciz	"dhr1"
	.asciz	"dhx1"
	.asciz	"dzr0"
	.asciz	"dzx0"
	.asciz	"dzr1"
	.asciz	"dzx1"
	.asciz	"dzr2"
	.asciz	"dzx2"
	.asciz	"dzr3"
	.asciz	"dzx3"
	.asciz	"dzr4"
	.asciz	"dzx4"
	.asciz	"dzr5"
	.asciz	"dzx5"
	.asciz	"dzr6"
	.asciz	"dzx6"
	.asciz	"dzr7"
	.asciz	"dzx7"
	.asciz	"ts0"
	.asciz	"dmfsr0"
	.asciz	"dmfsx0"
	.asciz	"dmfda0"
	.asciz	"dmfdb0"
	.asciz	"dmfr0"
	.asciz	"dmfx0"
	.asciz	"dmfl0"
	.asciz	"dmzra0"
	.asciz	"dmzxa0"
	.asciz	"dmzrb0"
	.asciz	"dmzxb0"
	.asciz	"dmzrc0"
	.asciz	"dmzxc0"
	.asciz	"dhur0"
	.asciz	"dhux0"
	.asciz	"lp0"
	.asciz	"enx0"
	.asciz	"enr0"
	.asciz	"encollide0"
	.asciz	"ecr0"
	.asciz	"eccollide0"
	.asciz	"ecx0"
	.asciz	"de0"
	.asciz	"ilr0"
	.asciz	"ilc0"
	.asciz	"excd0"
_eintrnames:

	.globl	_intrcnt

	.globl	_eintrcnt
	.align 2
_intrcnt:
	.space	4 * 13
_fltintrcnt:
	.space	4 * 56
_eintrcnt:

	.text
