.globl .lv,.rv
.globl succ

/ unary &

.lv:
	mov	2(sp),(sp)
	jmp	succ

/ *

.rv:
	mov	(sp),2(sp)
	mov	*2(sp),(sp)
	jmp	succ
