.text
/
/ routines to save space on put calls
/
.globl _put, _put1, _put2, _put3, _put4
/
_put1:
	mov	(sp),putret
	mov	$1,(sp)
	br	1f
_put2:
	mov	(sp),putret
	mov	$2,(sp)
	br	1f
_put3:
	mov	(sp),putret
	mov	$3,(sp)
	br	1f
_put4:
	mov	(sp),putret
	mov	$4,(sp)
1:
	mov	$1f,-(sp)
	jmp	_put
1:
	tst	(sp)+
	jmp	*putret
.bss
putret: .=.+2
