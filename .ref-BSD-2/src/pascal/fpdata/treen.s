.data
/
/ Routines to save space on tree calls
/
.globl _tree, _tree1, _tree2, _tree3, _tree4, _tree5
/
_tree1:
	mov	(sp),trret
	mov	$1,(sp)
	br	1f
_tree2:
	mov	(sp),trret
	mov	$2,(sp)
	br	1f
_tree3:
	mov	(sp),trret
	mov	$3,(sp)
	br	1f
_tree4:
	mov	(sp),trret
	mov	$4,(sp)
	br	1f
_tree5:
	mov	(sp),trret
	mov	$5,(sp)
1:
	mov	$1f,-(sp)
	jmp	_tree
1:
	tst	(sp)+
	jmp	*trret
.bss
trret:	. = .+2
