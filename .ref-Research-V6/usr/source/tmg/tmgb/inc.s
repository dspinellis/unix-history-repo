.globl succ,.u,update
.globl .ia,.ib,.da,.db

/prefix ++
.ib:
	inc	(sp)
	jmp	.u
/prefix --
.db:
	dec	(sp)
	jmp	.u
/postfix++
.ia:
	inc	(sp)
	jsr	pc,update
	dec	(sp)
	jmp	succ
/potsfix --
.da:
	dec	(sp)
	jsr	pc,update
	inc	(sp)
	jmp	succ

