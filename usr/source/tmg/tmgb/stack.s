/ builtins for shift-reduce parsking
f = r5
i = r3
g = r4
.globl succ,pbundle,goto,iget
.globl g1
.globl stack,unstack,gotab,accept

/stack label of present rule (state)
/should come first in a rule

stack:
	mov	i,(g)
	sub	$2,(g)+
	jmp	succ

/unstack(n) deletes last n stacked states
/states are distinguishable from translations in not having
/an exit bit ($1) nor being bundles ($100000)
unstack:
	jsr	pc,iget
	mov	(r0),r0
	mov	g,r1
1:
	bit	-(r1),$100001
	bne	1b
	dec	r0
	bge	1b
	tst	(r1)+
	br	1f

accept:			/clean out all states from stack
	mov	f,r1
	add	$g1,r1
1:
	mov	r1,r0
	mov	r1,-(sp)
1:
	cmp	r1,g
	bge	2f
	bit	(r1)+,$100001
	beq	1b
	mov	-2(r1),(r0)+
	br	1b
2:
	mov	r0,g
	mov	(sp)+,r0
	jsr	pc,pbundle
	tst	r0
	beq	1f
	mov	r0,(g)+
1:
	jmp	succ

/gotab(s1,t1,s2,t2,...sn,tn,0,t)
/checks top of stack for states
/s1,s2,... and goes to t1, t2 accordingly
/if top of stack is not in table, goes to t

gotab:
	mov	g,r0
1:
	bit	-(r0),$100001	/find top state
	bne	1b
	mov	(r0),-(sp)
1:
	jsr	pc,iget
	tst	(r0)
	beq	1f
	cmp	(sp),r0
	beq	1f
	jsr	pc,iget
	br	1b
1:
	tst	(sp)+
	jmp	goto
