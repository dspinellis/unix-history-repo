.data 0
	.asciz "	compat.s	4.1	82/05/12	"
.text
	.globl _regs
	.globl _psl
	.globl _pc
	.globl _compat
_compat:
	.word 0x0000
# fixup stack by doing rets from compat and the function
# which called it but modifying frame pc's to stay here
	moval l0,16(fp)	# fix first return address
	ret
l0:	moval l1,16(fp)	# fix first return address
	ret
l1:	moval l2,16(fp)	# fix next return address
	ret
l2:	movl _psl,4(sp)
	movl _pc,(sp)
# copy back saved register values
	movw _regs,r0
	movw _regs+02,r1
	movw _regs+04,r2
	movw _regs+06,r3
	movw _regs+010,r4
	movw _regs+012,r5
	movw _regs+014,r6
# go to compatability mode with rei assuming correct psl is setup
	rei
	.globl _getregs
_getregs:
	.word 0
# copy registers into known locations for examination or modification
	movw r0,_regs
	movw r1,_regs+02
	movw r2,_regs+04
	movw r3,_regs+06
	movw r4,_regs+010
	movw r5,_regs+012
	movw r6,_regs+014
	ret
