.globl _setexit, _reset

	.text
_setexit:
	mov     (sp)+,spc
	mov     sp,ssp
	mov     r1,sr1
	mov     r2,sr2
	mov     r3,sr3
	mov     r4,sr4
	mov     r5,sr5
	jmp     *spc


_reset:
	mov     ssp,sp
	mov     sr1,r1
	mov     sr2,r2
	mov     sr3,r3
	mov     sr4,r4
	mov     sr5,r5
	jmp     *spc

	.data
sr1:    0
sr2:    0
sr3:    0
sr4:    0
sr5:    0
ssp:    0
spc:    0
