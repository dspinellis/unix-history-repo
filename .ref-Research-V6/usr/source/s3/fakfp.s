/ fakefp -- fake floating point simulator

.globl	fptrap

fptrap:
	sub	$2,(sp)
	mov	r0,-(sp)
	sys	signal; 4; 0
	mov	(sp)+,r0
	2	/ rti
