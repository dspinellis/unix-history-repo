/ fakefp -- fake floating point simulator

.globl	fptrap
signal = 48.
rti = 2

fptrap:
	sub	$2,(sp)
	mov	r0,-(sp)
	sys	signal; 4; 0
	mov	(sp)+,r0
	rti
