# @(#)alarm.s	4.1 (Berkeley) 12/21/80
# C library - alarm, pause

	.set	alarm,27
.globl	_alarm
	.set	pause,29
.globl	_pause

	.align	1
_alarm:
	.word	0x0000
	chmk	$alarm
	ret

	.align	1
_pause:
	.word	0x0000
	chmk	$pause
	ret
