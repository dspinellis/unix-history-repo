/ Copyright 1974 Bell Telephone Laboratories Inc
/ low core

br4 = 200
br5 = 240
br6 = 300
br7 = 340

. = 0^.
	br	1f
	4

/ trap vectors
	trap; br7+0.		/ bus error
	trap; br7+1.		/ illegal instruction
	trap; br7+2.		/ bpt-trace trap
	trap; br7+3.		/ iot trap
	trap; br7+4.		/ power fail
	trap; br7+5.		/ emulator trap
	trap; br7+6.		/ system entry

. = 40^.
.globl	start, dump
1:	jmp	start
	jmp	dump


. = 60^.
	klin; br4
	klou; br4

. = 70^.
	pcin; br4
	pcou; br4

. = 100^.
	kwlp; br6
	kwlp; br6

. = 220^.
	rkio; br5

. = 240^.
	trap; br7+7.		/ programmed interrupt
	trap; br7+8.		/ floating point
	trap; br7+9.		/ segmentation violation

/ floating vectors
. = 300^.
	dcin; br5+0.
	dcou; br5+0.
	dcin; br5+1.
	dcou; br5+1.

//////////////////////////////////////////////////////
/		interface code to C
//////////////////////////////////////////////////////

.globl	call, trap

.globl	_klrint
klin:	jsr	r0,call; _klrint
.globl	_klxint
klou:	jsr	r0,call; _klxint

.globl	_pcrint
pcin:	jsr	r0,call; _pcrint
.globl	_pcpint
pcou:	jsr	r0,call; _pcpint

.globl	_clock
kwlp:	jsr	r0,call; _clock

.globl	_rkintr
rkio:	jsr	r0,call; _rkintr

.globl	_dcrint
dcin:	jsr	r0,call; _dcrint
.globl	_dcxint
dcou:	jsr	r0,call; _dcxint
