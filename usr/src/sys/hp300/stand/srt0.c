/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: srt0.c 1.18 92/12/21$
 *
 *	@(#)srt0.c	7.7 (Berkeley) %G%
 */

/*
 * Startup code for standalone system
 */

	.globl	begin
	.globl	_end
	.globl	_edata
	.globl	_main
	.globl	_configure
	.globl	_firstopen
	.globl	__rtt
	.globl	_bootdev,_howto,_lowram,_machineid
	.globl	_internalhpib

	STACK =    0xfffff000	| below the ROM page
	BOOTTYPE = 0xfffffdc0
	LOWRAM =   0xfffffdce
	SYSFLAG =  0xfffffed2	| system flags
	MSUS =	   0xfffffedc	| MSUS (?) structure
	VECTORS =  0xfffffee0	| beginning of jump vectors
	NMIRESET = 0xffffff9c	| reset vector
	BUSERR =   0xfffffffc
	MAXADDR =  0xfffff000
	NBPG =     4096
	MMUCMD =   0x005f400c	| MMU command/status register

	.data
_bootdev:
	.long	0
_howto:
	.long	0
_lowram:
	.long	0
_machineid:
	.long	0

	.text
begin:
	movl	#STACK,sp
	moveq	#47,d0		| # of vectors - 1
	movl	#VECTORS+2,a0	| addr part of first vector
vecloop:
	movl	#trap,a0@	| make it direct to trap
	addql	#6,a0		| move to next vector addr
	dbf	d0,vecloop	| go til done
	movl	#NMIRESET,a0	| NMI keyboard reset addr
	movl	#nmi,a0@	| catch in reset routine
/*
 * Determine our CPU type and look for internal HP-IB
 * (really only care about detecting 320 (no DIO-II) right now).
 */
	lea	_machineid,a0
	movl	#0x808,d0
	movc	d0,cacr		| clear and disable on-chip cache(s)
	movl	#0x200,d0	| data freeze bit
	movc	d0,cacr		|   only exists on 68030
	movc	cacr,d0		| read it back
	tstl	d0		| zero?
	jeq	not68030	| yes, we have 68020/68040
	movl	#0x808,d0
	movc	d0,cacr		| clear data freeze bit again

	movl	#0x80,MMUCMD	| set magic cookie
	movl	MMUCMD,d0	| read it back
	btst	#7,d0		| cookie still on?
	jeq	not370		| no, 360 or 375
	movl	#4,a0@		| consider a 370 for now
	movl	#0,MMUCMD	| clear magic cookie
	movl	MMUCMD,d0	| read it back
	btst	#7,d0		| still on?
	jeq	ihpibcheck	| no, a 370
	movl	#5,a0@		| yes, must be a 340
	jra	ihpibcheck
not370:
	movl	#3,a0@		| type is at least a 360
	movl	#0,MMUCMD	| clear magic cookie2
	movl	MMUCMD,d0	| read it back
	btst	#16,d0		| still on?
	jeq	ihpibcheck	| no, a 360
	movl	#6,a0@		| yes, must be a 345/375/400
	jra	ihpibcheck
not68030:
	bset	#31,d0		| data cache enable bit
	movc	d0,cacr		|   only exists on 68040
	movc	cacr,d0		| read it back
	tstl	d0		| zero?
	beq	is68020		| yes, we have 68020
	moveq	#0,d0		| now turn it back off
	movec	d0,cacr		|   before we access any data
	.long	0x4e7b0004	| movc d0,itt0
	.long	0x4e7b0005	| movc d0,itt1
	.long	0x4e7b0006	| movc d0,dtt0
	.long	0x4e7b0007	| movc d0,dtt1
	.word	0xf4d8		| cinva bc
	movl	MMUCMD,d0	| get MMU register
	lsrl	#8,d0		| get apparent ID
	cmpb	#6,d0		| id == 6?
	jeq	is33mhz		| yes, we have a 433s
	movl	#7,a0@		| no, we have a 380/425t
	jra	ihpibcheck
is33mhz:
	movl	#8,a0@		| 433s (XXX 425s returns same ID, ugh!)
	jra	ihpibcheck
is68020:
	movl	#1,a0@		| consider a 330 for now
	movl	#1,MMUCMD	| a 68020, write HP MMU location
	movl	MMUCMD,d0	| read it back
	btst	#0,d0		| zero?
	jeq	ihpibcheck	| yes, a 330
	movl	#0,a0@		| no, consider a 320 for now
	movl	#0x80,MMUCMD	| set magic cookie
	movl	MMUCMD,d0	| read it back
	btst	#7,d0		| cookie still on?
	jeq	ihpibcheck	| no, just a 320
	movl	#2,a0@		| yes, a 350
ihpibcheck:
	movl	#0,MMUCMD	| make sure MMU is off
	btst	#5,SYSFLAG	| do we have an internal HP-IB?
	jeq	boottype	| yes, continue
	clrl	_internalhpib	| no, clear the internal address
/*
 * If this is a reboot, extract howto/bootdev stored by kernel
 */
boottype:
	cmpw	#12,BOOTTYPE	| is this a reboot (REQ_REBOOT)?
	jne	notreboot	| no, skip
	lea	MAXADDR,a0	| find last page
	movl	a0@+,d7		| and extract howto, bootdev
	movl	a0@+,d6		|   from where doboot() left them
	jra	boot1
/*
 * At this point we do not know which logical device the MSUS select
 * code refers to so we cannot construct bootdev.  So we just punt
 * and let configure() construct it.
 */
notreboot:
	moveq	#0,d6		| make sure bootdev is invalid
	cmpw	#18,BOOTTYPE	| does the user want to interact?
	jeq	askme		| yes, go to it
	moveq	#0,d7		| default to RB_AUTOBOOT
	jra	boot1
askme:
	moveq	#3,d7		| default to RB_SINGLE|RB_ASKNAME
boot1:
	movl	d6,_bootdev	| save bootdev and howto
	movl	d7,_howto	|   globally so all can access
	movl	LOWRAM,d0	| read lowram value from bootrom
	addl	#NBPG,d0	| must preserve this for bootrom to reboot
	andl	#0xfffff000,d0	| round to next page
	movl	d0,_lowram	| stash that value
start:
	movl	#_edata,a2	| start of BSS
	movl	#_end,a3	| end
clr:
	clrb	a2@+		| clear BSS
	cmpl	a2,a3		| done?
	bne	clr		| no, keep going
	jsr	_configure	| configure critical devices
	movl	#1,_firstopen	| mark this as the first open
	jsr	_main		| lets go
__rtt:
	movl	#3,_howto	| restarts get RB_SINGLE|RB_ASKNAME
	jmp	start

/*
 * probe a location and see if it causes a bus error
 */
	.globl	_badaddr
_badaddr:
	movl	BUSERR,__bsave	| save ROM bus error handler address
	movl	sp,__ssave	| and current stack pointer
	movl	#catchbad,BUSERR| plug in our handler
	movl	sp@(4),a0	| address to probe
	movw	a0@,d1		| do it
	movl	__bsave,BUSERR	| if we got here, it did not fault
	clrl	d0		| return that this was not a bad addr
	rts

catchbad:
	movl	__bsave,BUSERR	| got a bus error, so restore old handler
	movl	__ssave,sp	| manually restore stack
	moveq	#1,d0		| indicate that we got a fault
	rts			| return to caller of badaddr()

__bsave:
	.long	0
__ssave:
	.long	0

	.globl	_trap
trap:
	moveml	#0xFFFF,sp@-	| save registers
	movl	sp,sp@-		| push pointer to frame
	jsr	_trap		| call C routine to deal with it
	tstl	d0
	jeq	Lstop
	addql	#4,sp
	moveml	sp@+,#0x7FFF
	addql	#8,sp
	rte
Lstop:
	stop	#0x2700		| stop cold

nmi:
	movw	#18,BOOTTYPE	| mark as system switch
	jsr	_kbdnmi		| clear the interrupt
	jra	begin		| start over

#ifdef ROMPRF
	.globl	_romout
_romout:
	movl	sp@(4),d0	| line number
	movl	sp@(8),a0	| string
	jsr	0x150		| do it
	rts
#endif
