/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * from: Utah $Hdr: srt0.c 1.8 88/12/03$
 *
 *	@(#)srt0.c	7.2 (Berkeley) 5/25/90
 */

/*
 * Startup code for standalone system
 */

	.globl	begin
	.globl	_end
	.globl	_edata
	.globl	_main
	.globl	_configure
	.globl	_openfirst
	.globl	__rtt
	.globl	_lowram,_howto,_devtype,_internalhpib

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

	.data
_lowram:
	.long	0
_howto:
	.long	0
_devtype:
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
	btst	#5,SYSFLAG	| do we have an internal HP-IB?
	jeq	boottype	| yes, continue
	clrl	_internalhpib	| no, clear the internal address
boottype:
	cmpw	#12,BOOTTYPE	| is this a reboot (REQ_REBOOT)?
	jne	notreboot	| no, skip
	movl	#MAXADDR,a0	| find last page
	movl	a0@+,d7		| and extract howto, devtype
	movl	a0@+,d6		|   from where doboot() left them
	jra	boot1
/*
 * At this point we do not know which logical hpib the given select
 * code refers to.  So we just put the select code in the adaptor field
 * where hpibinit() can replace it with the logical hpib number.
 * Note that this may clobber the B_DEVMAGIC field but that isn't set
 * til later anyway.
 */
notreboot:
	cmpw	#18,BOOTTYPE	| does the user want to interact?
	jeq	askme		| yes, go to it
	movl	MSUS,d1		| no, get rom info
	movw	d1,d6		| MSUS comes with SC in upper, unit in lower
	swap	d6		| put in place
	movw	#2,d6		| assume 'a' partition of rd disk
	moveq	#0,d7		| default to RB_AUTOBOOT
	jra	boot1
askme:
	moveq	#7,d6		| default to HP-IB at sc7
	lslw	#8,d6		| position as adaptor number
	swap	d6		| put in place (note implied unit 0)
	movw	#2,d6		| assume 'a' partition of rd disk
	moveq	#3,d7		| default to RB_SINGLE|RB_ASKNAME
boot1:
	movl	d6,_devtype	| save devtype and howto
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
	movl	#1,_openfirst	| mark this as the first open
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
	movl	__bsave,BUSERR	| if we got here, it didn't fault
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
