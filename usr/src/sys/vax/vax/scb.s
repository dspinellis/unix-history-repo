/*	scb.s	4.2	%G%	*/

/*
 * System control block
 */
	.set	INTSTK,1	# handle this interrupt on the interrupt stack
	.set	HALT,3		# halt if this interrupt occurs

/*	.align	PGSHIFT	*/
	.globl	_Scbbase
_Scbbase:
	.long	Xstray + INTSTK		# unused
	.long	Xmachcheck + INTSTK	# machine check interrupt
	.long	Xkspnotval + INTSTK	# kernel stack not valid
	.long	Xpowfail + HALT		# power fail
	.long	Xprivinflt		# privileged instruction 
	.long	Xxfcflt			# xfc instruction 
	.long	Xresopflt		# reserved operand 
	.long	Xresadflt		# reserved addressing 
	.long	Xprotflt		# protection and pt length violation
	.long	Xtransflt		# address translation not valid fault 
	.long	Xtracep			# trace pending
	.long	Xbptflt			# bpt instruction
	.long	Xcompatflt		# compatibility mode fault
	.long	Xarithtrap		# arithmetic trap
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xsyscall		# chmk
	.long	Xchme+INTSTK		# chme
	.long	Xchms+INTSTK		# chms
	.long	Xchmu+INTSTK		# chmu
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
#if VAX==750 || VAX==ANY
	.long	Xwtime + INTSTK		# write timeout
#else
	.long	Xstray + INTSTK		# unused
#endif
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# software level 1
	.long	Xstray + INTSTK		# software level 2 (asts)
	.long	Xresched		# reschedule nudge
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
ubabase:
	.long	Xclockint		# clock
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
#if VAX==750
	.long	Xconsdin + INTSTK	# tu58 receiver
	.long	Xconsdout + INTSTK	# tu58 transmitter
#else
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
#endif
	.long	Xcnrint + INTSTK	# console receiver 
	.long	Xcnxint + INTSTK	# console transmitter

/*
 * I/O vectors
 */

/* IPL 14 */
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused

/* IPL 15 */
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused

/* IPL 16 */
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused

/* IPL 17 */
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
