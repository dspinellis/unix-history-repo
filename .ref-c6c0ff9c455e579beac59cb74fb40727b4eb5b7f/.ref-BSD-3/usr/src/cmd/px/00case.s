# 00case.s
#
# Berkeley Pascal Assembler Kernel
#
	.globl	_interpret	#normal interpreter entry point
#
# register definitions
#
# registers R0 - R6 are for scratch use
#
	.set	lino, r11
	.set	lc, r10
	.set	dp, r9
	.set	loop, r8
	.set	buf, r7
#
# Global constants
#
	.set	R2,0x004	#register fields in call mask
	.set	R3,0x008
	.set	R4,0x010
	.set	R5,0x020
	.set	R6,0x040
	.set	R7,0x080
	.set	R8,0x100
	.set	R9,0x200
	.set	R10,0x400
	.set	R11,0x800
	.set	SIGINT,2	#interrupt signal
	.set	SIGFPE,8	#arithmetic exception signal
	.set	SIGSEGV,11	#segmentation violation
	.set	MASK,6		#offset of save mask in call stack
	.set	FP,12		#offset of "fp" in call stack
	.set	PC,16		#offset of "pc" in call stack
	.set	REGS,20		#beginning of saved registers in call stack
	.set	O_DATE,0346	#DATE opcode number
	.set	O_TIME,0347	#TIME opcode number
	.set	tempsize,-256	#maximum required temporary stack space
	.set	HZ,60		#interrupt frequency
#
# program variables
#
	.globl	_display	#runtime display
	.globl	_addrsze	#size of addresses
	.globl	_file		#current file name
	.globl	_bufopt		#standard output buffering option
	.globl	_fchain		#head of active file chain
	.globl	_llimit		#max number of output lines
	.globl	_stcnt		#number of stmts executed
	.globl	_stlim		#max number of stmts to exec
	.globl	_nodump		#1 => no postmortum dump
	.globl	_perrno		#interpreter error number
	.globl	_profcnts	#PX profile execution counts
	.globl	_pxpbuf		#ptr to pxp buffer
	.globl	_pxpsize	#size of pxp buffer
	.globl	_argc		#number of passed args
	.globl	_argv		#values of passed args
	.globl	__iob		#base of I/O buffer block
#
# system subroutines
#
	.globl	_signal
	.globl	_time
	.globl	_times
#
# system math routines
#
	.globl	_atan
	.globl	_cos
	.globl	_exp
	.globl	_log
	.globl	_sin
	.globl	_sqrt
	.globl	_srand
	.globl	_rand
#
# pascal specific subroutines
#
	.globl	_error		#error message routine
	.globl	_palloc		#heap allocator
	.globl	_pfree
	.globl	_pcttot		#set constructor
	.globl	_pdattim	#getting date, time info
	.globl	_perror		#process pxp errors
#
# initializing the interpreter
#
_interpret:
	.word	0xffc		#register save mask
	moval	iloop,r8
	tstl	8(ap)		#check for profiling
	beql	l0050
	moval	ploop,r8	#set profiling request
l0050:
	callg	*4(ap),l0051	#set ap to base of program
	ret
l0051:
	.word	0
	bispsw	$0xe0		#enable overflow traps
	movl	ap,r10		#program start address
	moval	_display,r9
	moval	-4(sp),(r9)
	pushal	stderr		#set up global file variables
	movl	sp,stderr+FLEV
	movl	_llimit,stderr+LLIMIT
	pushal	stdout
	movl	sp,stdout+FLEV
	movl	_llimit,stdout+LLIMIT
	pushal	stdin
	movl	sp,stdin+FLEV
	moval	stdin,_fchain
	jmp	(r8)		#begin interpreter
#
# main interpreter loop
# the instruction 'jmp	(loop)'
# transfers here
#
ploop:
	movzbl	(r10),r0
	incl	_profcnts[r0]
iloop:
	caseb	(r10)+,$0,$255
optab:
	.word	_ABORT-optab
	.word	_HALT-optab
	.word	_TRA4-optab
	.word	_NODUMP-optab
	.word	_BEG-optab
	.word	_END-optab
	.word	_CALL-optab
	.word	_TRACNT-optab
	.word	_PUSH-optab
	.word	_POP-optab
	.word	_INX4-optab
	.word	_SDUP-optab
	.word	_IF-optab
	.word	_TRA-optab
	.word	_LINO-optab
	.word	_GOTO-optab
	.word	_REL2-optab
	.word	_REL4-optab
	.word	_REL24-optab
	.word	_REL42-optab
	.word	_REL8-optab
	.word	_RELG-optab
	.word	_RELT-optab
	.word	badop-optab
	.word	_REL28-optab
	.word	_REL48-optab
	.word	_REL82-optab
	.word	_REL84-optab
	.word	_AND-optab
	.word	_OR-optab
	.word	_NOT-optab
	.word	badop-optab
	.word	_AS2-optab
	.word	_AS4-optab
	.word	_AS24-optab
	.word	_AS42-optab
	.word	_AS8-optab
	.word	_INX2P2-optab
	.word	_INX4P2-optab
	.word	_AS-optab
	.word	_AS21-optab
	.word	_AS41-optab
	.word	_AS28-optab
	.word	_AS48-optab
	.word	_OFF-optab
	.word	_INX2-optab
	.word	_NIL-optab
	.word	_LV-optab
	.word	_ADD2-optab
	.word	_ADD4-optab
	.word	_ADD24-optab
	.word	_ADD42-optab
	.word	_ADD28-optab
	.word	_ADD48-optab
	.word	_ADD82-optab
	.word	_ADD84-optab
	.word	_SUB2-optab
	.word	_SUB4-optab
	.word	_SUB24-optab
	.word	_SUB42-optab
	.word	_SUB28-optab
	.word	_SUB48-optab
	.word	_SUB82-optab
	.word	_SUB84-optab
	.word	_MUL2-optab
	.word	_MUL4-optab
	.word	_MUL24-optab
	.word	_MUL42-optab
	.word	_MUL28-optab
	.word	_MUL48-optab
	.word	_MUL82-optab
	.word	_MUL84-optab
	.word	_ABS2-optab
	.word	_ABS4-optab
	.word	_ABS8-optab
	.word	badop-optab
	.word	_ADD8-optab
	.word	_SUB8-optab
	.word	_MUL8-optab
	.word	_DVD8-optab
	.word	_DIV2-optab
	.word	_DIV4-optab
	.word	_DIV24-optab
	.word	_DIV42-optab
	.word	badop-optab
	.word	badop-optab
	.word	badop-optab
	.word	badop-optab
	.word	_MOD2-optab
	.word	_MOD4-optab
	.word	_MOD24-optab
	.word	_MOD42-optab
	.word	badop-optab
	.word	badop-optab
	.word	badop-optab
	.word	badop-optab
	.word	_NEG2-optab
	.word	_NEG4-optab
	.word	_NEG8-optab
	.word	badop-optab
	.word	badop-optab
	.word	badop-optab
	.word	badop-optab
	.word	badop-optab
	.word	badop-optab
	.word	badop-optab
	.word	badop-optab
	.word	badop-optab
	.word	badop-optab
	.word	badop-optab
	.word	badop-optab
	.word	badop-optab
	.word	_DVD2-optab
	.word	_DVD4-optab
	.word	_DVD24-optab
	.word	_DVD42-optab
	.word	_DVD28-optab
	.word	_DVD48-optab
	.word	_DVD82-optab
	.word	_DVD84-optab
	.word	_RV1-optab
	.word	_RV2-optab
	.word	_RV4-optab
	.word	_RV8-optab
	.word	_IND1-optab
	.word	_IND2-optab
	.word	_IND4-optab
	.word	_IND8-optab
	.word	_CON1-optab
	.word	_CON2-optab
	.word	_CON4-optab
	.word	_CON8-optab
	.word	_RV-optab
	.word	_IND-optab
	.word	_CON-optab
	.word	badop-optab
	.word	_RANG2-optab
	.word	_RANG42-optab
	.word	_RSNG2-optab
	.word	_RSNG42-optab
	.word	_RANG4-optab
	.word	_RANG24-optab
	.word	_RSNG4-optab
	.word	_RSNG24-optab
	.word	_WRITEF-optab
	.word	_WRITEC-optab
	.word	_WRITES-optab
	.word	_WRITEB-optab
	.word	badop-optab
	.word	badop-optab
	.word	badop-optab
	.word	badop-optab
	.word	_BLKBEG-optab
	.word	_PUSH4-optab
	.word	_POP4-optab
	.word	badop-optab
	.word	badop-optab
	.word	badop-optab
	.word	badop-optab
	.word	badop-optab
	.word	_CASE1OP-optab
	.word	_CASE2OP-optab
	.word	_CASE4OP-optab
	.word	_PXPBUF-optab
	.word	_COUNT-optab
	.word	badop-optab
	.word	badop-optab
	.word	badop-optab
	.word	_ADDT-optab
	.word	_SUBT-optab
	.word	_MULT-optab
	.word	_INCT-optab
	.word	_CTTOT-optab
	.word	_CARD-optab
	.word	_IN-optab
	.word	_ASRT-optab
	.word	_FOR1U-optab
	.word	_FOR2U-optab
	.word	_FOR4U-optab
	.word	_FOR1D-optab
	.word	_FOR2D-optab
	.word	_FOR4D-optab
	.word	_STLIM-optab
	.word	_SCLCK-optab
	.word	_STOI-optab
	.word	_STOD-optab
	.word	_ITOD-optab
	.word	_ITOS-optab
	.word	_BUFF-optab
	.word	_WCLCK-optab
	.word	_WRHEX2-optab
	.word	_WRHEX4-optab
	.word	_GET-optab
	.word	_PUT-optab
	.word	_MESSAGE-optab
	.word	_FNIL-optab
	.word	_EOF-optab
	.word	_EOLN-optab
	.word	_RESET-optab
	.word	_REWRITE-optab
	.word	_REMOVE-optab
	.word	_READ4-optab
	.word	_UNIT-optab
	.word	_READC-optab
	.word	_READ8-optab
	.word	_UNITINP-optab
	.word	_UNITOUT-optab
	.word	_READLN-optab
	.word	_WRIT2-optab
	.word	_WRIT4-optab
	.word	_WRITB-optab
	.word	_WRITC-optab
	.word	_WRIT8-optab
	.word	_WRITG-optab
	.word	_WRIT82-optab
	.word	_WRITLN-optab
	.word	_WROCT2-optab
	.word	_WROCT4-optab
	.word	_FLUSH-optab
	.word	_PACK-optab
	.word	_UNPACK-optab
	.word	_LLIMIT-optab
	.word	_ARGC-optab
	.word	_ARGV-optab
	.word	_CLCK-optab
	.word	_SEED-optab
	.word	_RANDOM-optab
	.word	_DISPOSE-optab
	.word	_NEW-optab
	.word	_EXPO-optab
	.word	_DATE-optab
	.word	_TIME-optab
	.word	_ATAN-optab
	.word	_COS-optab
	.word	_EXP-optab
	.word	_LN-optab
	.word	_SIN-optab
	.word	_SQRT-optab
	.word	_CHR2-optab
	.word	_CHR4-optab
	.word	_ODD2-optab
	.word	_ODD4-optab
	.word	_PRED2-optab
	.word	_PRED4-optab
	.word	_PRED24-optab
	.word	_SUCC2-optab
	.word	_SUCC4-optab
	.word	_SUCC24-optab
	.word	_DEFNAME-optab
	.word	_PAGE-optab
	.word	_UNDEF-optab
	.word	_SQR2-optab
	.word	_SQR4-optab
	.word	_SQR8-optab
	.word	_ROUND-optab
	.word	_TRUNC-optab
badop:
	incl	r10
	movw	$EBADOP,_perrno
	jbr	error
