/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)trapov_.c	5.3	11/3/86
 *
 *	Fortran/C floating-point overflow handler
 *
 *	The idea of these routines is to catch floating-point overflows
 *	and print an eror message.  When we then get a reserved operand
 *	exception, we then fix up the value to the highest possible
 *	number.  Keen, no?
 *	Messy, yes!
 *
 *	Synopsis:
 *		call trapov(n)
 *			causes overflows to be trapped, with the first 'n'
 *			overflows getting an "Overflow!" message printed.
 *		k = ovcnt(0)
 *			causes 'k' to get the number of overflows since the
 *			last call to trapov().
 *
 *	Gary Klimowicz, April 17, 1981
 *	Integerated with libF77: David Wasley, UCB, July 1981.
 */

# include <stdio.h>
# include <signal.h>
# include "opcodes.h"
# include "../libI77/fiodefs.h"
# define SIG_VAL	int (*)()

/*
 *	Potential operand values
 */
typedef	union operand_types {
		char	o_byte;
		short	o_word;
		long	o_long;
		float	o_float;
		long	o_quad[2];
		double	o_double;
	} anyval;

/*
 *	the fortran unit control table
 */
extern unit units[];

/*
 * Fortran message table is in main
 */
struct msgtbl {
	char	*mesg;
	int	dummy;
};
extern struct msgtbl	act_fpe[];

anyval *get_operand_address(), *addr_of_reg();
char *opcode_name();

/*
 * trap type codes
 */
# define INT_OVF_T	1
# define INT_DIV_T	2
# define FLT_OVF_T	3
# define FLT_DIV_T	4
# define FLT_UND_T	5
# define DEC_OVF_T	6
# define SUB_RNG_T	7
# define FLT_OVF_F	8
# define FLT_DIV_F	9
# define FLT_UND_F	10

# define RES_ADR_F	0
# define RES_OPC_F	1
# define RES_OPR_F	2

#ifdef vax
/*
 *	Operand modes
 */
# define LITERAL0	0x0
# define LITERAL1	0x1
# define LITERAL2	0x2
# define LITERAL3	0x3
# define INDEXED	0x4
# define REGISTER	0x5
# define REG_DEF	0x6
# define AUTO_DEC	0x7
# define AUTO_INC	0x8
# define AUTO_INC_DEF	0x9
# define BYTE_DISP	0xa
# define BYTE_DISP_DEF	0xb
# define WORD_DISP	0xc
# define WORD_DISP_DEF	0xd
# define LONG_DISP	0xe
# define LONG_DISP_DEF	0xf

/*
 *	Operand value types
 */
# define F		1
# define D		2
# define IDUNNO		3

# define PC	0xf
# define SP	0xe
# define FP	0xd
# define AP	0xc

/*
 *	GLOBAL VARIABLES (we need a few)
 *
 *	Actual program counter and locations of registers.
 */
static char	*pc;
static int	*regs0t6;
static int	*regs7t11;
static int	max_messages;
static int	total_overflows;
static union	{
	long	v_long[2];
	double	v_double;
	} retrn;
static int	(*sigill_default)() = (SIG_VAL)-1;
static int	(*sigfpe_default)();

/*
 *	This routine sets up the signal handler for the floating-point
 *	and reserved operand interrupts.
 */

trapov_(count, rtnval)
	int *count;
	double *rtnval;
{
	extern got_overflow(), got_illegal_instruction();

	sigfpe_default = signal(SIGFPE, got_overflow);
	if (sigill_default == (SIG_VAL)-1)
		sigill_default = signal(SIGILL, got_illegal_instruction);
	total_overflows = 0;
	max_messages = *count;
	retrn.v_double = *rtnval;
}



/*
 *	got_overflow - routine called when overflow occurs
 *
 *	This routine just prints a message about the overflow.
 *	It is impossible to find the bad result at this point.
 *	Instead, we wait until we get the reserved operand exception
 *	when we try to use it.  This raises the SIGILL signal.
 */

/*ARGSUSED*/
got_overflow(signo, codeword, myaddr, pc, ps)
	char *myaddr, *pc;
{
	int	*sp, i;
	FILE	*ef;

	signal(SIGFPE, got_overflow);
	ef = units[STDERR].ufd;
	switch (codeword) {
		case INT_OVF_T:
		case INT_DIV_T:
		case FLT_UND_T:
		case DEC_OVF_T:
		case SUB_RNG_T:
		case FLT_OVF_F:
		case FLT_DIV_F:
		case FLT_UND_F:
				if (sigfpe_default > (SIG_VAL)7)
					return((*sigfpe_default)(signo, codeword, myaddr, pc, ps));
				else
					sigdie(signo, codeword, myaddr, pc, ps);
					/* NOTREACHED */

		case FLT_OVF_T:
		case FLT_DIV_T:
				if (++total_overflows <= max_messages) {
					fprintf(ef, "trapov: %s",
						act_fpe[codeword-1].mesg);
					if (total_overflows == max_messages)
						fprintf(ef, ": No more messages will be printed.\n");
					else
						fputc('\n', ef);
				}
				return;
	}
}

int 
ovcnt_()
{
	return total_overflows;
}

/*
 *	got_illegal_instruction - handle "illegal instruction" signals.
 *
 *	This really deals only with reserved operand exceptions.
 *	Since there is no way to check this directly, we look at the
 *	opcode of the instruction we are executing to see if it is a
 *	floating-point operation (with floating-point operands, not
 *	just results).
 *
 *	This is complicated by the fact that the registers that will
 *	eventually be restored are saved in two places.  registers 7-11
 *	are saved by this routine, and are in its call frame. (we have
 *	to take special care that these registers are specified in
 *	the procedure entry mask here.)
 *	Registers 0-6 are saved at interrupt time, and are at a offset
 *	-8 from the 'signo' parameter below.
 *	There is ane extremely inimate connection between the value of
 *	the entry mask set by the 'makefile' script, and the constants
 *	used in the register offset calculations below.
 *	Can someone think of a better way to do this?
 */

/*ARGSUSED*/
got_illegal_instruction(signo, codeword, myaddr, trap_pc, ps)
	char *myaddr, *trap_pc;
{
	int first_local[1];		/* must be first */
	int i, opcode, type, o_no, no_reserved;
	anyval *opnd;

	regs7t11 = &first_local[0];
	regs0t6 = &signo - 8;
	pc = trap_pc;

	opcode = fetch_byte() & 0xff;
	no_reserved = 0;
	if (codeword != RES_OPR_F || !is_floating_operation(opcode)) {
		if (sigill_default > (SIG_VAL)7)
			return((*sigill_default)(signo, codeword, myaddr, trap_pc, ps));
		else
			sigdie(signo, codeword, myaddr, trap_pc, ps);
			/* NOTREACHED */
	}

	if (opcode == POLYD || opcode == POLYF) {
		got_illegal_poly(opcode);
		return;
	}

	if (opcode == EMODD || opcode == EMODF) {
		got_illegal_emod(opcode);
		return;
	}

	/*
	 * This opcode wasn't "unusual".
	 * Look at the operands to try and find a reserved operand.
	 */
	for (o_no = 1; o_no <= no_operands(opcode); ++o_no) {
		type = operand_type(opcode, o_no);
		if (type != F && type != D) {
			advance_pc(type);
			continue;
		}

		/* F or D operand.  Check it out */
		opnd = get_operand_address(type);
		if (opnd == NULL) {
			fprintf(units[STDERR].ufd, "Can't get operand address: 0x%x, %d\n",
				pc, o_no);
			f77_abort();
		}
		if (type == F && opnd->o_long == 0x00008000) {
			/* found one */
			opnd->o_long = retrn.v_long[0];
			++no_reserved;
		} else if (type == D && opnd->o_long == 0x00008000) {
			/* found one here, too! */
			opnd->o_quad[0] = retrn.v_long[0];
			/* Fix next pointer */
			if (opnd == addr_of_reg(6)) opnd = addr_of_reg(7);
			else opnd = (anyval *) ((char *) opnd + 4);
			opnd->o_quad[0] = retrn.v_long[1];
			++no_reserved;
		}

	}

	if (no_reserved == 0) {
		fprintf(units[STDERR].ufd, "Can't find any reserved operand!\n");
		f77_abort();
	}
}
/*
 * is_floating_exception - was the operation code for a floating instruction?
 */

is_floating_operation(opcode)
	int opcode;
{
	switch (opcode) {
		case ACBD:	case ACBF:	case ADDD2:	case ADDD3:
		case ADDF2:	case ADDF3:	case CMPD:	case CMPF:
		case CVTDB:	case CVTDF:	case CVTDL:	case CVTDW:
		case CVTFB:	case CVTFD:	case CVTFL:	case CVTFW:
		case CVTRDL:	case CVTRFL:	case DIVD2:	case DIVD3:
		case DIVF2:	case DIVF3:	case EMODD:	case EMODF:
		case MNEGD:	case MNEGF:	case MOVD:	case MOVF:
		case MULD2:	case MULD3:	case MULF2:	case MULF3:
		case POLYD:	case POLYF:	case SUBD2:	case SUBD3:
		case SUBF2:	case SUBF3:	case TSTD:	case TSTF:
			return 1;

		default:
			return 0;
	}
}
/*
 * got_illegal_poly - handle an illegal POLY[DF] instruction.
 *
 * We don't do anything here yet.
 */

/*ARGSUSED*/
got_illegal_poly(opcode)
{
	fprintf(units[STDERR].ufd, "Can't do 'poly' instructions yet\n");
	f77_abort();
}



/*
 * got_illegal_emod - handle illegal EMOD[DF] instruction.
 *
 * We don't do anything here yet.
 */

/*ARGSUSED*/
got_illegal_emod(opcode)
{
	fprintf(units[STDERR].ufd, "Can't do 'emod' instructions yet\n");
	f77_abort();
}


/*
 *	no_operands - determine the number of operands in this instruction.
 *
 */

no_operands(opcode)
{
	switch (opcode) {
		case ACBD:
		case ACBF:
			return 3;
		
		case MNEGD:
		case MNEGF:
		case MOVD:
		case MOVF:
		case TSTD:
		case TSTF:
			return 1;
		
		default:
			return 2;
	}
}



/*
 *	operand_type - is the operand a D or an F?
 *
 *	We are only descriminating between Floats and Doubles here.
 *	Other operands may be possible on exotic instructions.
 */

/*ARGSUSED*/
operand_type(opcode, no)
{
	if (opcode >= 0x40 && opcode <= 0x56) return F;
	if (opcode >= 0x60 && opcode <= 0x76) return D;
	return IDUNNO;
}



/*
 *	advance_pc - Advance the program counter past an operand.
 *
 *	We just bump the pc by the appropriate values.
 */

advance_pc(type)
{
	register int mode, reg;

	mode = fetch_byte();
	reg = mode & 0xf;
	mode = (mode >> 4) & 0xf;
	switch (mode) {
		case LITERAL0:
		case LITERAL1:
		case LITERAL2:
		case LITERAL3:
			return;

		case INDEXED:
			advance_pc(type);
			return;

		case REGISTER:
		case REG_DEF:
		case AUTO_DEC:
			return;
		
		case AUTO_INC:
			if (reg == PC) {
				if (type == F) (void) fetch_long();
				else if (type == D) {
					(void) fetch_long();
					(void) fetch_long();
				} else {
					fprintf(units[STDERR].ufd, "Bad type %d in advance\n",
						type);
					f77_abort();
				}
			}
			return;

		case AUTO_INC_DEF:
			if (reg == PC) (void) fetch_long();
			return;

		case BYTE_DISP:
		case BYTE_DISP_DEF:
			(void) fetch_byte();
			return;

		case WORD_DISP:
		case WORD_DISP_DEF:
			(void) fetch_word();
			return;

		case LONG_DISP:
		case LONG_DISP_DEF:
			(void) fetch_long();
			return;
		
		default:
			fprintf(units[STDERR].ufd, "Bad mode 0x%x in op_length()\n", mode);
			f77_abort();
	}
}


anyval *
get_operand_address(type)
{
	register int mode, reg, base;

	mode = fetch_byte() & 0xff;
	reg = mode & 0xf;
	mode = (mode >> 4) & 0xf;
	switch (mode) {
		case LITERAL0:
		case LITERAL1:
		case LITERAL2:
		case LITERAL3:
			return NULL;
		
		case INDEXED:
			base = (int) get_operand_address(type);
			if (base == NULL) return NULL;
			base += contents_of_reg(reg)*type_length(type);
			return (anyval *) base;

		case REGISTER:
			return addr_of_reg(reg);
		
		case REG_DEF:
			return (anyval *) contents_of_reg(reg);
		
		case AUTO_DEC:
			return (anyval *) (contents_of_reg(reg)
				- type_length(type));

		case AUTO_INC:
			return (anyval *) contents_of_reg(reg);

		case AUTO_INC_DEF:
			return (anyval *) * (long *) contents_of_reg(reg);
		
		case BYTE_DISP:
			base = fetch_byte();
			base += contents_of_reg(reg);
			return (anyval *) base;
		
		case BYTE_DISP_DEF:
			base = fetch_byte();
			base += contents_of_reg(reg);
			return (anyval *) * (long *) base;

		case WORD_DISP:
			base = fetch_word();
			base += contents_of_reg(reg);
			return (anyval *) base;

		case WORD_DISP_DEF:
			base = fetch_word();
			base += contents_of_reg(reg);
			return (anyval *) * (long *) base;
		
		case LONG_DISP:
			base = fetch_long();
			base += contents_of_reg(reg);
			return (anyval *) base;

		case LONG_DISP_DEF:
			base = fetch_long();
			base += contents_of_reg(reg);
			return (anyval *) * (long *) base;
		
		default:
			fprintf(units[STDERR].ufd, "Bad mode 0x%x in get_addr()\n", mode);
			f77_abort();
	}
	return NULL;
}



contents_of_reg(reg)
{
	int value;

	if (reg == PC) value = (int) pc;
	else if (reg == SP) value = (int) &regs0t6[6];
	else if (reg == FP) value = regs0t6[-2];
	else if (reg == AP) value = regs0t6[-3];
	else if (reg >= 0 && reg <= 6) value = regs0t6[reg];
	else if (reg >= 7 && reg <= 11) value = regs7t11[reg];
	else {
		fprintf(units[STDERR].ufd, "Bad register 0x%x to contents_of()\n", reg);
		f77_abort();
		value = -1;
	}
	return value;
}


anyval *
addr_of_reg(reg)
{
	if (reg >= 0 && reg <= 6) {
		return (anyval *) &regs0t6[reg];
	}
	if (reg >= 7 && reg <= 11) {
		return (anyval *) &regs7t11[reg];
	}
	fprintf(units[STDERR].ufd, "Bad reg 0x%x to addr_of()\n", reg);
	f77_abort();
	return NULL;
}
/*
 *	fetch_{byte, word, long} - extract values from the PROGRAM area.
 *
 *	These routines are used in the operand decoding to extract various
 *	fields from where the program counter points.  This is because the
 *	addressing on the Vax is dynamic: the program counter advances
 *	while we are grabbing operands, as well as when we pass instructions.
 *	This makes things a bit messy, but I can't help it.
 */
fetch_byte()
{
	return *pc++;
}



fetch_word()
{
	int *old_pc;

	old_pc = (int *) pc;
	pc += 2;
	return *old_pc;
}



fetch_long()
{
	long *old_pc;

	old_pc = (long *) pc;
	pc += 4;
	return *old_pc;
}


type_length(type)
{
	if (type == F) return 4;
	if (type == D) return 8;
	fprintf(units[STDERR].ufd, "Bad type 0x%x in type_length()\n", type);
	f77_abort();
	return -1;
}



char *opcode_name(opcode)
{
	switch (opcode) {
		case ACBD: 	return "ACBD";
		case ACBF: 	return "ACBF";
		case ADDD2: 	return "ADDD2";
		case ADDD3: 	return "ADDD3";
		case ADDF2: 	return "ADDF2";
		case ADDF3: 	return "ADDF3";
		case CMPD: 	return "CMPD";
		case CMPF: 	return "CMPF";
		case CVTDB: 	return "CVTDB";
		case CVTDF: 	return "CVTDF";
		case CVTDL: 	return "CVTDL";
		case CVTDW: 	return "CVTDW";
		case CVTFB: 	return "CVTFB";
		case CVTFD: 	return "CVTFD";
		case CVTFL: 	return "CVTFL";
		case CVTFW: 	return "CVTFW";
		case CVTRDL: 	return "CVTRDL";
		case CVTRFL: 	return "CVTRFL";
		case DIVD2: 	return "DIVD2";
		case DIVD3: 	return "DIVD3";
		case DIVF2: 	return "DIVF2";
		case DIVF3: 	return "DIVF3";
		case EMODD: 	return "EMODD";
		case EMODF: 	return "EMODF";
		case MNEGD: 	return "MNEGD";
		case MNEGF: 	return "MNEGF";
		case MOVD: 	return "MOVD";
		case MOVF: 	return "MOVF";
		case MULD2: 	return "MULD2";
		case MULD3: 	return "MULD3";
		case MULF2: 	return "MULF2";
		case MULF3: 	return "MULF3";
		case POLYD: 	return "POLYD";
		case POLYF: 	return "POLYF";
		case SUBD2: 	return "SUBD2";
		case SUBD3: 	return "SUBD3";
		case SUBF2: 	return "SUBF2";
		case SUBF3: 	return "SUBF3";
		case TSTD: 	return "TSTD";
		case TSTF: 	return "TSTF";
	}
}
#endif	vax

#ifdef tahoe
/*
 *	NO RESERVED OPERAND EXCEPTION ON RESULT OF FP OVERFLOW ON TAHOE.
 * 	JUST PRINT THE OVERFLOW MESSAGE. RESULT IS 0 (zero).
 */

/*
 *	GLOBAL VARIABLES (we need a few)
 *
 *	Actual program counter and locations of registers.
 */
static char	*pc;
static int	*regs0t1;
static int	*regs2t12;
static int	max_messages;
static int	total_overflows;
static union	{
	long	v_long[2];
	double	v_double;
	} retrn;
static int	(*sigill_default)() = (SIG_VAL)-1;
static int	(*sigfpe_default)();


/*
 *	This routine sets up the signal handler for the floating-point
 *	and reserved operand interrupts.
 */

trapov_(count, rtnval)
	int *count;
	double *rtnval;
{
	extern got_overflow();

	sigfpe_default = signal(SIGFPE, got_overflow);
	total_overflows = 0;
	max_messages = *count;
	retrn.v_double = *rtnval;
}



/*
 *	got_overflow - routine called when overflow occurs
 *
 *	This routine just prints a message about the overflow.
 *	It is impossible to find the bad result at this point.
 * 	 NEXT 2 LINES DON'T HOLD FOR TAHOE !
 *	Instead, we wait until we get the reserved operand exception
 *	when we try to use it.  This raises the SIGILL signal.
 */

/*ARGSUSED*/
got_overflow(signo, codeword, sc)
	int signo, codeword;
	struct sigcontext *sc;
{
	int	*sp, i;
	FILE	*ef;

	signal(SIGFPE, got_overflow);
	ef = units[STDERR].ufd;
	switch (codeword) {
		case INT_OVF_T:
		case INT_DIV_T:
		case FLT_UND_T:
		case FLT_DIV_T:
				if (sigfpe_default > (SIG_VAL)7)
					return((*sigfpe_default)(signo, codeword, sc));
				else
					sigdie(signo, codeword, sc);
					/* NOTREACHED */

		case FLT_OVF_T:
				if (++total_overflows <= max_messages) {
					fprintf(ef, "trapov: %s",
						act_fpe[codeword-1].mesg);
					fprintf(ef, ": Current PC = %X", sc->sc_pc);
					if (total_overflows == max_messages)
						fprintf(ef, ": No more messages will be printed.\n");
					else
						fputc('\n', ef);
				}
				return;
	}
}
int 
ovcnt_()
{
	return total_overflows;
}
#endif tahoe
