/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)trpfpe_.c	5.7 (Berkeley) %G%";
#endif /* not lint */

/* #define OLD_BSD if you're running < 4.2 bsd */

/*
 *	Fortran floating-point error handler
 *
 *	Synopsis:
 *		call trpfpe (n, retval)
 *			causes floating point faults to be trapped, with the
 *			first 'n' errors getting a message printed.
 *			'retval' is put in place of the bad result.
 *		k = fpecnt()
 *			causes 'k' to get the number of errors since the
 *			last call to trpfpe().
 *
 *		common /fpeflt/ fpflag
 *		logical fpflag
 *			fpflag will become .true. on faults
 *
 *	David Wasley, UCBerkeley, June 1983.
 */


#include <stdio.h>
#include <sys/signal.h>
#include "../libI77/fiodefs.h"

#define	SIG_VAL		void (*)()

#ifdef vax
#include "opcodes.h"
#include "operand.h"

struct arglist {		/* what AP points to */
	long	al_numarg;	/* only true in CALLS format */
	long	al_arg[256];
};

struct cframe {			/* VAX call frame */
	long		cf_handler;
	unsigned short	cf_psw;
	unsigned short	cf_mask;
	struct arglist	*cf_ap;
	struct cframe	*cf_fp;
	char		*cf_pc;
};

/*
 * bits in the PSW
 */
#define	PSW_V	0x2
#define	PSW_FU	0x40
#define	PSW_IV	0x20

/*
 * where the registers are stored as we see them in the handler
 */
struct reg0_6 {
	long	reg[7];
};

struct reg7_11 {
	long	reg[5];
};

#define	iR0	reg0_6->reg[0]
#define	iR1	reg0_6->reg[1]
#define	iR2	reg0_6->reg[2]
#define	iR3	reg0_6->reg[3]
#define	iR4	reg0_6->reg[4]
#define	iR5	reg0_6->reg[5]
#define	iR6	reg0_6->reg[6]
#define	iR7	reg7_11->reg[0]
#define	iR8	reg7_11->reg[1]
#define	iR9	reg7_11->reg[2]
#define	iR10	reg7_11->reg[3]
#define	iR11	reg7_11->reg[4]

union objects {		/* for load/store */
	char	ua_byte;
	short	ua_word;
	long	ua_long;
	float	ua_float;
	double	ua_double;
	union objects	*ua_anything;
};

typedef union objects	anything;
enum object_type { BYTE, WORD, LONG, FLOAT, QUAD, DOUBLE, UNKNOWN };


/*
 * assembly language assist
 * There are some things you just can't do in C
 */
asm(".text");

struct cframe	*myfp();
asm("_myfp: .word 0x0");
	asm("movl 12(fp),r0");
	asm("ret");

struct arglist	*myap();
asm("_myap: .word 0x0");
	asm("movl 8(fp),r0");
	asm("ret");

char	*mysp();
asm("_mysp: .word 0x0");
	asm("extzv $30,$2,4(fp),r0");
	asm("addl2 ap,r0");	/* SP in caller is AP+4 here + SPA bits! */
	asm("addl2 $4,r0");
	asm("ret");

char	*mypc();
asm("_mypc: .word 0x0");
	asm("movl 16(fp),r0");
	asm("ret");

asm(".data");


/*
 * Where interrupted objects are
 */
static struct cframe	**ifp;	/* addr of saved FP */
static struct arglist	**iap;	/* addr of saved AP */
static char		 *isp;	/* value of interrupted SP */
static char		**ipc;	/* addr of saved PC */
static struct reg0_6	*reg0_6;/* registers 0-6 are saved on the exception */
static struct reg7_11	*reg7_11;/* we save 7-11 by our entry mask */
static anything		*result_addr;	/* where the dummy result goes */
static enum object_type	 result_type;	/* what kind of object it is */

/*
 * some globals
 */
static union {
	long	rv_long[2];
	float	rv_float;
	double	rv_double;
			} retval; /* the user specified dummy result */
static int	max_messages	= 1;		/* the user can tell us */
static int	fpe_count	= 0;		/* how bad is it ? */
       long	fpeflt_		= 0;	/* fortran "common /fpeflt/ flag" */
static sig_t	sigfpe_dfl	= SIG_DFL;	/* if we can't fix it ... */

/*
 * The fortran unit control table
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


/*
 * Get the address of the (saved) next operand & update saved PC.
 * The major purpose of this is to determine where to store the result.
 * There is one case we can't deal with: -(SP) or (SP)+
 * since we can't change the size of the stack.
 * Let's just hope compilers don't generate that for results.
 */

anything *
get_operand (oper_size)
	int	oper_size;	/* size of operand we expect */
{
	register int	regnum;
	register int	operand_code;
	int		index;
	anything	*oper_addr;
	anything	*reg_addr;

	regnum = (**ipc & 0xf);
	if (regnum == PC)
		operand_code = (*(*ipc)++ & 0xff);
	else
		operand_code = (*(*ipc)++ & 0xf0);
	if (regnum <= R6)
		reg_addr = (anything *)&reg0_6->reg[regnum];
	else if (regnum <= R11)
		reg_addr = (anything *)&reg7_11->reg[regnum];
	else if (regnum == AP)
		reg_addr = (anything *)iap;
	else if (regnum == FP)
		reg_addr = (anything *)ifp;
	else if (regnum == SP)
		reg_addr = (anything *)&isp;	/* We saved this ourselves */
	else if (regnum == PC)
		reg_addr = (anything *)ipc;


	switch (operand_code)
	{
		case IMMEDIATE:
			oper_addr = (anything *)(*ipc);
			*ipc += oper_size;
			return(oper_addr);

		case ABSOLUTE:
			oper_addr = (anything *)(**ipc);
			*ipc += sizeof (anything *);
			return(oper_addr);

		case LITERAL0:
		case LITERAL1:
		case LITERAL2:
		case LITERAL3:
			/* we don't care about the address of these */
			return((anything *)0);

		case INDEXED:
			index = reg_addr->ua_long * oper_size;
			oper_addr = (anything *)(get_operand(sizeof (long))->ua_long + index);
			return(oper_addr);

		case REGISTER:
			return(reg_addr);

		case REGDEFERED:
			return(reg_addr->ua_anything);

		case AUTODEC:
			if (regnum == SP)
			{
				fprintf(stderr, "trp: can't fix -(SP) operand\n");
				exit(1);
			}
			reg_addr->ua_long -= oper_size;
			oper_addr = reg_addr->ua_anything;
			return(oper_addr);

		case AUTOINC:
			if (regnum == SP)
			{
				fprintf(stderr, "trp: can't fix (SP)+ operand\n");
				exit(1);
			}
			oper_addr = reg_addr->ua_anything;
			reg_addr->ua_long += oper_size;
			return(oper_addr);

		case AUTOINCDEF:
			if (regnum == SP)
			{
				fprintf(stderr, "trp: can't fix @(SP)+ operand\n");
				exit(1);
			}
			oper_addr = (reg_addr->ua_anything)->ua_anything;
			reg_addr->ua_long += sizeof (anything *);
			return(oper_addr);

		case BYTEDISP:
		case BYTEREL:
			index = ((anything *)(*ipc))->ua_byte;
			*ipc += sizeof (char);	/* do it now in case reg==PC */
			oper_addr = (anything *)(index + reg_addr->ua_long);
			return(oper_addr);

		case BYTEDISPDEF:
		case BYTERELDEF:
			index = ((anything *)(*ipc))->ua_byte;
			*ipc += sizeof (char);	/* do it now in case reg==PC */
			oper_addr = (anything *)(index + reg_addr->ua_long);
			oper_addr = oper_addr->ua_anything;
			return(oper_addr);

		case WORDDISP:
		case WORDREL:
			index = ((anything *)(*ipc))->ua_word;
			*ipc += sizeof (short);	/* do it now in case reg==PC */
			oper_addr = (anything *)(index + reg_addr->ua_long);
			return(oper_addr);

		case WORDDISPDEF:
		case WORDRELDEF:
			index = ((anything *)(*ipc))->ua_word;
			*ipc += sizeof (short);	/* do it now in case reg==PC */
			oper_addr = (anything *)(index + reg_addr->ua_long);
			oper_addr = oper_addr->ua_anything;
			return(oper_addr);

		case LONGDISP:
		case LONGREL:
			index = ((anything *)(*ipc))->ua_long;
			*ipc += sizeof (long);	/* do it now in case reg==PC */
			oper_addr = (anything *)(index + reg_addr->ua_long);
			return(oper_addr);

		case LONGDISPDEF:
		case LONGRELDEF:
			index = ((anything *)(*ipc))->ua_long;
			*ipc += sizeof (long);	/* do it now in case reg==PC */
			oper_addr = (anything *)(index + reg_addr->ua_long);
			oper_addr = oper_addr->ua_anything;
			return(oper_addr);

		/* NOTREACHED */
	}
}

/*
 * Trap & repair floating exceptions so that a program may proceed.
 * There is no notion of "correctness" here; just the ability to continue.
 *
 * The on_fpe() routine first checks the type code to see if the
 * exception is repairable. If so, it checks the opcode to see if
 * it is one that it knows. If this is true, it then simulates the
 * VAX cpu in retrieving operands in order to increment iPC correctly.
 * It notes where the result of the operation would have been stored
 * and substitutes a previously supplied value.
 */

#ifdef	OLD_BSD
on_fpe(signo, code, myaddr, pc, ps)
	int signo, code, ps;
	char *myaddr, *pc;
#else
void
on_fpe(signo, code, sc, grbg)
	int signo, code;
	struct sigcontext *sc;
#endif
{
	/*
	 * There must be at least 5 register variables here
	 * so our entry mask will save R11-R7.
	 */
	register long	*stk;
	register long	*sp;
	register struct arglist	*ap;
	register struct cframe	*fp;
	register FILE	*ef;

	ef = units[STDERR].ufd;		/* fortran error stream */

	switch (code)
	{
		case FPE_INTOVF_TRAP:	/* integer overflow */
		case FPE_INTDIV_TRAP:	/* integer divide by zero */
		case FPE_FLTOVF_TRAP:	/* floating overflow */
		case FPE_FLTDIV_TRAP:	/* floating/decimal divide by zero */
		case FPE_FLTUND_TRAP:	/* floating underflow */
		case FPE_DECOVF_TRAP:	/* decimal overflow */
		case FPE_SUBRNG_TRAP:	/* subscript out of range */
		default:
cant_fix:
			if (sigfpe_dfl > (SIG_VAL)7)	/* user specified */
#ifdef	OLD_BSD
				(*sigfpe_dfl)(signo, code, myaddr, pc, ps);
#else
				(*sigfpe_dfl)(signo, code, sc, grbg);
#endif
			else
#ifdef	OLD_BSD
				sigdie(signo, code, myaddr, pc, ps);
#else
				sigdie(signo, code, sc, grbg);
#endif
			/* NOTREACHED */

		case FPE_FLTOVF_FAULT:	/* floating overflow fault */
		case FPE_FLTDIV_FAULT:	/* divide by zero floating fault */
		case FPE_FLTUND_FAULT:	/* floating underflow fault */
			if (++fpe_count <= max_messages) {
				fprintf(ef, "trpfpe: %s",
					act_fpe[code-1].mesg);
				if (fpe_count == max_messages)
					fprintf(ef, ": No more messages will be printed.\n");
				else
					fputc('\n', ef);
			}
			fpeflt_ = -1;
			break;
	}

	ap = myap();			/* my arglist pointer */
	fp = myfp();			/* my frame pointer */
	ifp = &(fp->cf_fp)->cf_fp;	/* user's stored in next frame back */
	iap = &(fp->cf_fp)->cf_ap;
	/*
	 * these are likely to be system dependent
	 */
	reg0_6 = (struct reg0_6 *)((char *)fp->cf_fp + sizeof (struct cframe));
	reg7_11 = (struct reg7_11 *)((char *)fp->cf_fp - sizeof (struct reg7_11));

#ifdef	OLD_BSD
	ipc = &pc;
	isp = (char *)&ap->al_arg[ap->al_numarg + 2];	/* assumes 2 dummys */
	ps &= ~(PSW_V|PSW_FU);
#else
	ipc = (char **)&sc->sc_pc;
	isp = (char *)sc + sizeof (struct sigcontext);
	sc->sc_ps &= ~(PSW_V|PSW_FU);
#endif


	switch (*(*ipc)++)
	{
		case ADDD3:
		case DIVD3:
		case MULD3:
		case SUBD3:
			(void) get_operand(sizeof (double));
			/* intentional fall-thru */

		case ADDD2:
		case DIVD2:
		case MULD2:
		case SUBD2:
		case MNEGD:
		case MOVD:
			(void) get_operand(sizeof (double));
			result_addr = get_operand(sizeof (double));
			result_type = DOUBLE;
			break;

		case ADDF3:
		case DIVF3:
		case MULF3:
		case SUBF3:
			(void) get_operand(sizeof (float));
			/* intentional fall-thru */

		case ADDF2:
		case DIVF2:
		case MULF2:
		case SUBF2:
		case MNEGF:
		case MOVF:
			(void) get_operand(sizeof (float));
			result_addr = get_operand(sizeof (float));
			result_type = FLOAT;
			break;

		case CVTDF:
			(void) get_operand(sizeof (double));
			result_addr = get_operand(sizeof (float));
			result_type = FLOAT;
			break;

		case CVTFD:
			(void) get_operand(sizeof (float));
			result_addr = get_operand(sizeof (double));
			result_type = DOUBLE;
			break;

		case EMODF:
		case EMODD:
			fprintf(ef, "trpfpe: can't fix emod yet\n");
			goto cant_fix;

		case POLYF:
		case POLYD:
			fprintf(ef, "trpfpe: can't fix poly yet\n");
			goto cant_fix;

		case ACBD:
		case ACBF:
		case CMPD:
		case CMPF:
		case TSTD:
		case TSTF:
		case CVTDB:
		case CVTDL:
		case CVTDW:
		case CVTFB:
		case CVTFL:
		case CVTFW:
		case CVTRDL:
		case CVTRFL:
			/* These can generate only reserved operand faults */
			/* They are shown here for completeness */

		default:
			fprintf(stderr, "trp: opcode 0x%02x unknown\n",
				*(--(*ipc)) & 0xff);
			goto cant_fix;
			/* NOTREACHED */
	}

	if (result_type == FLOAT)
		result_addr->ua_float = retval.rv_float;
	else
	{
		if (result_addr == (anything *)&iR6)
		{	/*
			 * special case - the R6/R7 pair is stored apart
			 */
			result_addr->ua_long = retval.rv_long[0];
			((anything *)&iR7)->ua_long = retval.rv_long[1];
		}
		else
			result_addr->ua_double = retval.rv_double;
	}
	signal(SIGFPE, on_fpe);
}

trpfpe_ (count, rval)
	long	*count;	/* how many to announce */
	double	*rval;	/* dummy return value */
{
	max_messages = *count;
	retval.rv_double = *rval;
	sigfpe_dfl = signal(SIGFPE, on_fpe);
	fpe_count = 0;
}

long
fpecnt_ ()
{
	return (fpe_count);
}
#endif vax

#ifdef tahoe
/*
 *	This handler just prints a message. It cannot fix anything
 * 	on Power6 because of its fpp architecture. In any case, there
 * 	are no arithmetic faults (only traps) around, so that no instruction
 *	is interrupted befor it completes, and PC points to the next floating
 *	point instruction (not necessarily next executable instr after the one
 *	that got the exception).
 */

struct arglist {		/* what AP points to */
	long	al_arg[256];
};

struct reg0_1 {
	long	reg[2];
};
struct reg2_12 {
	long	reg[11];
};
#include <sys/types.h>
#include <frame.h>
#include "sigframe.h"

/*
 * bits in the PSL
 */
#include <machine/psl.h>

/*
 * where the registers are stored as we see them in the handler
 */


#define	iR0	reg0_1->reg[1]
#define	iR1	reg0_1->reg[0]

#define	iR2	reg2_12->reg[0]
#define	iR3	reg2_12->reg[1]
#define	iR4	reg2_12->reg[2]
#define	iR5	reg2_12->reg[3]
#define	iR6	reg2_12->reg[4]
#define	iR7	reg2_12->reg[5]
#define	iR8	reg2_12->reg[6]
#define	iR9	reg2_12->reg[7]
#define	iR10	reg2_12->reg[8]
#define	iR11	reg2_12->reg[9]
#define	iR12	reg2_12->reg[10]

union objects {		/* for load/store */
	char	ua_byte;
	short	ua_word;
	long	ua_long;
	float	ua_float;
	double	ua_double;
	union objects	*ua_anything;
};

typedef union objects	anything;
enum object_type { BYTE, WORD, LONG, FLOAT, QUAD, DOUBLE, UNKNOWN };


/*
 * assembly language assist
 * There are some things you just can't do in C
 */
asm(".text");

long *myfp();
asm("_myfp: .word 0");
	asm("movl (fp),r0");
	asm("ret");

struct frame *framep(p)
long *p;
{
	return((struct frame *)(p-2));
}

struct arglist	*argp(p) 
long *p;
{
	return((struct arglist *)(p+1));
}

char	*mysp();
asm("_mysp: .word 0");
	asm("addl3 $4,fp,r0");
	asm("ret");

char	*mypc();
asm("_mypc: .word 0");
	asm("movl -8(fp),r0");
	asm("ret");

asm(".data");


/*
 * Where interrupted objects are
 */
static struct frame	*ifp;	/* addr of saved FP */
static struct arglist	*iap;	/* addr of saved AP */
static char		 *isp;	/* value of interrupted SP */
static char		**ipc;	/* addr of saved PC */
static struct reg0_1	*reg0_1;/* registers 0-1 are saved on the exception */
static struct reg2_12	*reg2_12;/* we save 2-12 by our entry mask */
static anything		*result_addr;	/* where the dummy result goes */
static enum object_type	 result_type;	/* what kind of object it is */

/*
 * some globals
 */
static union {
	long	rv_long[2];
	float	rv_float;
	double	rv_double;
			} retval; /* the user specified dummy result */
static int	max_messages	= 1;		/* the user can tell us */
static int	fpe_count	= 0;		/* how bad is it ? */
       long	fpeflt_		= 0;	/* fortran "common /fpeflt/ flag" */
static sig_t sigfpe_dfl		= SIG_DFL;	/* if we can't fix it ... */

/*
 * The fortran unit control table
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


/* VALID ONLY ON VAX !!!
 *
 * Get the address of the (saved) next operand & update saved PC.
 * The major purpose of this is to determine where to store the result.
 * There is one case we can't deal with: -(SP) or (SP)+
 * since we can't change the size of the stack.
 * Let's just hope compilers don't generate that for results.
 */


/*
 * Trap & repair floating exceptions so that a program may proceed.
 * There is no notion of "correctness" here; just the ability to continue.
 *
 * The on_fpe() routine first checks the type code to see if the
 * exception is repairable. If so, it checks the opcode to see if
 * it is one that it knows. If this is true, it then simulates the
 * VAX cpu in retrieving operands in order to increment iPC correctly.
 * It notes where the result of the operation would have been stored
 * and substitutes a previously supplied value.
 *  DOES NOT REPAIR ON TAHOE !!!
 */
void
on_fpe(signo, code, sc)
	int signo, code;
	struct sigcontext *sc;
{
	/*
	 * There must be at least 11 register variables here
	 * so our entry mask will save R12-R2.
	 */
	register long	*stk;
	register long	*sp, *rfp;
	register struct arglist	*ap;
	register struct frame	*fp;
	register FILE	*ef;
	register struct sigframe *sfp;
	register long dmy1, dmy2, dmy3, dmy4;

	dmy1 = dmy2 = dmy3 = dmy4 = 0;

	ef = units[STDERR].ufd;		/* fortran error stream */

	switch (code)
	{
		case FPE_INTOVF_TRAP:	/* integer overflow */
		case FPE_INTDIV_TRAP:	/* integer divide by zero */
		case FPE_FLTOVF_TRAP:	/* floating overflow */
		case FPE_FLTDIV_TRAP:	/* floating divide by zero */
		case FPE_FLTUND_TRAP:	/* floating underflow */
		default:
cant_fix:
			if (sigfpe_dfl > (SIG_VAL)7)	/* user specified */
				(*sigfpe_dfl)(signo, code, sc);
			else
			if (++fpe_count <= max_messages) {
				fprintf(ef, "trpfpe: %s",
					act_fpe[code-1].mesg);
				if (fpe_count == max_messages)
					fprintf(ef, ": No more messages will be printed.\n");
				else
					fputc('\n', ef);
			}
			fpeflt_ = -1;
			break;
	}

/*
 * Find all the registers just in case something better can be done.
 */

	rfp = myfp();			/* contents of fp register */
	ap = argp(rfp);			/* my arglist pointer */
	fp = framep(rfp);		/* my frame pointer */
	ifp = framep(*rfp);		/* user's stored in next frame back */
	iap = argp(*rfp);

	sfp = (struct sigframe *)ap;	/* sigframe contains at its bottom the
					   signal handler arguments */

	reg0_1 = (struct reg0_1 *)&sfp->r1;
	reg2_12 = (struct reg2_12 *)((char *)fp - sizeof (struct reg2_12));

	ipc = (char **)&sc->sc_pc;
	isp = (char *)sc + sizeof (struct sigcontext);
	sc->sc_ps &= ~(PSL_V|PSL_FU);

	fprintf(ef, "Current PC = %X \n", sc->sc_pc);

	signal(SIGFPE, on_fpe);
	sigdie(signo, code, sc);
}

trpfpe_ (count, rval)
	long	*count;	/* how many to announce */
	double	*rval;	/* dummy return value */
{
	max_messages = *count;
	retval.rv_double = *rval;
	sigfpe_dfl = signal(SIGFPE, on_fpe);
	fpe_count = 0;
}

long
fpecnt_ ()
{
	return (fpe_count);
}

#endif tahoe
