/*	align.c	1.1	86/07/20	*/

#include	"../tahoealign/align.h"

/*
 * Part of the parameter list is set up by locore.s.
 * First 2 dummy variables MUST BE the first local
 * variables; leaving place for registers 0 and 1
 * which are not preserved by the current C compiler.
 * Then, the array of structures and the last_operand
 * HAVE to be in the given order, to correspond to the
 * description of process_info in 'alignment.h'. 
 */
alignment()
{
	long	space_4_Register_1;	/* register 1 */
	long	space_4_Register_0;	/* register 0 */
	struct	oprnd	space_4_decoded[4];
	long	space_4_opcode;
	long	space_4_last_operand;	/* Last operand # processed */
	long	space_4_saved_pc;
	long	space_4_saved_sp;

	register process_info *infop;

	infop = (process_info *)&space_4_saved_sp;
	saved_pc = pc;
	saved_sp = sp;	     			/* For possible exceptions */

	last_operand = -1;   /* To get the operand routine going correctly */

	opCODE = 0xff & *(char *)pc;
	pc++;
	(*Table[opCODE].routine) (infop);	/* Call relevant handler */
	/*
	 * NOTE: nothing should follow, except the return.
	 * The register variables cannot be trusted anymore,
	 * if an exception is signalled.  See 'exception.c'
	 * to understand why.
	 */
}
