/*
 * Code generation template numbers
 */

#define C_PUSH		 1	/* push anything onto stack */
#define C_PUSHI		 2	/* push immediate quantity onto stack */
#define C_PUSHZ		 3	/* push a zero onto stack */
#define C_PUSHA		 4	/* push address/offset onto stack */
#define C_PUSHC		 5	/* push the address of a constant */
#define C_PUSHL		 6	/* push a label onto stack */
#define C_POP		 7	/* pop the stack into some location */
#define C_MOV		 8	/* move one location to another */
#define C_MOVI		 9	/* move an immediate somewhere */
#define C_MOVA   	10	/* move an address to some location */
#define C_MOVL   	11	/* move a label to some location */
#define C_CLR		12	/* clear a location */
#define C_ADDTOP	13	/* add immediate quantity to top of stack */
#define	C_INIT		14	/* test initial flag of procedure */
#define C_CALL		15	/* call a subroutine */
#define C_JUMP		16	/* conditional jump to a local label */
#define C_LABEL	      	17	/* place a local label */
#define C_MISC		18	/* miscellaneous opcode and operand fields */
