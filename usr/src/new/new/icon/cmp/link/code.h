/*
 * Code generation template numbers
 */

#define C_PUSH		 1	/* push anything onto stack */
#define C_PUSHI		 2	/* push immediate quantity onto stack */
#define C_PUSHN		 3	/* push a null descriptor on the stack */
#define C_PUSHZ		 4	/* push a zero onto stack */
#define C_PUSHA		 5	/* push address/offset onto stack */
#define C_PUSHC		 6	/* push the address of a constant */
#define C_PUSHL		 7	/* push a label onto stack */
#define C_POP		 8	/* pop the stack into some location */
#define C_MOV		 9	/* move one location to another */
#define C_MOVI		10	/* move an immediate somewhere */
#define C_MOVA   	11	/* move an address to some location */
#define C_MOVL   	12	/* move a label to some location */
#define C_CLR		13	/* clear a location */
#define C_ADDTOP	14	/* add immediate quantity to top of stack */
#define	C_INIT		15	/* test initial flag of procedure */
#define C_CALLJ		16	/* jump to a subroutine */
#define C_CALL		17	/* call a subroutine */
#define C_JUMP		18	/* conditional jump to a local label */
#define C_LABEL	      	19	/* place a local label */
#define C_MISC		20	/* miscellaneous opcode and operand fields */

#ifdef VAX
#define BYTES		4
char *iformat	=	"\t.long %d;";
char *nameformat=	"\t.long %d,_ident+%d\t# %s\n";
#define r(i)		(1<<(i))
#define C_MOVQ		26	/* move a quadword */
#define C_POPR		27	/* pop registers */
#define C_PUSHR		28	/* push registers */
#define C_PUSHIN	29	/* push an indexed value */
#endif

#ifdef PDP11
#define BYTES		2
char *iformat	=	"\t%06o;";
char *nameformat=	"\t%06o;\t_ident+%o\t/ %s\n";
#endif

