/*
 * Opcode table structure
 */

struct opentry {
   char *op_name;		/* name of opcode */
   int   op_code;		/* opcode number */
   };

/*
 * External definitions
 */

extern struct opentry optable[];
extern int NOPCODES;

/*
 * Opcode definitions
 */

/* Operators */

#define OP_ASGN		 1
#define OP_BANG		 2
#define OP_CAT		 3
#define OP_COMPL	 4
#define OP_DIFF		 5
#define OP_DIV		 6
#define OP_EQV		 7
#define OP_INTER	 8
#define OP_LCONCAT	 9
#define OP_LEXEQ	10
#define OP_LEXGE	11
#define OP_LEXGT	12
#define OP_LEXLE	13
#define OP_LEXLT	14
#define OP_LEXNE	15
#define OP_MINUS	16
#define OP_MOD		17
#define OP_MULT		18
#define OP_NEG		19
#define OP_NEQV		20
#define OP_NONNULL	21
#define OP_NULL		22
#define OP_NUMBER	23
#define OP_NUMEQ	24
#define OP_NUMGE	25
#define OP_NUMGT	26
#define OP_NUMLE	27
#define OP_NUMLT	28
#define OP_NUMNE	29
#define OP_PLUS		30
#define OP_POWER	31
#define OP_RANDOM	32
#define OP_RASGN	33
#define OP_REFRESH	34
#define OP_RSWAP	35
#define OP_SECT		36
#define OP_SIZE		37
#define OP_SUBSC	38
#define OP_SWAP		39
#define OP_TABMAT	40
#define OP_TOBY		41
#define OP_UNIONCS	42
#define OP_VALUE	43

/* Instructions */

#define OP_BSCAN	44
#define OP_CCASE     	45
#define OP_CHFAIL    	46
#define OP_COACT     	47
#define OP_COFAIL    	48
#define OP_CORET     	49
#define OP_CREATE    	50
#define OP_CSET      	51
#define OP_DUP       	52
#define OP_EFAIL     	53
#define OP_ERET      	54
#define OP_ESCAN     	55
#define OP_ESUSP     	56
#define OP_FIELD     	57
#define OP_FILE      	58
#define OP_GOTO   	59
#define OP_INCRES 	60
#define OP_INIT   	61
#define OP_INT    	62
#define OP_INVOKE 	63
#define OP_KEYWD  	64
#define OP_LAB    	65
#define OP_LIMIT  	66
#define OP_LINE   	67
#define OP_LLIST  	68
#define OP_LSUSP  	69
#define OP_MARK   	70
#define OP_PFAIL  	71
#define OP_PNULL  	72
#define OP_POP    	73
#define OP_PRET   	74
#define OP_PSUSP  	75
#define OP_PUSH1  	76
#define OP_PUSHN1 	77
#define OP_REAL   	78
#define OP_SDUP   	79
#define OP_STR    	80
#define OP_UNMARK 	81
#define OP_VAR    	82

/* Declarations */

#define OP_PROC		83
#define OP_LOCAL	84
#define OP_CON		85
#define OP_DECLEND	86
#define OP_END		87

/* Global symbol table declarations */

#define OP_RECORD	88
#define OP_IMPL		89
#define OP_ERROR	90
#define OP_TRACE	91
#define OP_GLOBAL	92
