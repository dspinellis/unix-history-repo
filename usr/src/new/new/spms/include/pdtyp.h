/* $Header$ */

/*
 * Project directory type label definitions
 *
 * Author: Peter J. Nicklin
 */

/*
 * Boolean operation definitions
 *
 * Note: Definitions B_ID, B_NOT, B_AND, B_OR, B_LPAREN, B_RPAREN, B_EOS
 * must not be changed because they are used as indices for the operator
 * precedence parsing table. B_ID is also used as the default initializer
 * for the type expression token table.
 */
#define B_ID		0		/* boolean expression identifier */
#define B_NOT		1		/* boolean "not" operation */
#define B_AND		2		/* boolean "and" operation */
#define B_OR		3		/* boolean "or" operation */
#define B_LPAREN	4		/* "(" */
#define B_RPAREN	5		/* ")" */
#define B_EOS		6		/* end of boolean string */
#define B_WHITE		7		/* tab or blank */
/*
 * Project directory boolean type expression token struct
 */
typedef struct _postfix
	{
	short p_class;			/* type of boolean expression token */
	short p_sw;			/* TRUE/FALSE switch */
	char *p_id;			/* boolean expression identifier */
	char *p_label;			/* project directory type label */
	} POSTFIX;
/*
 * Project directory postfix type expression struct
 */
typedef struct _pdtyp
	{
	int pfxsize;			/* size of postfix expression */
	POSTFIX *pfx;			/* base of postfix expression */
	short *eval;			/* base of expr evaluation stack */
	} PDTYP;
