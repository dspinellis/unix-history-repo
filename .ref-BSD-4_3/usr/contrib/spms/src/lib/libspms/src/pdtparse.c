/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pdtparse() translates an infix project directory boolean type label
 * expression to postfix, using operator-precedence parsing. An adjacency
 * matrix augments the parsing method by detecting tokens that must not
 * be adjacent to one another. Returns NO if syntax error or out of memory,
 * otherwise YES.
 *
 * The maximum size of the input stack used by the parser determines the
 * size of the postfix expression evaluation stack.
 */
#include "null.h"
#include "macro.h"
#include "pdtyp.h"
#include "truefalse.h"
#include "yesno.h"

#define INCRISTAK	20		/* amount to increase input stack */
#define MAXISTAK	20		/* initial size of input stack */
#define INCRPEXP	40		/* amount to increase postfix expr */
#define MAXPEXP		40		/* initial size of postfix expr */
#define TOKTABSIZE	256		/* token lookup table size */

static char *type;			/* current type label */
static int intok;			/* current input token */
static int typlen;			/* length of current type label */
static short *istak;			/* boolean expression input stack */
static short toktab[TOKTABSIZE];	/* token lookup table */

static short opr[7][7] =		/* operator precedence relations */
	{
	/*id   !    &    |    (    )    B_EOS	*/
	{ 0 ,  0 , '>', '>',  0 , '>', '>'},	/* id */
	{'<',  0 , '>', '>', '<', '>', '>'},	/* ! */
	{'<', '<', '>', '>', '<', '>', '>'},	/* & */
	{'<', '<', '<', '>', '<', '>', '>'},	/* | */
	{'<', '<', '<', '<', '<', '=',  0 },	/* ( */
	{ 0 ,  0 , '>', '>',  0 , '>', '>'},	/* ) */
	{'<', '<', '<', '<', '<',  0 ,  0 },	/* B_EOS */
	};

static short adj[7][7] =		/* token adjacency matrix */
	{
	/*id   !    &    |    (    )    B_EOS	*/
	{ NO,  NO, YES, YES,  NO, YES, YES},	/* id */
	{YES,  NO,  NO,  NO, YES,  NO,  NO},	/* ! */
	{YES, YES,  NO,  NO, YES,  NO,  NO},	/* & */
	{YES, YES,  NO,  NO, YES,  NO,  NO},	/* | */
	{YES, YES,  NO,  NO, YES,  NO,  NO},	/* ( */
	{ NO,  NO, YES, YES,  NO, YES, YES},	/* ) */
	{YES, YES,  NO,  NO, YES,  NO,  NO},	/* B_EOS */
	};

pdtparse(typexpr, postfix)
	char *typexpr;			/* boolean type expression */
	PDTYP *postfix;			/* postfix expression struct */
{
	register int lastok;		/* previous input token */
	register int maxestak;		/* maximum size of evaluation stack */
	register int poptok;		/* token most recently popped */
	register int pp;		/* postfix expression pointer */
	register int toi;		/* top-of-input-stack pointer */
	register int toitok;		/* topmost token on stack */
	char *getnextok();		/* get next type expression token */
	char *malloc();			/* memory allocator */
	char *realloc();		/* reallocate memory block */
	int maxistak;			/* maximum size of input stack */
	int maxpexp;			/* maximum size of postfix expression */

	/* initialize token lookup table */
	toktab['\0'] = B_EOS;
	toktab['\t'] = B_WHITE;
	toktab[' '] = B_WHITE;
	toktab['&'] = B_AND;
	toktab['|'] = B_OR;
	toktab['!'] = B_NOT;
	toktab['('] = B_LPAREN;
	toktab[')'] = B_RPAREN;
	/* the rest of the lookup table is B_ID which is 0 */

	/* initialize boolean type expression input stack */
	maxistak = MAXISTAK;
	if ((istak = (short *) malloc((unsigned)maxistak*sizeof(short))) == NULL)
		goto nomemory;
	maxestak = toi = 0;
	istak[toi] = B_EOS;

	/* initialize postfix type expression */
	maxpexp = MAXPEXP;
	if ((postfix->pfx = (POSTFIX *) malloc((unsigned)maxpexp*sizeof(POSTFIX))) == NULL)
		goto nomemory;
	pp = 0;

	/* get first token */
	lastok = B_EOS;
	typexpr = getnextok(typexpr);
	if (adj[lastok][intok] == NO)
		goto badsyntax;

	while (TRUE)
		{
		if (toi == 0 && intok == B_EOS)
			break;	/* ACCEPT */
		toitok = istak[toi];
		if (opr[toitok][intok] == '<' || opr[toitok][intok] == '=')
			{ /* SHIFT current input token onto the stack */
			toi++;
			maxestak = MAX(maxestak, toi);
			if (toi >= maxistak)
				{ /* increase input stack size */
				maxistak += INCRISTAK;
				if ((istak = (short *) realloc((char *)istak,
				     (unsigned)maxistak*sizeof(short))) == NULL)
					goto nomemory;
				}
			istak[toi] = lastok = intok;
			typexpr = getnextok(typexpr);
			if (adj[lastok][intok] == NO)
				goto badsyntax;
			}
		else if (opr[toitok][intok] == '>')
			do	{ /* REDUCE */
				if (pp >= maxpexp)
					{ /* increase postfix expression size */
					maxpexp += INCRPEXP;
					if ((postfix->pfx = (POSTFIX *)
					     realloc((char *)postfix->pfx,
						     (unsigned)maxpexp*sizeof(POSTFIX))) == NULL)
						goto nomemory;
					}
				switch (toitok)
					{
					case B_ID:
						type[typlen] = '\0';
						(postfix->pfx)[pp].p_id = type;
					case B_AND:
					case B_OR:
					case B_NOT:
						(postfix->pfx)[pp].p_class = toitok;
						pp++;
						break;
					}
				toi--;
				poptok = toitok;
				toitok = istak[toi];
				} while (opr[toitok][poptok] != '<');
		else	{
			goto badsyntax;
			}
		}
	free((char *)istak);
	if ((postfix->eval = (short *) malloc((unsigned)(maxestak+1)*sizeof(short))) == NULL)
		goto nomemory;
	postfix->pfxsize = pp;
	return(YES);
badsyntax:
	warn("type expression syntax error");
	free((char *) istak);
	free((char *) postfix->pfx);
	return(NO);
nomemory:
	warn("out of memory");
	return(NO);
}



/*
 * getnextok() identifies the next token in the boolean type expression
 * and returns an updated pointer to the expression. `type' points to
 * the current type label in the boolean expression.
 */
static char *
getnextok(t)
	register char *t;		/* current pointer to type expr */
{
	register int i;			/* type label buffer index */

	while (WHITESPACE(*t))
		t++;
	intok = toktab[*t];
	if (intok == B_ID)
		{
		type = t;
		for (i = 0; toktab[*t] == B_ID; i++, t++)
			continue;
		typlen = i;
		}
	else if (intok != B_EOS)
		{
		t++;
		}
	return(t);
}
