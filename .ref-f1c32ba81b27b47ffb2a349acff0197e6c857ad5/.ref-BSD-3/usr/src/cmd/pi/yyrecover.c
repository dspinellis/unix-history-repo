/* Copyright (c) 1979 Regents of the University of California */
#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.1 February 1978
 *
 *
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.1 February 1978
 */

#include "whoami"
#include "0.h"
#include "yy.h"

/*
 * Very simplified version of Graham-Rhodes error recovery
 * method for LALR parsers.  Backward move is embodied in
 * default reductions of the yacc parser until an error condition
 * is reached.  Forward move is over a small number of input tokens
 * and cannot "condense".  The basic corrections are:
 *
 *	1) Delete the input token.
 *
 *	2) Replace the current input with a legal input.
 *
 *	3) Insert a legal token.
 *
 * All corrections are weighted, considered only if they allow
 * at least two shifts, and the cost of a correction increases if
 * it allows shifting over only a part of the lookahead.
 *
 * Another error situation is that which occurs when an identifier "fails"
 * a reduction because it is not the required "class".
 * In this case, we also consider replacing this identifier, which has
 * already been shifted over, with an identifier of the correct class.
 *
 * Another correction performed here is unique symbol insertion.
 * If the current state admits only one input, and no other alternative
 * correction presents itself, then that symbol will be inserted.
 * There is a danger in this of looping, and it is handled
 * by counting true shifts over input (see below).
 *
 *
 * A final class of corrections, considered only when the error
 * occurred immediately after a shift over a terminal, involves
 * the three basic corrections above, but with the point of error
 * considered to be before this terminal was shifted over, effectively
 * "unreading" this terminal.  This is a feeble attempt at elimination
 * of the left-right bias and because "if" has a low weight and some
 * statements are quite simple i.e.
 *
 *	cse ch of ...
 *
 * we can get a small number of errors.  The major deficiency of
 * this is that we back up only one token, and that the forward
 * move is over a small number of tokens, often not enough to really
 * tell what the input should be, e.g. in
 *
 *	a[i] > a[i - 1] ...
 *
 * In such cases a bad identifier (misspelled keyword) or omitted
 * keyword will be change or inserted as "if" as it has the lowest cost.
 * This is not terribly bad, as "if"s are most common.
 * This also allows the correction of other errors.
 *
 * This recovery depends on the default reductions which delay
 * noticing the error until the parse reaches a state where the
 * relevant "alternatives" are visible.  Note that it does not
 * consider tokens which will cause reductions before being
 * shifted over.  This requires the grammar to be written in a
 * certain way for the recovery to work correctly.
 * In some sense, also, the recovery suffers because we have
 * LALR(1) tables rather than LR(1) tables, e.g. in
 *
 *	if rec.field < rec2,field2 then
 */

/*
 * Definitions of possible corrective actions
 */
#define	CPANIC		0
#define	CDELETE		1
#define	CREPLACE	2
#define	CINSERT		3
#define	CUNIQUE		4
#define	CCHIDENT	5

/*
 * Multiplicative cost factors for corrective actions.
 *
 * When an error occurs we take YCSIZ - 1 look-ahead tokens.
 * If a correction being considered will shift over only part of
 * that look-ahead, it is not completely discarded, but rather
 * "weighted", its cost being multiplied by a weighting factor.
 * For a correction to be considered its weighted cost must be less
 * than CLIMIT.
 *
 * Non-weighted costs are considered:
 *
 *	LOW	<= 3
 *	MEDIUM	4,5
 *	HIGH	>= 6
 *
 * CURRENT WEIGHTING STRATEGY: Aug 20, 1977
 *
 * For all kinds of corrections we demand shifts over two symbols.
 * Corrections have high weight even after two symbol
 * shifts because the costs for deleting and inserting symbols are actually
 * quite low; we do not want to change weighty symbols 
 * on inconclusive evidence.
 *
 * The weights are the same after the third look ahead.
 * This prevents later, unrelated errors from causing "funny"
 * biases of the weights toward one type of correction.
 *
 * Current look ahead is 5 symbols.
 */

/*** CLIMIT is defined in yy.h for yycosts ***/
#define	CPRLIMIT	50
#define	CCHIDCOST	3

char	insmult[8]	= {INFINITY, INFINITY, INFINITY, 15, 8, 6, 3, 1};
char	repmult[7]	= {INFINITY, INFINITY, INFINITY, 8, 6, 3, 1};
char	delmult[6]	= {INFINITY, INFINITY, INFINITY, 6, 3, 1};

#define	NOCHAR	-1

#define	Eprintf	if (errtrace) printf
#define	Tprintf	if (testtrace) printf

/*
 * Action arrays of the parser needed here
 */
int	yyact[], yypact[], *yypv;

/*
 * Yytips is the tip of the stack when using
 * the function loccor to check for local
 * syntactic correctness. As we don't want
 * to copy the whole parser stack, but want
 * to simulate parser moves, we "split"
 * the parser stack and keep the tip here.
 */
#define	YYTIPSIZ 16
int	yytips[YYTIPSIZ], yytipct;
int	yytipv[YYTIPSIZ];

/*
 * The array YC saves the lookahead tokens for the
 * forward moves.
 * Yccnt is the number of tokens in the YC array.
 */
#define	YCSIZ	6

int	yCcnt;
struct	yytok YC0[YCSIZ + 1];
struct	yytok *YC;

/*
 * YCps gives the top of stack at
 * the point of error.
 */

bool	yyunique	1;

STATIC	unsigned yyTshifts;

/*
 * Cact is the corrective action we have decided on
 * so far, ccost its cost, and cchar the associated token.
 * Cflag tells if the correction is over the previous input token.
 */
int	cact, ccost, cchar, cflag;

/*
 * ACtok holds the token under
 * consideration when examining
 * the lookaheads in a state.
 */
struct	yytok ACtok;

#define acchar	ACtok.Yychar
#define aclval	ACtok.Yylval

/*
 * Make a correction to the current stack which has
 * top of stack pointer Ps.
 */
yyrecover(Ps0, idfail)
	int *Ps0, idfail;
{
	register int c, i;
	int yyrwant, yyrhave;

#ifdef PI
	Recovery = 1;
#endif

	YC = &YC0[1];
#ifdef DEBUG
	if (errtrace) {
		setpfx('p');
		yerror("Point of error");
		printf("States %d %d ...", Ps0[0], Ps0[-1]);
		if (idfail)
			printf(" [Idfail]");
		pchr('\n');
		printf("Input %s%s", tokname(&Y , 0)
				   , tokname(&Y , 1));
	}

#endif
	/*
	 * We first save the current input token
	 * and its associated semantic information.
	 */
	if (yychar < 0)
		yychar = yylex();
	copy(&YC[0], &Y, sizeof Y);

	/*
	 * Set the default action and cost
	 */
	cact = CPANIC, ccost = CLIMIT, cflag = 0;

	/*
	 * Peek ahead
	 */
	for (yCcnt = 1; yCcnt < YCSIZ; yCcnt++) {
		yychar = yylex();
		copy(&YC[yCcnt], &Y, sizeof YC[0]);
#ifdef DEBUG
		Eprintf(" | %s%s", tokname(&YC[yCcnt] , 0 )
				 , tokname(&YC[yCcnt] , 1 ));
#endif
	}
#ifdef DEBUG
	Eprintf("\n");
#endif

	/*
	 * If we are here because a reduction failed, try
	 * correcting that.
	 */
	if (idfail) {
		/*
		 * Save the particulars about
		 * the kind of identifier we want/have.
		 */
		yyrwant = yyidwant;
		yyrhave = yyidhave;
#ifdef DEBUG
		Tprintf("  Try Replace %s identifier with %s identifier cost=%d\n",
		    classes[yyidhave], classes[yyidwant], CCHIDCOST);
#endif

		/*
		 * Save the semantics of the ID on the
		 * stack, and null them out to free
		 * up the reduction in question.
		 */
		i = yypv[0];
		yypv[0] = nullsem(YID);
		c = correct(NOCHAR, 0, CCHIDCOST, &repmult[2], Ps0, yypv);
		yypv[0] = i;
#ifdef DEBUG
		if (c < CPRLIMIT || fulltrace)
			Eprintf("Cost %2d Replace %s identifier with %s identifier\n", c, classes[yyrhave], classes[yyrwant]);
#endif
		if (c < ccost)
			cact = CCHIDENT, ccost = c, cchar = YID;
	}

	/*
	 * First try correcting the state we are in
	 */
	trystate(Ps0, yypv, 0, &insmult[1], &delmult[1], &repmult[1]);

	/*
	 * Now, if we just shifted over a terminal, try
	 * correcting it.
	 */
	if (OY.Yychar != -1 && OY.Yylval != nullsem(OY.Yychar)) {
		YC--;
		copy(&YC[0], &OY, sizeof YC[0]);
		trystate(Ps0 - 1, yypv - 1, 1, insmult, delmult, repmult);
		if (cflag == 0)
			YC++;
		else {
			yypv--;
#ifdef PXP
			yypw--;
#endif
			Ps0--;
			yCcnt++;
		}
	}

	/*
	 * Restoring the first look ahead into
	 * the scanner token allows the error message
	 * routine to print the error message with the text
	 * of the correct line.
	 */
	copy(&Y, &YC[0], sizeof Y);

	/*
	 * Unique symbol insertion.
	 *
	 * If there was no reasonable correction found,
	 * but only one input to the parser is acceptable
	 * we report that, and try it.
	 *
	 * Special precautions here to prevent looping.
	 * The number of true inputs shifted over at the point
	 * of the last unique insertion is recorded in the
	 * variable yyTshifts.  If this is not less than
	 * the current number in yytshifts, we do not insert.
	 * Thus, after one unique insertion, no more unique
	 * insertions will be made until an input is shifted
	 * over.  This guarantees termination.
	 */
	if (cact == CPANIC && !idfail) {
		register int *ap;

		ap = &yyact[yypact[*Ps0 + 1]];
		if (*ap == -ERROR)
			ap =+ 2;
		if (ap[0] <= 0 && ap[2] > 0) {
			cchar = -ap[0];
			if (cchar == YEOF)
				yyexeof();
			if (cchar != ERROR && yyTshifts < yytshifts) {
				cact = CUNIQUE;
#ifdef DEBUG
				Eprintf("Unique symbol %s%s\n"
					, charname(cchar , 0 )
					, charname(cchar , 1 ));
#endif
				/*
				 * Note that the inserted symbol
				 * will not be counted as a true input
				 * (i.e. the "yytshifts--" below)
				 * so that a true shift will be needed
				 * to make yytshifts > yyTshifts.
				 */
				yyTshifts = yytshifts;
			}
		}
	}

	/*
	 * Set up to perform the correction.
	 * Build a token appropriate for replacement
	 * or insertion in the yytok structure ACchar
	 * having the attributes of the input at the
	 * point of error.
	 */
	copy(&ACtok, &YC[0], sizeof ACtok);
	acchar = cchar;
	aclval = nullsem(acchar);
	if (aclval != NIL)
		recovered();
	switch (cact) {
		/*
		 * Panic, just restore the
		 * lookahead and return.
		 */
		case CPANIC:
			setpfx('E');
			if (idfail) {
				copy(&Y, &OY, sizeof Y);
				if (yyrhave == NIL) {
#ifdef PI
					if (yybaduse(yypv[0], yyeline, ISUNDEF) == NIL)
#endif
						yerror("Undefined identifier");
				} else {
					yerror("Improper %s identifier", classes[yyrhave]);
#ifdef PI
					yybaduse(yypv[0], yyeline, NIL);
#endif
				}
				/*
				 * Suppress message from panic routine
				 */
				yyshifts = 1;
			}
			i = 0;
			/* Note that on one path we dont touch yyshifts ! */
			break;
		/*
		 * Delete the input.
		 * Mark this as a shift over true input.
		 * Restore the lookahead starting at
		 * the second token.
		 */
		case CDELETE:
			if (ccost != 0)
				yerror("Deleted %s%s", tokname(&YC[0] , 0 )
						     , tokname(&YC[0] , 1 ));
			yytshifts++;
			i = 1;
			yyshifts = 0;
			break;
		/*
		 * Replace the input with a new token.
		 */
		case CREPLACE:
			if (acchar == YEOF)
				yyexeof();
			if (acchar == YEND)
				aclval = NIL;
			yerror("Replaced %s%s with a %s%s",
			    tokname(&YC[0] , 0 ),
			    tokname(&YC[0] , 1 ),
			    tokname(&ACtok , 0 ),
			    tokname(&ACtok , 1 ));
			copy(&YC[0], &ACtok, sizeof YC[0]);
			i = 0;
			yyshifts = 0;
			break;
		/*
		 * Insert a token.
		 * Don't count this token as a true input shift.
		 * For inserted "end"s pas.y is responsible
		 * for the error message later so suppress it.
		 * Restore all the lookahead.
		 */
		case CINSERT:
			if (acchar == YEOF)
				yyexeof();
			if (acchar != YEND)
				yerror("Inserted %s%s",
					tokname(&ACtok , 0 ),
					tokname(&ACtok , 1 ));
			yytshifts--;
			i = 0;
			yyshifts = 0;
			break;
		/*
		 * Make a unique symbol correction.
		 * Like an insertion but a different message.
		 */
		case CUNIQUE:
			setpfx('E');
			yerror("Expected %s%s",
				tokname(&ACtok , 0 ),
				tokname(&ACtok , 1 ));
			yytshifts--;
			i = 0;
			if (ccost == 0 || yyunique)
				yyshifts = 0;
			else
				yyshifts = -1;
			break;
		/*
		 * Change an identifier's type
		 * to make it work.
		 */
		case CCHIDENT:
			copy(&Y, &OY, sizeof Y);
#ifdef PI
			i = 1 << yyrwant;
#endif
			if (yyrhave == NIL) {
				yerror("Undefined %s", classes[yyrwant]);
#ifdef PI
				i =| ISUNDEF;
#endif
			} else
				yerror("Replaced %s id with a %s id", classes[yyrhave], classes[yyrwant]);
#ifdef PI
			yybaduse(yypv[0], yyeline, i);
#endif
			yypv[0] = nullsem(YID);
			i = 0;
			yyshifts = 0;
			break;
	}

	/*
	 * Restore the desired portion of the lookahead,
	 * and possibly the inserted or unique inserted token.
	 */
	for (yCcnt--; yCcnt >= i; yCcnt--)
		unyylex(&YC[yCcnt]);
	if (cact == CINSERT || cact == CUNIQUE)
		unyylex(&ACtok);

	/*
	 * Put the scanner back in sync.
	 */
	yychar = yylex();

	/*
	 * We succeeded if we didn't "panic".
	 */
	Recovery = 0;
	Ps = Ps0;
	return (cact != CPANIC);
}

yyexeof()
{

	yerror("End-of-file expected - QUIT");
	pexit(ERRS);
}

yyunexeof()
{

	yerror("Unexpected end-of-file - QUIT");
	pexit(ERRS);
}

/*
 * Try corrections with the state at Ps0.
 * Flag is 0 if this is the top of stack state,
 * 1 if it is the state below.
 */
trystate(Ps0, Pv0, flag, insmult, delmult, repmult)
	int *Ps0, *Pv0, flag;
	char *insmult, *delmult, *repmult;
{
	/*
	 * C is a working cost, ap a pointer into the action
	 * table for looking at feasible alternatives.
	 */
	register int c, *ap;
	int i, *actions;

#ifdef DEBUG
	Eprintf("Trying state %d\n", *Ps0);
#endif
	/*
	 * Try deletion.
	 * Correct returns a cost.
	 */
#ifdef DEBUG
	Tprintf("  Try Delete %s%s cost=%d\n",
		tokname(&YC[0] , 0 ),
		tokname(&YC[0] , 1 ),
		delcost(YC[0].Yychar));
#endif
	c = delcost(YC[0].Yychar);
#ifndef DEBUG
	if (c < ccost) {
#endif
		c = correct(NOCHAR, 1, c, delmult, Ps0, Pv0);
#ifdef DEBUG
		if (c < CPRLIMIT || fulltrace)
			Eprintf("Cost %2d Delete %s%s\n", c,
				tokname(&YC[0] , 0 ),
				tokname(&YC[0] , 1 ));
#endif
		if (c < ccost)
			cact = CDELETE, ccost = c, cflag = flag;
#ifndef DEBUG
	}
#endif

	/*
	 * Look at the inputs to this state
	 * which will cause parse action shift.
	 */
	aclval = NIL;
	ap = &yyact[yypact[*Ps0 + 1]];

	/*
	 * Skip action on error to
	 * detect true unique inputs.
	 * Error action is always first.
	 */
	if (*ap == -ERROR) 
		ap=+ 2;

	/*
	 * Loop through the test actions
	 * for this state.
	 */
	for (actions = ap; *ap <= 0; ap =+ 2) {
		/*
		 * Extract the token of this action
		 */
		acchar = -*ap;

		/*
		 * Try insertion
		 */
#ifdef DEBUG
		Tprintf("  Try Insert %s%s cost=%d\n"
			, charname(acchar , 0 )
			, charname(acchar , 1 )
			, inscost(acchar));
#endif
		c = inscost(acchar, YC[0].Yychar);
#ifndef DEBUG
		if (c < ccost) {
#endif
			if (c == 0) {
				c = correct(acchar, 0, 1, insmult + 1, Ps0, Pv0);
#ifdef DEBUG
				Eprintf("Cost %2d Freebie %s%s\n", c
					, charname(acchar , 0 )
					, charname(acchar , 1 ));
#endif
				if (c < ccost)
					cact = CUNIQUE, ccost = 0, cchar = acchar, cflag = flag;
			} else {
				c = correct(acchar, 0, c, insmult, Ps0, Pv0);
#ifdef DEBUG
				if (c < CPRLIMIT || fulltrace)
					Eprintf("Cost %2d Insert %s%s\n", c
						, charname(acchar , 0 )
						, charname(acchar , 1 ));
#endif
				if (c < ccost)
					cact = CINSERT, ccost = c, cchar = acchar, cflag = flag;
			}
#ifndef DEBUG
		}
#endif

		/*
		 * Try replacement
		 */
#ifdef DEBUG
		Tprintf("  Try Replace %s%s with %s%s cost=%d\n",
		    tokname(&YC[0] , 0 ),
		    tokname(&YC[0] , 1 ),
		    charname(acchar , 0 ),
		    charname(acchar , 1 ),
		    repcost(YC[0].Yychar, acchar));
#endif
		c = repcost(YC[0].Yychar, acchar);
#ifndef DEBUG
		if (c < ccost) {
#endif
			c = correct(acchar, 1, repcost(YC[0].Yychar, acchar), repmult, Ps0, Pv0);
#ifdef DEBUG
			if (c < CPRLIMIT || fulltrace)
				Eprintf("Cost %2d Replace %s%s with %s%s\n",
					c,
					tokname(&YC[0] , 0 ),
					tokname(&YC[0] , 1 ),
					tokname(&ACtok , 0 ),
					tokname(&ACtok , 1 ));
#endif
			if (c < ccost)
				cact = CREPLACE, ccost = c, cchar = acchar, cflag = flag;
#ifndef DEBUG
		}
#endif
	}
}

int	*yCpv;
char	yyredfail;

/*
 * The ntok structure is used to build a
 * scanner structure for tokens inserted
 * from the argument "fchar" to "correct" below.
 */
static	struct yytok ntok;

/*
 * Compute the cost of a correction
 * C is the base cost for it.
 * Fchar is the first input character from
 * the current state, NOCHAR if none.
 * The rest of the inputs come from the array
 * YC, starting at origin and continuing to the
 * last character there, YC[yCcnt - 1].Yychar.
 *
 * The cost returned is INFINITE if this correction
 * allows no shifts, otherwise is weighted based
 * on the number of shifts this allows against the
 * maximum number possible with the available lookahead.
 */
correct(fchar, origin, c, multvec, Ps0, Pv0)
	register int fchar, c;
	int origin;
	char *multvec;
	int *Ps0, *Pv0;
{
	register char *mv;

	/*
	 * Ps is the top of the parse stack after the most
	 * recent local correctness check.  Loccor returns
	 * NIL when we cannot shift.
	 */
	register int *ps;

	yyredfail = 0;
	/*
	 * Initialize the tip parse and semantic stacks.
	 */
	ps = Ps0;
	yytips[0] = *ps;
	ps--;
	yytipv[0] = Pv0[0];
	yCpv = Pv0 - 1;
	yytipct = 1;

	/*
	 * Shift while possible.
	 * Adjust cost as necessary.
	 */
	mv = multvec;
	do {
		if (fchar != NOCHAR) {
			copy(&ntok, &YC[0], sizeof ntok);
			ntok.Yychar = fchar, ntok.Yylval = nullsem(fchar);
			fchar = NOCHAR;
			ps = loccor(ps, &ntok);
		} else
			ps = loccor(ps, &YC[origin++]);
		if (ps == NIL) {
			if (yyredfail && mv > multvec)
				mv--;
			c =* *mv;
			break;
		}
		mv++;
	} while (*mv != 1);
	return (c);
}

extern	int yygo[], yypgo[], yyr1[], yyr2[];
/*
 * Local syntactic correctness check.
 * The arguments to this routine are a
 * top of stack pointer, ps, and an input
 * token tok.  Also, implicitly, the contents
 * of the yytips array which contains the tip
 * of the stack, and into which the new top
 * state on the stack will be placed if we shift.
 *
 * If we succeed, we return a new top of stack
 * pointer, else we return NIL.
 */
loccor(ps, ntok)
	int *ps;
	struct yytok *ntok;
{
	register int *p, n;
	register int nchar;
	int i;

	if (ps == NIL)
		return (NIL);
	nchar = ntok->Yychar;
	yyeline = ntok->Yyeline;
#ifdef DEBUG
	Tprintf("    Stack ");
	for (i = yytipct - 1; i >= 0; i--)
		Tprintf("%d ", yytips[i]);
	Tprintf("| %d, Input %s%s\n", *ps
		, charname(nchar , 0 )
		, charname(nchar , 1 ));
#endif
	/*
	 * As in the yacc parser yyparse,
	 * p traces through the action list
	 * and "n" is the information associated
	 * with the action.
	 */
newstate:
	p = &yyact[ yypact[yytips[yytipct - 1]+1] ];

actn:
	/*
	 * Search the parse actions table
	 * for something useful to do.
	 * While n is non-positive, it is the
	 * arithmetic inverse of the token to be tested.
	 * This allows a fast check.
	 */
	while ((n = *p++) <= 0)
		if ((n =+ nchar) != 0)
			p++;
	switch (n >> 12) {
		/*
		 * SHIFT
		 */
		case 2:
			n =& 07777;
			yyredfail = 0;
			if (nchar == YID)
				yyredfail++;
			if (yytipct == YYTIPSIZ) {
tipover:
#ifdef DEBUG
				Tprintf("\tTIP OVFLO\n");
#endif
				return (NIL);
			}
			yytips[yytipct] = n;
			yytipv[yytipct] = ntok->Yylval;
			yytipct++;
#ifdef DEBUG
			Tprintf("\tShift to state %d\n", n);
#endif
			return (ps);
		/*
		 * REDUCE
		 */
		case 3:
			n =& 07777;
			if (yyEactr(n, yytipv[yytipct - 1]) == 0) {
#ifdef DEBUG
				Tprintf("\tYyEactr objects: have %s id, want %s id\n", classes[yyidhave], classes[yyidwant]);
#endif
				return (NIL);
			}
			yyredfail = 0;
			i = yyr2[n];
#ifdef DEBUG
			Tprintf("\tReduce, length %d,", i);
#endif
			if (i > yytipct) {
				i =- yytipct;
				yytipct = 0;
				ps =- i;
				yCpv =- i;
			} else
				yytipct =- i;
			if (yytipct >= YYTIPSIZ)
				goto tipover;
			/*
			 * Use goto table to find next state
			 */
			p = &yygo[yypgo[yyr1[n]]];
			i = yytipct ? yytips[yytipct - 1] : *ps;
			while (*p != i && *p >= 0)
				p =+ 2;
#ifdef DEBUG
			Tprintf(" new state %d\n", p[1]);
#endif
			yytips[yytipct] = p[1];
			yytipct++;
			goto newstate;
		/*
		 * ACCEPT
		 */
		case 4:
#ifdef DEBUG
			Tprintf("\tAccept\n");
#endif
			return (ps);
		/*
		 * ERROR
		 */
		case 1:
#ifdef DEBUG
			Tprintf("\tError\n");
#endif
			return (0);
	}
	panic("loccor");
}
