#include <stdio.h>
#include <ctype.h>
#ifndef	lint
static char ident[] = "@(#)$Id: lexstring.c,v 1.5 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#define YYLERR yysvec
#define YYTYPE int
#define YYLMAX 256

struct yysvf { 
#ifndef hpux
	struct yywork *yystoff;
#else	/* hpux */
	int yystoff;
#endif	/* hpux */
	struct yysvf *yyother;
	int *yystops;
};

struct yywork { 
	YYTYPE	verify;
	YYTYPE	advance; 
}; 

extern int yyvstop[];
extern struct yywork yycrank[];
extern struct yysvf yysvec[];
extern char yymatch[];
extern char yyextra[];

#ifdef LEXDEBUG
static int debug = 0;
#endif	/* LEXDEBUG */

#ifdef ONECASE
static char case_map[] = {
     0,   1,   2,   3,   4,   5,   6,   7,   8,   9,
    10,  11,  12,  13,  14,  15,  16,  17,  18,  19,
    20,  21,  22,  23,  24,  25,  26,  27,  28,  29,
    30,  31,  32,  33,  34,  35,  36,  37,  38,  39,
    40,  41,  42,  43,  44,  45,  46,  47,  48,  49,
    50,  51,  52,  53,  54,  55,  56,  57,  58,  59,
    60,  61,  62,  63,  64,  97,  98,  99, 100, 101,
   102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
   112, 113, 114, 115, 116, 117, 118, 119, 120, 121,
   122,  91,  92,  93,  94,  95,  96,  97,  98,  99,
   100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
   110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
   120, 121, 122, 123, 124, 125, 126, 127,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0
};
#endif	/* ONECASE */

lex_string( strptr, start_cond)
	char	**strptr;
	int	start_cond;
{
	register struct yysvf *state, **lsp;
	register struct yywork *tran;
	register int statenum;
	register int ch;
	register char	*cp = *strptr;
	register int	*found;
	struct	yysvf *yylstate[YYLMAX];

	/* start off machines */
	lsp = yylstate;
	statenum = 1 + start_cond;
	state = yysvec + statenum;
	for (;;){
# ifdef LEXDEBUG
		if (debug) {
			fprintf(stderr,"%d ",statenum - 1);
		}
# endif
#ifndef hpux
		tran = state->yystoff;
#else	/* hpux */
		tran = &yycrank[state->yystoff];
#endif	/* hpux */
		if(tran == yycrank)
			/* may not be any transitions */
			if (state->yyother == 0 ||
#ifndef hpux
			    state->yyother->yystoff == yycrank)
#else	/* hpux */
			    state->yyother->yystoff == 0)
#endif	/* hpux */
				break;

#ifdef ONECASE
		ch = case_map[*cp++];
#else	/*  not ONECASE */
		ch = *cp++;
#endif	/* ONECASE */
# ifdef LEXDEBUG
		if (debug) {
			fprintf(stderr,"(");
			allprint(ch);
			fprintf(stderr, ")");
		}
# endif
tryagain:
#ifndef hpux
		if ( tran > yycrank){
#else	/* hpux */
		if ( (int)tran > (int)yycrank){
#endif	/* hpux */
			tran += ch;
			if (tran->verify == statenum){
				if ((statenum = tran->advance) == 0){
					/* error transitions */
					--cp;
					break;
				}
				state = statenum + yysvec;
				*lsp++ = state;
				goto contin;
			}

#ifndef hpux
		} else if(tran < yycrank) {
#else	/* hpux */
		} else if( (int)tran < (int)yycrank) {
#endif	/* hpux */
			tran = yycrank+(yycrank-tran) + ch;
# ifdef LEXDEBUG
			if (debug) {
				fprintf(stderr," compressed");
			}
# endif
			if (tran->verify == statenum){
				if ((statenum = tran->advance) == 0)
					/* error transitions */
					break;

				state = statenum + yysvec;
				*lsp++ = state;
				goto contin;
			}
			tran += (yymatch[ch] - ch);
# ifdef LEXDEBUG
			if (debug) {
				fprintf(stderr,"(fb ");
				allprint(yymatch[ch]);
				fprintf(stderr,")");
			}
# endif
			if (tran->verify == statenum){
				if((statenum = tran->advance) == 0)
					/* error transition */
					break;

				state = statenum + yysvec;
				*lsp++ = state;
				goto contin;
			}
		}
		if ((state = state->yyother) &&
#ifndef hpux
		    (tran = state->yystoff) != yycrank){
#else	/* hpux */
		    (tran = &yycrank[state->yystoff]) != yycrank){
#endif	/* hpux */
			statenum = state - yysvec;
# ifdef LEXDEBUG
			if (debug) {
				fprintf(stderr,"fb %d", statenum - 1);
			}
# endif
			goto tryagain;
		} else
			break;

contin:
# ifdef LEXDEBUG
		if (debug) {
			fprintf(stderr,">");
		}
# endif
		;
	}
# ifdef LEXDEBUG
	if (debug) {
		fprintf(stderr,"\nStopped in state %d (",*(lsp-1)-yysvec-1);
		allprint(ch);
		fprintf(stderr, ") ");
	}
# endif
	while (lsp-- > yylstate){
		if (*lsp != 0 && (found= (*lsp)->yystops) && *found > 0){
			if(yyextra[*found]){
				/* must backup */
				ch = -*found;
				do {
					while (*found && *found++ != ch)
						;
				 } while (lsp > yylstate &&
					  (found = (*--lsp)->yystops));
			}
# ifdef LEXDEBUG
			if (debug) {
				fprintf(stderr," Match \"");
				for ( cp = *strptr;
				      cp <= ((*strptr)+(lsp-yylstate));
				      cp++)
					allprint( *cp );
				fprintf(stderr,"\" action %d\n",*found);
			}
# endif
			*strptr += (lsp - yylstate + 1);
			return(*found);
		}
	}
	/* the string didn't match anything - if we're looking at
	 * eos, just return 0.  Otherwise, bump the string pointer
	 * and return -1.
	 */
# ifdef LEXDEBUG
	if (debug) {
		fprintf(stderr," No match\n");
	}
#endif	/* LEXDEBUG */
	if ( **strptr ) {
		(*strptr)++;
		return (-1);
	}
	return (0);
}

#ifdef LEXDEBUG
allprint(c)
	char c;
{
	if ( c < 32 ) {
	    putc( '^', stderr );
	    c += 32;
	} else if ( c == 127 ) {
	    putc( '^', stderr );
	    c = '?';
	}
	putc( c, stderr );
}
#endif	/* LEXDEBUG */
