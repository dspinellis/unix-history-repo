#include <stdio.h>
#include <ctype.h>

#define YYLERR yysvec
#define YYTYPE int
#define YYLMAX 256

struct yysvf { 
	struct yywork *yystoff;
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
extern struct yywork *yytop;
extern char yymatch[];
extern char yyextra[];

#ifdef LEXDEBUG
static int debug = 0;
#endif LEXDEBUG

lex_string( strptr, start_cond)
	char	**strptr;
	int	start_cond;
{
	register struct yysvf *state, **lsp;
	register struct yywork *tran;
	register int ch;
	register char	*cp = *strptr;
	register int	*found;
	struct	yysvf *yylstate[YYLMAX];

	/* start off machines */
	lsp = yylstate;
	state = yysvec+1+start_cond;
	for (;;){
# ifdef LEXDEBUG
		if(debug)
			fprintf(stderr,"state %d\n",state-yysvec-1);
# endif
		tran = state->yystoff;
		if(tran == yycrank)
			/* may not be any transitions */
			if (state->yyother == 0 ||
			    state->yyother->yystoff == yycrank)
				break;

		ch = *cp++;
#ifdef ONECASE
		if (isupper(ch) )
			ch = tolower(ch);
#endif ONECASE
tryagain:
# ifdef LEXDEBUG
		if(debug){
			fprintf(stderr,"char ");
			allprint(ch);
			putchar('\n');
		}
# endif
		if ( tran > yycrank){
			tran += ch;
			if (tran <= yytop && tran->verify+yysvec == state){
				if ((state = tran->advance+yysvec) == YYLERR){
					/* error transitions */
					--cp;
					break;
				}
				*lsp++ = state;
				goto contin;
			}

		} else if(tran < yycrank) {
			/* r < yycrank */
			tran = yycrank+(yycrank-tran) + ch;
# ifdef LEXDEBUG
			if (debug)
				fprintf(stderr,"compressed state\n");
# endif
			if(tran <= yytop && tran->verify+yysvec == state){
				if ((state = tran->advance+yysvec) == YYLERR)
					/* error transitions */
					break;

				*lsp++ = state;
				goto contin;
			}
			tran += (yymatch[ch] - ch);
# ifdef LEXDEBUG
			if(debug){
				fprintf(stderr,"try fall back character ");
				allprint(yymatch[ch]);
				putchar('\n');
			}
# endif
			if(tran <= yytop && tran->verify+yysvec == state){
				if(tran->advance+yysvec == YYLERR)
					/* error transition */
					break;

				*lsp++ = state = tran->advance+yysvec;
				goto contin;
			}
		}
		if ((state = state->yyother) &&
		    (tran = state->yystoff) != yycrank){
# ifdef LEXDEBUG
			if(debug)
				fprintf(stderr,"fall back to state %d\n",
					state-yysvec-1);
# endif
			goto tryagain;
		} else
			break;

contin:
# ifdef LEXDEBUG
		if(debug){
			fprintf(stderr,"state %d char ",state-yysvec-1);
			allprint(ch);
			putchar('\n');
		}
# endif
		;
	}
# ifdef LEXDEBUG
	if(debug){
		fprintf(stderr,"stopped at %d with ",*(lsp-1)-yysvec-1);
		allprint(ch);
		putchar('\n');
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
			if(debug){
				fprintf(stderr,"\nmatch ");
				for ( cp = *strptr;
				      cp <= ((*strptr)+(lsp-yylstate));
				      cp++)
					allprint( *cp );
				fprintf(stderr," action %d\n",*found);
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
	if(debug)
		fprintf(stderr,"\nno match\n");
#endif LEXDEBUG
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
#endif LEXDEBUG
