/* $Header: search.h,v 4.3 85/05/01 11:50:46 lwall Exp $
 *
 * $Log:	search.h,v $
 * Revision 4.3  85/05/01  11:50:46  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#ifndef NBRA
#define	NBRA	10		/* the maximum number of meta-brackets in an
				   RE -- \( \) */
#define NALTS	10		/* the maximum number of \|'s */
 
typedef struct {	
    char *expbuf;		/* The compiled search string */
    int eblen;			/* Length of above buffer */
    char *alternatives[NALTS];	/* The list of \| seperated alternatives */
    char *braslist[NBRA];	/* RE meta-bracket start list */
    char *braelist[NBRA];	/* RE meta-bracket end list */
    char *brastr;		/* saved match string after execute() */
    char nbra;			/* The number of meta-brackets int the most
				   recenlty compiled RE */
    bool do_folding;		/* fold upper and lower case? */
} COMPEX;

void	search_init();
void	init_compex();
void	free_compex();
char	*getbracket();
void	case_fold();
char	*compile(); 
void	grow_eb();
char	*execute(); 
bool	advance();
bool	backref(); 
bool	cclass(); 
#endif
