# include	<curses.h>
# include	"deck.h"
# include	"cribbage.h"

bool		iwon		= FALSE;	/* if comp won last game */
bool		explain		= FALSE;	/* player mistakes explained */
bool		rflag		= FALSE;	/* if all cuts random */
bool		quiet		= FALSE;	/* if suppress random mess */
bool		Hasread		= TRUE;		/* if has read msg */

char		expl[128];			/* explanation */

int		knownum		= 0;		/* number of cards we know */
int		pscore		= 0;		/* player score in this game */
int		cscore		= 0;		/* comp score in this game */
int		pgames		= 0;		/* number games player won */
int		cgames		= 0;		/* number games comp won */
int		gamecount	= 0;		/* number games played */
int		glimit		= LGAME;	/* game playe to glimit */

CARD		deck[CARDS];			/* a deck */
CARD		phand[FULLHAND];		/* player's hand */
CARD		chand[FULLHAND];		/* computer's hand */
CARD		crib[CINHAND];			/* the crib */
CARD		turnover;			/* the starter */
CARD		known[CARDS];			/* cards we have seen */

WINDOW		*Playwin;			/* player's hand window */
WINDOW		*Tablewin;			/* table window */
WINDOW		*Compwin;			/* computer's hand window */
