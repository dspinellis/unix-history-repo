# define	PLAY_Y		15	/* size of player's hand window */
# define	PLAY_X		12
# define	TABLE_Y		21	/* size of table window */
# define	TABLE_X		14
# define	COMP_Y		15	/* size of computer's hand window */
# define	COMP_X		12
# define	SCORE_Y	 	0	/* starting position of scoring board */
# define	SCORE_X		0
# define	SCORE_SZ	41	/* X size of score board */
# define	CRIB_Y		17	/* position of crib (cut card) */
# define	CRIB_X		(PLAY_X + TABLE_X + SCORE_SZ)

# define	PEG	'*'	/* what a peg looks like on the board */

extern	WINDOW		*Playwin;		/* player's hand window */
extern	WINDOW		*Tablewin;		/* table window */
extern	WINDOW		*Compwin;		/* computer's hand window */
