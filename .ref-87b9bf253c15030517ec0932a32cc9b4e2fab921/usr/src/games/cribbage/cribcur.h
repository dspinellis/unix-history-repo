# define	PLAY_Y	15	/* starting pos for player's hand window */
# define	PLAY_X	12
# define	TABLE_Y	21	/* starting pos for table window */
# define	TABLE_X	14
# define	COMP_Y	15	/* starting pos for computer's hand window */
# define	COMP_X	12
# define	SCORE_Y	 0	/* starting pos for scoring board */
# define	SCORE_X	39
# define	CRIB_Y	17	/* position of crib (cut card) */
# define	CRIB_X	(PLAY_X + TABLE_X)

# define	PEG	'*'	/* what a peg looks like on the board */

extern	WINDOW		*Playwin;		/* player's hand window */
extern	WINDOW		*Tablewin;		/* table window */
extern	WINDOW		*Compwin;		/* computer's hand window */
