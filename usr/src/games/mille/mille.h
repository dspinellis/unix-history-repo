# include	<ctype.h>
# include	"curses.h"

/*
 * @(#)mille.h	1.1 (Berkeley) 4/1/82
 */

/*
 * Miscellaneous constants
 */

# define	unsgn		unsigned
# define	CARD		short

# define	ARNOLD		214	/* my uid			*/

# define	GURP		28672	/* bad uid			*/
# define	MAXUSERS	35	/* max # of users for startup	*/
# define	HAND_SZ		7	/* number of cards in a hand	*/
# define	DECK_SZ		101	/* number of cards in decks	*/
# define	NUM_SAFE	4	/* number of saftey cards	*/
# define	NUM_MILES	5	/* number of milestones types	*/
# define	NUM_CARDS	20	/* number of types of cards	*/
# define	BOARD_Y		17	/* size of board screen		*/
# define	BOARD_X		40
# define	MILES_Y		7	/* size of mileage screen	*/
# define	MILES_X		80
# define	SCORE_Y		17	/* size of score screen		*/
# define	SCORE_X		40
# define	MOVE_Y		10	/* Where to print move prompt	*/
# define	MOVE_X		20
# define	ERR_Y		15	/* Where to print errors	*/
# define	ERR_X		5
# define	EXT_Y		4	/* Where to put Extension	*/
# define	EXT_X		9

# define	PLAYER		0
# define	COMP		1

# define	W_SMALL		0	/* Small (initial) window	*/
# define	W_FULL		1	/* Full (final) window		*/

/*
 * Move types
 */

# define	M_DISCARD	0
# define	M_DRAW		1
# define	M_PLAY		2
# define	M_ORDER		3

/*
 * Scores
 */

# define	SC_SAFETY	100
# define	SC_ALL_SAFE	300
# define	SC_COUP		300
# define	SC_TRIP		400
# define	SC_SAFE		300
# define	SC_DELAY	300
# define	SC_EXTENSION	200
# define	SC_SHUT_OUT	500

/*
 * safety descriptions
 */

# define	S_UNKNOWN	0	/* location of safety unknown	*/
# define	S_IN_HAND	1	/* safety in player's hand	*/
# define	S_PLAYED	2	/* safety has been played	*/
# define	S_GAS_SAFE	0	/* Gas safety card index	*/
# define	S_SPARE_SAFE	1	/* Tire safety card index	*/
# define	S_DRIVE_SAFE	2	/* Driveing safety card index	*/
# define	S_RIGHT_WAY	3	/* Right-of-Way card index	*/
# define	S_CONV		15	/* conversion from C_ to S_	*/

/*
 * card numbers
 */

# define	C_INIT		-1
# define	C_25		0
# define	C_50		1
# define	C_75		2
# define	C_100		3
# define	C_200		4
# define	C_EMPTY		5
# define	C_FLAT		6	
# define	C_CRASH		7
# define	C_STOP		8
# define	C_LIMIT		9
# define	C_GAS		10
# define	C_SPARE		11
# define	C_REPAIRS	12
# define	C_GO		13
# define	C_END_LIMIT	14
# define	C_GAS_SAFE	15
# define	C_SPARE_SAFE	16
# define	C_DRIVE_SAFE	17
# define	C_RIGHT_WAY	18

/*
 * prompt types
 */

# define	MOVEPROMPT		0
# define	REALLYPROMPT		1
# define	ANOTHERHANDPROMPT	2
# define	ANOTHERGAMEPROMPT	3
# define	SAVEGAMEPROMPT		4
# define	SAMEFILEPROMPT		5
# define	FILEPROMPT		6
# define	EXTENSIONPROMPT		7
# define	OVERWRITEFILEPROMPT	8

# ifdef	SYSV
# define	srandom(x)	srand(x)
# define	random()	rand()

# ifndef	attron
#	define	erasechar()	_tty.c_cc[VERASE]
#	define	killchar()	_tty.c_cc[VKILL]
# endif
# else
# ifndef	erasechar
#	define	erasechar()	_tty.sg_erase
#	define	killchar()	_tty.sg_kill
# endif
# endif	SYSV

typedef struct {
	bool	coups[NUM_SAFE];
	bool	can_go;
	bool	new_battle;
	bool	new_speed;
	short	safety[NUM_SAFE];
	short	sh_safety[NUM_SAFE];
	short	nummiles[NUM_MILES];
	short	sh_nummiles[NUM_MILES];
	CARD	hand[HAND_SZ];
	CARD	sh_hand[HAND_SZ];
	CARD	battle;
	CARD	sh_battle;
	CARD	speed;
	CARD	sh_speed;
	int	mileage;
	int	sh_mileage;
	int	hand_tot;
	int	sh_hand_tot;
	int	safescore;
	int	sh_safescore;
	int	coupscore;
	int	total;
	int	sh_total;
	int	games;
	int	sh_games;
	int	was_finished;
} PLAY;

/*
 * macros
 */

# define	other(x)	(1 - x)
# define	nextplay()	(Play = other(Play))
# define	nextwin(x)	(1 - x)
# define	opposite(x)	(Opposite[x])
# define	issafety(x)	(x >= C_GAS_SAFE)

/*
 * externals
 */

extern bool	Debug, Finished, Next, On_exit, Order, Saved;

extern char	*C_fmt, **C_name, *Fromfile, Initstr[];

extern int	Card_no, End, Handstart, Movetype, Numcards[], Numgos,
		Numneed[], Numseen[NUM_CARDS], Play, Value[], Window;

extern CARD	Deck[DECK_SZ], Discard, Opposite[NUM_CARDS], Sh_discard,
		*Topcard;

extern FILE	*outf;

extern PLAY	Player[2];

extern WINDOW	*Board, *Miles, *Score;

/*
 * functions
 */

CARD	getcard();
