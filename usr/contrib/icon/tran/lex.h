/*
 * Token table structure.
 */

struct toktab {
   char *t_word;		/* token */
   int  t_type;			/* token type returned by yylex */
   int  t_flags;		/* flags for semicolon insertion */
   };

extern struct toktab toktab[];	/* token table */
extern struct toktab *restab[];	/* reserved word index */

#define T_IDENT		&toktab[0]
#define T_INT		&toktab[1]
#define T_REAL		&toktab[2]
#define T_STRING	&toktab[3]
#define T_CSET		&toktab[4]
#define T_EOF		&toktab[5]

/*
 * t_flags values for token table.
 */

#define BEGINNER 1		/* token can follow a semicolon */
#define ENDER    2		/* token can precede a semicolon */

/*
 * Operator table - a finite-state automaton for recognizing Icon operators.
 */

struct optab {
   char o_input;
   char o_action;
   int o_val;
   };

extern struct optab state0[];
extern struct optab state1[];
extern struct optab state2[];
extern struct optab state3[];
extern struct optab state4[];
extern struct optab state5[];
extern struct optab state6[];
extern struct optab state7[];
extern struct optab state8[];
extern struct optab state9[];
extern struct optab state10[];
extern struct optab state11[];
extern struct optab state12[];
extern struct optab state13[];
extern struct optab state14[];
extern struct optab state15[];
extern struct optab state16[];
extern struct optab state17[];
extern struct optab state18[];
extern struct optab state19[];
extern struct optab state20[];
extern struct optab state21[];
extern struct optab state22[];
extern struct optab state23[];
extern struct optab state24[];
extern struct optab state25[];
extern struct optab state26[];
extern struct optab state27[];
extern struct optab state28[];
extern struct optab state29[];
extern struct optab state30[];
extern struct optab state31[];
extern struct optab state32[];
extern struct optab state33[];
extern struct optab state34[];
extern struct optab state35[];
extern struct optab state36[];
extern struct optab state37[];
extern struct optab state38[];
extern struct optab state39[];
extern struct optab state40[];
extern struct optab state41[];
extern struct optab state42[];
extern struct optab state43[];
extern struct optab state44[];
extern struct optab state45[];
extern struct optab state46[];
extern struct optab state47[];
extern struct optab state48[];
extern struct optab state49[];
extern struct optab state50[];
extern struct optab state51[];
extern struct optab state52[];
extern struct optab state53[];
extern struct optab state54[];
extern struct optab state55[];
extern struct optab state56[];
extern struct optab state57[];
extern struct optab state58[];
extern struct optab state59[];
extern struct optab state60[];
extern struct optab state61[];

/*
 * o_action values.
 */

#define A_GOTO   1	/* shift input and goto new state o_val */
#define A_ERROR  2	/* illegal operator */
#define A_RETURN 3	/* return o_val, save input char */
#define A_IMMRET 4	/* return o_val, discard input char */

/*
 * Miscellaneous.
 */

#define isoctal(c) ((c)>='0'&&(c)<='7')	/* macro to test for octal digit */
#define NEXTCHAR   nextchar()		/* macro to get next character */
#define PUSHCHAR(c) peekc=(c)		/* macro to push back a character */

#define COMMENT '#'			/* comment beginner */
#define ESCAPE  '\\'			/* string literal escape character */
