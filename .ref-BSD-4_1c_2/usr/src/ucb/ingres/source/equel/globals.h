/*
**  GLOBALS.H -- Equel declarations
**
**	Compilation Flags:
**		xDEBUG -- for Chardebug, to provide compatability
**			with older Equels' "-c" flag, and for 
**			Lex_debug for the "-v" flag.
**
**	Version:
**		@(#)globals.h	7.1	2/5/81
*/



/*
**	Structure declarations
*/


	/* parser keyword and operator table node */

struct optab
{
	char	*op_term;	/* pointer to terminal */
	int  	op_token;	/* lexical token */
	int  	op_code;	/* code to distinguish different op_terms 
				 * with the same .op_token
				 */
};


	/* C variable tree node */

struct cvar
{
	char		*c_id;		/* identifier */
	char		c_type;		/* type */
	char		c_indir;	/* indirection level (- 1 for strings) */
	struct cvar	*c_left;	/* < sub-tree */
	struct cvar	*c_right;	/* > sub-tree */
};

	/* Display structures (linked list of strings) */

struct disp_node
{
	char			*d_elm;		/* display element */
	struct disp_node	*d_next;	/* next node */
	int			d_line;	/* for Symsp nodes, line
					 * where lexeme was seen
					 */
};

struct display
{
	struct disp_node	*disp_first;	/* first node in display */
	struct disp_node	*disp_last;	/* last node in display */
};

	/* Retrieval list */

struct ret_var
{
	char		*r_elm;		/* string invoking variable
					 * e.g. "*intpa [2]"
					 */
	struct ret_var	*r_next;	/* next variable used in "tupret" */
	char		r_type;		/* type of variable */
};

struct ret_list
{
	struct ret_var	*ret_first;	/* first node in ret_var chain */
	struct ret_var	*ret_last;	/* last node in chain */
};


	/* "# include" file processing stack (list) */

struct inc_file
{
	int		inc_yyline;	/* old file's yyline */
	int		inc_lineout;	/*  "    "     Lineout */
	char		*inc_fid;	/* name */
	FILE		*inc_infile;	/* In_file */
	FILE		*inc_outfile;	/* Out_file */
	struct inc_file	*inc_next;
};


/* 
** Structure for yacc generated terminal codes
**	This avoids having to include functions in
**	[grammar.y] for the function to know the 
**	parser generated token numbers.
*/

struct special
{
	int	sp_name;		/* NAME */
	int	sp_sconst;		/* SCONST */
	int	sp_i2const;		/* I2CONST */
	int	sp_i4const;		/* I4CONST */
	int	sp_f8const;		/* F8CONST */
	int	sp_quote;		/* QUOTE */
	int	sp_bgncmnt;		/* BGNCMNT */
	int	sp_endcmnt;		/* ENDCMNT */
	int	sp_c_code;		/* C_CODE */
	int	sp_struct_var;
};


/*
**	Global definitions 
*/

int		Opcode;			/* operator code to distinguis tokens */
extern int	yychar;			/* yacc input token */
int		yyline;			/* yacc external line counter */
extern int	yydebug;		/* set by "-y" command line arg,
					 * has yacc parser give details
					 * of parse
					 */
int		Newline;		/* less than one token has been
					 * read from the input line, and
					 *  yylex isn't sure yet whether it is
					 * C_CODE or not
					 */
int		Lineout;		/* output line count */
int		In_quote;		/* set if inside an IIwrite("... */
char		*Input_file_name;	/* input file name */
int		Type_spec;		/* used in parser, in C-declarations */
int 		C_code_flg;		/* set to indicate C-code in scanner */
int 		Pre_proc_flg;		/* set to indicate a 
					 * pre-processor line 
					 */
int		Block_level;		/* > 0 local, 0 global */
int		Indir_level;		/* holds indirection level
					 * of a reference to a C-var
					 */
int		Field_indir;		/* indirection on a field of
					 * a structure variable
					 */
int		Opflag;			/* mode in which to interpret syntax */
int		In_string;		/* set if output is in a string */
int		Fillmode;		/* set if output line is being filled */
int		Fillcnt;		/* count to fill a line to */
int		Charcnt;		/* # chars written onto output line */
int		Lastc;			/* type of last char read (OPCHAR or 
					 * KEYCHAR)
					 */
int		Arg_error;		/* set in argproc() [main.c] on
					 * error in prcessing command line
					 * arguments.
					 */
int		Rtdb;			/* set by "-d" command line arg,
					 * supplies equel runtime support
					 * with info to report file name
					 * and line number in case of an
					 * error
					 */
int		Kwrdnum;		/* # of entries in Kwrdtab [tokens.y] */
char		Line_buf [MAXSTRING + 1];	/* buffer for input line */
char		*Line_pos;		/* position in input line */
int		Peekc [2];		/* holds backup character */
int		Struct_flag;		/* while processing a structure
					 * variable declaration, is set
					 */
struct cvar	*Cvarp, *Fieldp;		/* to process C variables */



struct optab 		Kwrdtab [];	/* table of key words [tokens.y] */
struct optab 		Optab [];	/* table of operators [tokens.y] */
struct special		Tokens;		/* holds yacc generated lex codes
					 * [tokens.y]
					 */
char			Cmap [];	/* a type for each character [cmap.c] */
struct cvar		*C_globals;	/* Global C-variable tree */
struct cvar		*C_locals;	/* Local C-variable tree */
struct cvar		*F_locals;	/* Local structure fields */
struct cvar		*F_globals;	/* Global structure fields */
struct display		Displays [1];	/* Cv_display is set to point at this
					 * so that Cv_display may be passed
					 * as a parameter instead of &Cv_display
					 */
struct display		*Cv_display;	/* display into which "cvarx"'s 
					 * get put 
					 */
struct display		Symsp;		/* storage for symbols read by
					 * yylex() 
					 */
struct ret_list		Ret_list;	/* list of variables in a "ctlelm" */
struct inc_file		*Inc_files;	/* stack of files pushed by #includes */




/*
**	I/O manipulation data structures
*/

FILE	*In_file;		/* input file */
FILE	*Out_file;		/* output file */

# ifdef xDEBUG
char	Lex_debug;		/* debugging info for lexemes in yylex()
				 * [yylex.c]
				 */
char	Chardebug;		/* print debugging info for routine
				 * in getch.c
				 */
# endif

extern struct cvar	*getcvar(), *getfield();
extern struct disp_node	*addsym();
