#
/*
**	SCANNER.H
**	contains the global structures and variable declarations needed
**	by the lexical analyzer.  This includes Manifest Constants and
**	certain variables for internal communication purposes.  Therefore,
**	extreme care should be exercised when modifying this file.
**
**	Version:
**		@(#)scanner.h	7.1	2/5/81
*/


/* MANIFEST CONSTANTS */
# define	SBUFSIZ		2000	/* max size of symbol table for */
# define	MAXSTRING	255	/* max length of strings */
# define	GOVAL		-1	/* semantic value for command names */

# ifndef	WARN
# define	WARN		0
# define	FATAL		1
# endif

/* CONSTANTS FOR THE GET_SCANNER CALL */
# define	NORMAL		0	/* get a character from front */
# define	PRIME		1	/* prime the front end */
# define	SYNC		2	/* sync the front end */

/* CHARACTER TYPES */
# define	ALPHA		1
# define	NUMBR		2
# define	OPATR		3
# define	PUNCT		4
# define	CNTRL		5

/* Modes for input from EQUEL front end */
# define	CVAR_I2		'\1'	/* 2 byte integer */
# define	CVAR_F8		'\4'	/* 8 byte floating number */
# define	CVAR_S		'\3'	/* string with null byte */
# define	CVAR_I4		'\6'	/* 4 byte integer */

/* error number definitions */
# define	SYMERR		2600	/* syntactic error */
# define	STRTERM		2700	/* non term string */
# define	STRLONG		2701	/* string too long */
# define	BADOP		2702	/* can't find operator in tables */
# define	NAMELONG	2703	/* name too long */
# define	SBUFOFLO	2704	/* over flow symbol table */
# define	COMMTERM	2705	/* non term comment */
# define	FCONSTERR	2707	/* float constant error */
# define	CNTRLCHR	2708	/* control char from equel */
# define	NUMBUFOFLO	2709	/* buffer oflo in number.c */
/* error number for yacc stack overflow */
# define	YOVRFLOW	2800	/* if yacc stack ^ */

/* KEYWORD and OPERATOR TABLE */
struct optab				/* key word/operator tables */
{
	char	*term;			/* key word/operator body */
	int	token;			/* associated parser token */
	int	opcode;			/* associated parser opcode */
};

/* SPECIAL TOKENS for scanner */
struct special
{
	int	sconst;
	int	bgncmnt;
	int	endcmnt;
	int	i2const;
	int	i4const;
	int	f4const;
	int	f8const;
	int	name;
};

/* last token struct */
struct lastok
{
	int	toktyp;
	char	*tok;
	int	tokop;
};

/* declarations */
struct special	Tokens;			/* special tokens table */
struct optab	Optab[];		/* operator table */
struct optab	Keyword[];		/* keyword table */
struct lastok	Lastok;
int		Opcode;			/* opcode for current token */
int		Lcase;			/* UPPER->lower conversion flag */
int		Pars;			/* flag for call to getcvar or not */
int		Newline;		/* set if last char read was a newline */
int		Cflag;			/* set if line of C-code recognized */
int		Keyent;			/* number of entries in the Keyword table */

char		Sbuf[SBUFSIZ];		/* symbol table buffer */
/*
**	TRACE FLAG ASSIGNMENTS
**	Comment, Expand		70
**	Name, String		71
**	Number, Yylex		72
**	Operator		73
**	Inout			74
*/
