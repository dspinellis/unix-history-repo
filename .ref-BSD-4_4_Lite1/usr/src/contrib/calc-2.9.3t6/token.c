/*
 * Copyright (c) 1994 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Read input file characters into tokens
 */

#include "stdarg.h"
#include "calc.h"
#include "token.h"
#include "string.h"


#define isletter(ch)	((((ch) >= 'a') && ((ch) <= 'z')) || \
				(((ch) >= 'A') && ((ch) <= 'Z')))
#define isdigit(ch)	(((ch) >= '0') && ((ch) <= '9'))
#define issymbol(ch)	(isletter(ch) || isdigit(ch) || ((ch) == '_'))


/*
 * Current token.
 */
static struct {
	short t_type;		/* type of token */
	char *t_str;		/* string value or symbol name */
	long t_numindex;	/* index of numeric value */
} curtoken;


static BOOL rescan;		/* TRUE to reread current token */
static BOOL newlines;		/* TRUE to return newlines as tokens */
static BOOL allsyms;		/* TRUE if always want a symbol token */
static STRINGHEAD strings;	/* list of constant strings */
static char *numbuf;		/* buffer for numeric tokens */
static long numbufsize;		/* current size of numeric buffer */

long errorcount;		/* number of compilation errors */


/*
 * Table of keywords
 */
struct keyword {
	char *k_name;	/* keyword name */
	int k_token;	/* token number */
};

static struct keyword keywords[] = {
	"if",		T_IF,
	"else",		T_ELSE,
	"for",		T_FOR,
	"while",	T_WHILE,
	"do",		T_DO,
	"continue",	T_CONTINUE,
	"break",	T_BREAK,
	"goto",		T_GOTO,
	"return",	T_RETURN,
	"local",	T_LOCAL,
	"global",	T_GLOBAL,
	"static",	T_STATIC,
	"switch",	T_SWITCH,
	"case",		T_CASE,
	"default",	T_DEFAULT,
	"quit",		T_QUIT,
	"exit",		T_QUIT,
	"define",	T_DEFINE,
	"read",		T_READ,
	"show",		T_SHOW,
	"help",		T_HELP,
	"write",	T_WRITE,
	"mat",		T_MAT,
	"obj",		T_OBJ,
	"print",	T_PRINT,
	NULL,		0
};


static void eatcomment MATH_PROTO((void));
static void eatstring MATH_PROTO((int quotechar));
static int eatsymbol MATH_PROTO((void));
static int eatnumber MATH_PROTO((void));


/*
 * Initialize all token information.
 */
void
inittokens()
{
	initstr(&strings);
	newlines = FALSE;
	allsyms = FALSE;
	rescan = FALSE;
	setprompt(PROMPT1);
}


/*
 * Set the new token mode according to the specified flag, and return the
 * previous value of the flag.
 */
int
tokenmode(flag)
	int flag;
{
	int	oldflag;

	oldflag = TM_DEFAULT;
	if (newlines)
		oldflag |= TM_NEWLINES;
	if (allsyms)
		oldflag |= TM_ALLSYMS;
	newlines = FALSE;
	allsyms = FALSE;
	if (flag & TM_NEWLINES)
		newlines = TRUE;
	if (flag & TM_ALLSYMS)
		allsyms = TRUE;
	setprompt(newlines ? PROMPT1 : PROMPT2);
	return oldflag;
}


/*
 * Routine to read in the next token from the input stream.
 * The type of token is returned as a value.  If the token is a string or
 * symbol name, information is saved so that the value can be retrieved.
 */
int
gettoken()
{
	int ch;			/* current input character */
	int type;		/* token type */

	if (rescan) {		/* rescanning */
		rescan = FALSE;
		return curtoken.t_type;
	}
	curtoken.t_str = NULL;
	curtoken.t_numindex = 0;
	type = T_NULL;
	while (type == T_NULL) {
		ch = nextchar();
		if (allsyms && ((ch!=' ') && (ch!=';') && (ch!='"') && (ch!='\n'))) {
			reread();
			type = eatsymbol();
			break;
		}
		switch (ch) {
		case ' ':
		case '\t':
		case '\0':
			break;
		case '\n':
			if (newlines)
				type = T_NEWLINE;
			break;
		case EOF: type = T_EOF; break;
		case '{': type = T_LEFTBRACE; break;
		case '}': type = T_RIGHTBRACE; break;
		case '(': type = T_LEFTPAREN; break;
		case ')': type = T_RIGHTPAREN; break;
		case '[': type = T_LEFTBRACKET; break;
		case ']': type = T_RIGHTBRACKET; break;
		case ';': type = T_SEMICOLON; break;
		case ':': type = T_COLON; break;
		case ',': type = T_COMMA; break;
		case '?': type = T_QUESTIONMARK; break;
		case '"':
		case '\'':
			type = T_STRING;
			eatstring(ch);
			break;
		case '^':
			switch (nextchar()) {
				case '=': type = T_POWEREQUALS; break;
				default: type = T_POWER; reread();
			}
			break;
		case '=':
			switch (nextchar()) {
				case '=': type = T_EQ; break;
				default: type = T_ASSIGN; reread();
			}
			break;
		case '+':
			switch (nextchar()) {
				case '+': type = T_PLUSPLUS; break;
				case '=': type = T_PLUSEQUALS; break;
				default: type = T_PLUS; reread();
			}
			break;
		case '-':
			switch (nextchar()) {
				case '-': type = T_MINUSMINUS; break;
				case '=': type = T_MINUSEQUALS; break;
				default: type = T_MINUS; reread();
			}
			break;
		case '*':
			switch (nextchar()) {
				case '=': type = T_MULTEQUALS; break;
				case '*':
					switch (nextchar()) {
						case '=': type = T_POWEREQUALS; break;
						default: type = T_POWER; reread();
					}
					break;
				default: type = T_MULT; reread();
			}
			break;
		case '/':
			switch (nextchar()) {
				case '/':
					switch (nextchar()) {
						case '=': type = T_SLASHSLASHEQUALS; break;
						default: reread(); type = T_SLASHSLASH; break;
					}
					break;
				case '=': type = T_DIVEQUALS; break;
				case '*': eatcomment(); break;
				default: type = T_DIV; reread();
			}
			break;
		case '%':
			switch (nextchar()) {
				case '=': type = T_MODEQUALS; break;
				default: type = T_MOD; reread();
			}
			break;
		case '<':
			switch (nextchar()) {
				case '=': type = T_LE; break;
				case '<':
					switch (nextchar()) {
						case '=': type = T_LSHIFTEQUALS; break;
						default:  reread(); type = T_LEFTSHIFT; break;
					}
					break;
				default: type = T_LT; reread();
			}
			break;
		case '>':
			switch (nextchar()) {
				case '=': type = T_GE; break;
				case '>':
					switch (nextchar()) {
						case '=': type = T_RSHIFTEQUALS; break;
						default:  reread(); type = T_RIGHTSHIFT; break;
					}
					break;
				default: type = T_GT; reread();
			}
			break;
		case '&':
			switch (nextchar()) {
				case '&': type = T_ANDAND; break;
				case '=': type = T_ANDEQUALS; break;
				default: type = T_AND; reread(); break;
			}
			break;
		case '|':
			switch (nextchar()) {
				case '|': type = T_OROR; break;
				case '=': type = T_OREQUALS; break;
				default: type = T_OR; reread(); break;
			}
			break;
		case '!':
			switch (nextchar()) {
				case '=': type = T_NE; break;
				default: type = T_NOT; reread(); break;
			}
			break;
		case '\\':
			switch (nextchar()) {
				case '\n': setprompt(PROMPT2); break;
				default: scanerror(T_NULL, "Unknown token character '%c'", ch);
			}
			break;
		default:
			if (isletter(ch)) {
				reread();
				type = eatsymbol();
				break;
			}
			if (isdigit(ch) || (ch == '.')) {
				reread();
				type = eatnumber();
				break;
			}
			scanerror(T_NULL, "Unknown token character '%c'", ch);
		}
	}
	curtoken.t_type = (short)type;
	return type;
}


/*
 * Continue to eat up a comment string.
 * The leading slash-asterisk has just been scanned at this point.
 */
static void
eatcomment()
{
	int ch;

	for (;;) {
		ch = nextchar();
		if (ch == '*') {
			ch = nextchar();
			if (ch == '/')
				return;
			reread();
		}
		if ((ch == EOF) || (ch == '\0') ||
			(newlines && (ch == '\n') && inputisterminal())) {
				reread();
				scanerror(T_NULL, "Unterminated comment");
				return;
		}
	}
}


/*
 * Read in a string and add it to the literal string pool.
 * The leading single or double quote has been read in at this point.
 */
static void
eatstring(quotechar)
	int quotechar;
{
	register char *cp;	/* current character address */
	int ch;			/* current character */
	char buf[MAXSTRING+1];	/* buffer for string */

	cp = buf;
	for (;;) {
		ch = nextchar();
		switch (ch) {
			case '\0':
			case EOF:
			case '\n':
				reread();
				scanerror(T_NULL, "Unterminated string constant");
				*cp = '\0';
				curtoken.t_str = addliteral(buf);
				return;

			case '\\':
				ch = nextchar();
				switch (ch) {
					case 'n': ch = '\n'; break;
					case 'r': ch = '\r'; break;
					case 't': ch = '\t'; break;
					case 'b': ch = '\b'; break;
					case 'f': ch = '\f'; break;
					case '\n':
						setprompt(PROMPT2);
						continue;
					case EOF:
						reread();
						continue;
				}
				*cp++ = (char)ch;
				break;

			case '"':
			case '\'':
				if (ch == quotechar) {
					*cp = '\0';
					curtoken.t_str = addliteral(buf);
					return;
				}
				/* fall into default case */

			default:
				*cp++ = (char)ch;
		}
	}
}


/*
 * Read in a symbol name which may or may not be a keyword.
 * If allsyms is set, keywords are not looked up and almost all chars
 * will be accepted for the symbol.  Returns the type of symbol found.
 */
static int
eatsymbol()
{
	register struct keyword *kp;	/* pointer to current keyword */
	register char *cp;		/* current character pointer */
	int ch;				/* current character */
	int cc;				/* character count */
	static char buf[SYMBOLSIZE+1];	/* temporary buffer */

	cp = buf;
	cc = SYMBOLSIZE;
	if (allsyms) {
		for (;;) {
			ch = nextchar();
			if ((ch == ' ') || (ch == ';') || (ch == '\n'))
				break;
			if (cc-- > 0)
				*cp++ = (char)ch;
		}
		reread();
		*cp = '\0';
		if (cc < 0)
			scanerror(T_NULL, "Symbol too long");
		curtoken.t_str = buf;
		return T_SYMBOL;
	}
	for (;;) {
		ch = nextchar();
		if (!issymbol(ch))
			break;
		if (cc-- > 0)
			*cp++ = (char)ch;
	}
	reread();
	*cp = '\0';
	if (cc < 0)
		scanerror(T_NULL, "Symbol too long");
	for (kp = keywords; kp->k_name; kp++)
		if (strcmp(kp->k_name, buf) == 0)
			return kp->k_token;
	curtoken.t_str = buf;
	return T_SYMBOL;
}


/*
 * Read in and remember a possibly numeric constant value.
 * The constant is inserted into a constant table so further uses
 * of the same constant will not take more memory.  This can also
 * return just a period, which is used for element accesses and for
 * the old numeric value.
 */
static int
eatnumber()
{
	register char *cp;	/* current character pointer */
	long len;		/* parsed size of number */
	long res;		/* result of parsing number */

	if (numbufsize == 0) {
		numbuf = (char *)malloc(128+1);
		if (numbuf == NULL)
			math_error("Cannot allocate number buffer");
		numbufsize = 128;
	}
	cp = numbuf;
	len = 0;
	for (;;) {
		if (len >= numbufsize) {
			cp = (char *)realloc(numbuf, numbufsize + 1001);
			if (cp == NULL)
				math_error("Cannot reallocate number buffer");
			numbuf = cp;
			numbufsize += 1000;
			cp = &numbuf[len];
		}
		*cp = nextchar();
		*(++cp) = '\0';
		if ((numbuf[0] == '.') && isletter(numbuf[1])) {
			reread();
			return T_PERIOD;
		}
		res = qparse(numbuf, QPF_IMAG);
		if (res < 0) {
			reread();
			scanerror(T_NULL, "Badly formatted number");
			curtoken.t_numindex = addnumber("0");
			return T_NUMBER;
		}
		if (res != ++len)
			break;
	}
	cp[-1] = '\0';
	reread();
	if ((numbuf[0] == '.') && (numbuf[1] == '\0')) {
		curtoken.t_numindex = 0;
		return T_OLDVALUE;
	}
	cp -= 2;
	res = T_NUMBER;
	if ((*cp == 'i') || (*cp == 'I')) {
		*cp = '\0';
		res = T_IMAGINARY;
	}
	curtoken.t_numindex = addnumber(numbuf);
	return res;
}


/*
 * Return the string value of the current token.
 */
char *
tokenstring()
{
	return curtoken.t_str;
}


/*
 * Return the constant index of a numeric token.
 */
long
tokennumber()
{
	return curtoken.t_numindex;
}


/*
 * Push back the token just read so that it will be seen again.
 */
void
rescantoken()
{
	rescan = TRUE;
}


/*
 * Describe an error message.
 * Then skip to the next specified token (or one more powerful).
 */
#ifdef VARARGS
# define VA_ALIST skip, fmt, va_alist
# define VA_DCL int skip; char *fmt; va_dcl
#else
# if defined(__STDC__) && __STDC__ == 1
#  define VA_ALIST int skip, char *fmt, ...
#  define VA_DCL
# else
#  define VA_ALIST skip, fmt
#  define VA_DCL int skip; char *fmt;
# endif
#endif
/*VARARGS*/
void
scanerror(VA_ALIST)
	VA_DCL
{
	va_list ap;
	char *name;		/* name of file with error */
	char buf[MAXERROR+1];

	errorcount++;
	name = inputname();
	if (name)
		fprintf(stderr, "\"%s\", line %ld: ", name, linenumber());
#ifdef VARARGS
	va_start(ap);
#else
	va_start(ap, fmt);
#endif
	vsprintf(buf, fmt, ap);
	va_end(ap);
	fprintf(stderr, "%s\n", buf);
	switch (skip) {
		case T_NULL:
			return;
		case T_COMMA:
			rescan = TRUE;
			for (;;) {
				switch (gettoken()) {
				case T_NEWLINE:
				case T_SEMICOLON:
				case T_LEFTBRACE:
				case T_RIGHTBRACE:
				case T_EOF:
				case T_COMMA:
					rescan = TRUE;
					return;
				}
			}
		default:
			fprintf(stderr, "Unknown skip token for scanerror\n");
			/* fall into semicolon case */
			/*FALLTHRU*/
		case T_SEMICOLON:
			rescan = TRUE;
			for (;;) switch (gettoken()) {
				case T_NEWLINE:
				case T_SEMICOLON:
				case T_LEFTBRACE:
				case T_RIGHTBRACE:
				case T_EOF:
					rescan = TRUE;
					return;
			}
	}
}

/* END CODE */
