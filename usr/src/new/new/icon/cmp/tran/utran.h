#include <stdio.h>
#include "../h/config.h"

/*
 * external definitions needed throughout translator
 */

extern int yychar;		/* parser's current input token type */
extern int yynerrs;		/* number of errors in parse */
extern int fatalerrs;		/* total number of fatal errors */
extern int warnings;		/* total number of warnings */
extern int nocode;		/* true to suppress code generation */
extern int implicit;		/* implicit local/error */
extern int silence;		/* verbose/mute switch */
extern int trace;		/* initial setting of &trace */

extern int inline;		/* current line number in input */
extern int tline;		/* line number of current token */
extern int incol;		/* current column number in input */
extern int tcol;		/* column number of current token */
extern int peekc;		/* one character look-ahead */
extern char ctran[];		/* input translation table */
extern char esctab[];		/* string literal escape table */
extern FILE *infile;		/* current input file */
extern FILE *codefile;		/* current icode output file */
extern FILE *globfile;		/* current global output file */
extern char **filep;		/* list of input file names */

extern int alclflg;		/* counter for local table overflow */
extern int alcgflg;		/* counter for global table overflow */
extern int alccflg;		/* counter for constant table overflow */

extern char *strings;		/* string space */
extern char *send;		/* end of string space */
extern char *sfree;		/* free pointer for string space */
extern int ssize;		/* initial size of string space */
