/*
 * Copyright (c) 1994 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Definitions for calculator program.
 */

#ifndef	CALC_H
#define	CALC_H


#include <stdio.h>
#include <setjmp.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "value.h"


/*
 * Configuration definitions
 */
#define	CALCPATH	"CALCPATH"	/* environment variable for files */
#define	CALCRC		"CALCRC"	/* environment variable for startup */
#define	CALCBINDINGS	"CALCBINDINGS"	/* environment variable for hist bindings */
#define	HOME		"HOME"		/* environment variable for home dir */
#define	PAGER		"PAGER"		/* environment variable for help */
#define	SHELL		"SHELL"		/* environment variable for shell */
#define DEFAULTCALCHELP	"help"		/* help file that -h prints */
#define DEFAULTSHELL	"sh"		/* default shell to use */
#define	CALCEXT		".cal"	/* extension for files read in */
#define	PATHSIZE	1024	/* maximum length of path name */
#define	HOMECHAR	'~'	/* char which indicates home directory */
#define DOTCHAR		'.'	/* char which indicates current directory */
#define	PATHCHAR	'/'	/* char which separates path components */
#define	LISTCHAR	':'	/* char which separates paths in a list */
#define	MAXCMD		1024	/* maximum length of command invocation */
#define	MAXERROR	512	/* maximum length of error message string */

#define	SYMBOLSIZE	256	/* maximum symbol name size */
#define	MAXINDICES	20	/* maximum number of indices for objects */
#define	MAXLABELS	100	/* maximum number of user labels in function */
#define	MAXOBJECTS	10	/* maximum number of object types */
#define	MAXSTRING	1024	/* maximum size of string constant */
#define	MAXSTACK	1000	/* maximum depth of evaluation stack */
#define	MAXFILES	20	/* maximum number of opened files */
#define PROMPT1		"> "	/* normal prompt */
#define PROMPT2		">> "	/* prompt when inside multi-line input */


#define	TRACE_NORMAL	0x00	/* normal trace flags */
#define	TRACE_OPCODES	0x01	/* trace every opcode */
#define	TRACE_NODEBUG	0x02	/* suppress debugging opcodes */
#define	TRACE_MAX	0x03	/* maximum value for trace flag */

#define DISPLAY_DEFAULT 20	/* default digits for float display */
#define EPSILON_DEFAULT "1e-20"	/* allowed error for float calculations */
#define MAXPRINT_DEFAULT 16	/* default number of elements printed */

#define ABORT_NONE	0	/* abort not needed yet */
#define ABORT_STATEMENT	1	/* abort on statement boundary */
#define ABORT_OPCODE	2	/* abort on any opcode boundary */
#define ABORT_MATH	3	/* abort on any math operation */
#define ABORT_NOW	4	/* abort right away */

#define CONFIG_MODE	1	/* types of configuration parameters */
#define CONFIG_DISPLAY	2
#define CONFIG_EPSILON	3
#define CONFIG_TRACE	4
#define CONFIG_MAXPRINT	5
#define	CONFIG_MUL2	6
#define	CONFIG_SQ2	7
#define	CONFIG_POW2	8
#define	CONFIG_REDC2	9
#define CONFIG_TILDE   10
#define CONFIG_TAB     11


/*
 * File ids corresponding to standard in, out, error, and when not in use.
 */
#define	FILEID_STDIN	((FILEID) 0)
#define	FILEID_STDOUT	((FILEID) 1)
#define	FILEID_STDERR	((FILEID) 2)
#define	FILEID_NONE	((FILEID) -1)

/*
 * File I/O routines.
 */
extern FILEID openid MATH_PROTO((char *name, char *mode));
extern FILEID indexid MATH_PROTO((long index));
extern BOOL validid MATH_PROTO((FILEID id));
extern BOOL errorid MATH_PROTO((FILEID id));
extern BOOL eofid MATH_PROTO((FILEID id));
extern BOOL closeid MATH_PROTO((FILEID id));
extern int getcharid MATH_PROTO((FILEID id));
extern void idprintf MATH_PROTO((FILEID id, char *fmt, int count, VALUE **vals));
extern void printid MATH_PROTO((FILEID id, int flags));
extern void flushid MATH_PROTO((FILEID id));
extern void readid MATH_PROTO((FILEID id, char **retptr));


/*
 * Input routines.
 */
extern FILE *f_open MATH_PROTO((char *name, char *mode));
extern int openstring MATH_PROTO((char *str));
extern int openterminal MATH_PROTO((void));
extern int opensearchfile MATH_PROTO((char *name, char *pathlist, char *exten, int reopen_ok));
extern char *nextline MATH_PROTO((void));
extern int nextchar MATH_PROTO((void));
extern void reread MATH_PROTO((void));
extern void resetinput MATH_PROTO((void));
extern void setprompt MATH_PROTO((char *));
extern BOOL inputisterminal MATH_PROTO((void));
extern char *inputname MATH_PROTO((void));
extern long linenumber MATH_PROTO((void));
extern void runrcfiles MATH_PROTO((void));


/*
 * Other routines.
 */
extern NUMBER *constvalue MATH_PROTO((long index));
extern long addnumber MATH_PROTO((char *str));
extern long addqconstant MATH_PROTO((NUMBER *q));
extern void initstack MATH_PROTO((void));
extern void version MATH_PROTO((FILE *stream));
extern void getcommands MATH_PROTO((BOOL toplevel));
extern void givehelp MATH_PROTO((char *type));

extern void getconfig MATH_PROTO((int type, VALUE *vp));
extern void setconfig MATH_PROTO((int type, VALUE *vp));
extern int configtype MATH_PROTO((char *name));


/*
 * Global data definitions.
 */
extern long maxprint;		/* number of elements to print */
extern int abortlevel;		/* current level of aborts */
extern BOOL inputwait;		/* TRUE if in a terminal input wait */
extern FLAG traceflags;		/* tracing flags */
extern VALUE *stack;		/* execution stack */
extern jmp_buf jmpbuf;		/* for errors */

extern char *calcpath;		/* $CALCPATH or default */
extern char *calcrc;		/* $CALCRC or default */
extern char *calcbindings;	/* $CALCBINDINGS or default */
extern char *home;		/* $HOME or default */
extern char *shell;		/* $SHELL or default */

#endif

/* END CODE */
