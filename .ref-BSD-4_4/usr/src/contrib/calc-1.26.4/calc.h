/*
 * Copyright (c) 1993 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Definitions for calculator program.
 */

#include <stdio.h>
#include <setjmp.h>
#include "math.h"


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
#define MAXDIM		4	/* maximum number of dimensions in matrices */
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
#define USUAL_ELEMENTS	4	/* usual number of elements for object */

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


/*
 * Flags to modify results from the printvalue routine.
 * These flags are OR'd together.
 */
#define	PRINT_NORMAL	0x00	/* print in normal manner */
#define	PRINT_SHORT	0x01	/* print in short format (no elements) */
#define	PRINT_UNAMBIG	0x02	/* print in non-ambiguous manner */


/*
 * Definition of values of various types.
 */
typedef struct value VALUE;
typedef struct object OBJECT;
typedef struct matrix MATRIX;
typedef struct list LIST;
typedef	long FILEID;


struct value {
	short v_type;			/* type of value */
	short v_subtype;		/* other data related to some types */
	union {
		long vv_int;		/* small integer value */
		FILEID vv_file;		/* id of opened file */
		NUMBER *vv_num;		/* arbitrary sized numeric value */
		COMPLEX *vv_com;	/* complex number */
		VALUE *vv_addr;		/* address of variable value */
		MATRIX *vv_mat;		/* address of matrix */
		LIST *vv_list;		/* address of list */
		OBJECT *vv_obj;		/* address of object */
		char *vv_str;		/* string value */
	} v_union;
};


/*
 * For ease in referencing
 */
#define v_int	v_union.vv_int
#define	v_file	v_union.vv_file
#define v_num	v_union.vv_num
#define v_com	v_union.vv_com
#define v_addr	v_union.vv_addr
#define v_str	v_union.vv_str
#define v_mat	v_union.vv_mat
#define	v_list	v_union.vv_list
#define v_obj	v_union.vv_obj
#define	v_valid	v_union.vv_int


/*
 * Value types.
 */
#define V_NULL	0	/* null value */
#define V_INT	1	/* normal integer */
#define V_NUM	2	/* number */
#define V_COM	3	/* complex number */
#define V_ADDR	4	/* address of variable value */
#define V_STR	5	/* address of string */
#define V_MAT	6	/* address of matrix structure */
#define	V_LIST	7	/* address of list structure */
#define V_OBJ	8	/* address of object structure */
#define	V_FILE	9	/* opened file id */
#define V_MAX	9	/* highest legal value */

#define V_STRLITERAL	0	/* string subtype for literal str */
#define V_STRALLOC	1	/* string subtype for allocated str */

#define TWOVAL(a,b) ((a) * (V_MAX+1) + (b))	/* for switch of two values */


/*
 * Structure of a matrix.
 */
struct matrix {
	long m_dim;		/* dimension of matrix */
	long m_size;		/* total number of elements */
	long m_min[MAXDIM];	/* minimum bound for indices */
	long m_max[MAXDIM];	/* maximum bound for indices */
	VALUE m_table[1];	/* actually varying length table */
};

#define matsize(n) (sizeof(MATRIX) - sizeof(VALUE) + ((n) * sizeof(VALUE)))


/*
 * List definitions.
 * An individual list element.
 */
typedef struct listelem LISTELEM;
struct listelem {
	LISTELEM *e_next;	/* next element in list (or NULL) */
	LISTELEM *e_prev;	/* previous element in list (or NULL) */
	VALUE e_value;		/* value of this element */
};


/*
 * Structure for a list of elements.
 */
struct list {
	LISTELEM *l_first;	/* first list element (or NULL) */
	LISTELEM *l_last;	/* last list element (or NULL) */
	LISTELEM *l_cache;	/* cached list element (or NULL) */
	long l_cacheindex;	/* index of cached element (or undefined) */
	long l_count;		/* total number of elements in the list */
};

extern void insertlistfirst(), insertlistlast(), insertlistmiddle();
extern void removelistfirst(), removelistlast(), removelistmiddle();
extern void listfree(), listprint();
extern long listsearch(), listrsearch();
extern BOOL listcmp();
extern VALUE *listindex();
extern LIST *listalloc(), *listcopy();


/*
 * Object actions.
 */
#define OBJ_PRINT	0	/* print the value */
#define OBJ_ONE		1	/* create the multiplicative identity */
#define OBJ_TEST	2	/* test a value for "zero" */
#define OBJ_ADD		3	/* add two values */
#define OBJ_SUB		4	/* subtrace one value from another */
#define OBJ_NEG		5	/* negate a value */
#define OBJ_MUL		6	/* multiply two values */
#define OBJ_DIV		7	/* divide one value by another */
#define OBJ_INV		8	/* invert a value */
#define OBJ_ABS		9	/* take absolute value of value */
#define OBJ_NORM	10	/* take the norm of a value */
#define OBJ_CONJ	11	/* take the conjugate of a value */
#define OBJ_POW		12	/* take the power function */
#define OBJ_SGN		13	/* return the sign of a value */
#define OBJ_CMP		14	/* compare two values for equality */
#define OBJ_REL		15	/* compare two values for inequality */
#define OBJ_QUO		16	/* integer quotient of values */
#define OBJ_MOD		17	/* remainder of division of values */
#define OBJ_INT		18	/* integer part of */
#define OBJ_FRAC	19	/* fractional part of */
#define OBJ_INC		20	/* increment by one */
#define OBJ_DEC		21	/* decrement by one */
#define OBJ_SQUARE	22	/* square value */
#define OBJ_SCALE	23	/* scale by power of two */
#define OBJ_SHIFT	24	/* shift left (or right) by number of bits */
#define OBJ_ROUND	25	/* round to specified decimal places */
#define OBJ_BROUND	26	/* round to specified binary places */
#define OBJ_ROOT	27	/* take nth root of value */
#define OBJ_SQRT	28	/* take square root of value */
#define OBJ_MAXFUNC	28	/* highest function */


/*
 * Definition of an object type.
 * This is actually a varying sized structure.
 */
typedef struct {
	char *name;			/* name of object */
	int count;			/* number of elements defined */
	int actions[OBJ_MAXFUNC+1];	/* function indices for actions */
	int elements[1];		/* element indexes (MUST BE LAST) */
} OBJECTACTIONS;

#define objectactionsize(elements) \
	(sizeof(OBJECTACTIONS) + ((elements) - 1) * sizeof(int))


/*
 * Structure of an object.
 * This is actually a varying sized structure.
 * However, there are always at least USUAL_ELEMENTS values in the object.
 */
struct object {
	OBJECTACTIONS *o_actions;	/* action table for this object */
	VALUE o_table[USUAL_ELEMENTS];	/* object values (MUST BE LAST) */
};

#define objectsize(elements) \
	(sizeof(OBJECT) + ((elements) - USUAL_ELEMENTS) * sizeof(VALUE))


/*
 * File ids corresponding to standard in, out, error, and when not in use.
 */
#define	FILEID_STDIN	((FILEID)0)
#define	FILEID_STDOUT	((FILEID)1)
#define	FILEID_STDERR	((FILEID)2)
#define	FILEID_NONE	((FILEID)-1)


/*
 * Common definitions
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


/*
 * Functions.
 */
extern MATRIX *matadd(), *matsub(), *matmul(), *matneg();
extern MATRIX *matalloc(), *matcopy(), *matsquare(), *matinv();
extern MATRIX *matscale(), *matmulval(), *matpowi(), *matconj(), *matquoval();
extern MATRIX *matmodval(), *matint(), *matfrac(), *matround(), *matbround();
extern MATRIX *mattrans(), *matcross(), *matshift();
extern BOOL mattest(), matcmp();
extern long matsearch(), matrsearch();
extern VALUE matdet(), matdot();
extern void matfill(), matfree(), matprint();

#if 0
extern BOOL matisident();
#endif

extern OBJECT *objcopy(), *objalloc();
extern VALUE objcall();
extern void objfree();
extern void objuncache();
extern int addelement();
extern int defineobject();
extern int checkobject();
extern void showobjfuncs();
extern int findelement();
extern int objoffset();

extern void freevalue(), copyvalue(), negvalue(), addvalue(), subvalue();
extern void mulvalue(), squarevalue(), invertvalue(), roundvalue();
extern void broundvalue(), intvalue(), fracvalue(), incvalue(), decvalue();
extern void conjvalue(), sqrtvalue(), rootvalue(), absvalue(), normvalue();
extern void shiftvalue(), scalevalue(), powivalue(), powervalue();
extern void divvalue(), quovalue(), modvalue(), printvalue();
extern BOOL testvalue(), comparevalue();
extern FLAG relvalue();

extern FILEID openid(), indexid();
extern BOOL validid(), errorid(), eofid(), closeid();
extern int getcharid();
extern void idprintf(), printid(), flushid(), readid();

extern FILE *f_open();
extern int openstring();
extern int openterminal();
extern int opensearchfile();
extern int nextchar();
extern void reread();

extern VALUE builtinfunc();
extern NUMBER *constvalue();
extern long addnumber(), addqconstant();
extern long linenumber();
extern char *builtinname();
extern char *inputname();
extern BOOL inputisterminal();
extern void resetinput();
extern char *nextline();
extern void calculate();
extern void initstack();
extern int dumpop();
extern void version();		/* print version string */
extern void runrcfiles();
extern void getcommands();
extern void givehelp();
extern void setprompt();

extern void getconfig();
extern void setconfig();
extern int configtype();

#ifdef VARARGS
void error();
#else
# ifdef __STDC__
void error(char *, ...);
# else
void error();
# endif
#endif


/* END CODE */
