#

/*
 * Structure of a tree node
 */

typedef	struct node	*nodeptr;

struct node {
   int n_type;			/* node type */
   int n_line;			/* line number in source program */
   int n_col;			/* column number in source program */
   union {
      int n_val;		/* integer-valued fields */
      char *n_str;		/* string-valued fields */
      nodeptr n_ptr;   	     	/* subtree pointers */
      } n_field[4];
   };

/*
 * Macros to access fields of parse tree nodes
 */

#define TYPE(t)		t->n_type
#define LINE(t)		t->n_line
#define COL(t)		t->n_col
#define TREE0(t)	t->n_field[0].n_ptr
#define TREE1(t)	t->n_field[1].n_ptr
#define TREE2(t)	t->n_field[2].n_ptr
#define TREE3(t)	t->n_field[3].n_ptr
#define VAL0(t)		t->n_field[0].n_val
#define VAL1(t)		t->n_field[1].n_val
#define VAL2(t)		t->n_field[2].n_val
#define VAL3(t)		t->n_field[3].n_val
#define STR0(t)		t->n_field[0].n_str
#define STR1(t)		t->n_field[1].n_str
#define STR2(t)		t->n_field[2].n_str
#define STR3(t)		t->n_field[3].n_str

/*
 * External declarations
 */

extern nodeptr tree;		/* parse tree space */
extern nodeptr tfree;		/* free pointer for tree space */
extern nodeptr tend;		/* end of tree space */
extern int tsize;		/* parse tree size (integers) */
extern nodeptr tree1();         /* tree node allocator routines */
extern nodeptr tree3();
extern nodeptr tree4();
extern nodeptr tree5();
extern nodeptr tree6();
extern nodeptr yylval;		/* parser's current token value */

/*
 * Node Types
 */

#define N_ACTIVAT   1           /* activation control structure */
#define N_ALT       2           /* alternation operator */
#define N_AUGOP     3		/* augmented operator */
#define N_BAR       4           /* generator control structure */
#define N_BINOP     5           /* other binary operator */
#define N_BREAK     6           /* break statement */
#define N_CASE      7           /* case statement */
#define N_CCLS      8           /* case clause */
#define N_CLIST     9           /* list of case clauses */
#define N_CONJ     10           /* conjunction operator */
#define N_CREATE   11           /* create control structure */
#define N_CSET     12           /* cset literal */
#define N_ELIST    14           /* list of expressions */
#define N_EMPTY    15           /* empty expression or statement */
#define N_FIELD    16           /* record field reference */
#define N_ID       17           /* identifier token */
#define N_IF       18           /* if-then-else statement */
#define N_INT      19           /* integer literal */
#define N_INVOK    20           /* procedure call */
#define N_KEY      21           /* keyword */
#define N_LIMIT    22           /* LIMIT control structure */
#define N_LIST     23           /* [ ... ] style list */
#define N_LOOP     24           /* while, until, every, or repeat */
#define N_NOT      25           /* not prefix control structure */
#define N_NEXT     26           /* next statement */
#define N_OP       27           /* operator token */
#define N_PROC     28           /* procedure */
#define N_REAL     29           /* real literal */
#define N_RES      30           /* reserved word token */
#define N_RET      31           /* fail, return, or succeed */
#define N_SCAN     32           /* scan-using statement */
#define N_SECT	   33		/* s[i:j] (section) */
#define N_SLIST    34           /* list of statements */
#define N_STR      35           /* string literal */
#define N_SUSP     36           /* suspend statement */
#define N_TO       37           /* TO operator */
#define N_TOBY     38           /* TO-BY operator */
#define N_UNOP     39           /* unary operator */

/*
 * node constructor macros
 */

#define ACTIVNODE(a,b,c)        tree6(N_ACTIVAT,LINE(a),COL(a),a,b,c)
#define ALTNODE(a,b,c)          tree5(N_ALT,LINE(a),COL(a),b,c)
#define AUGOPNODE(a,b,c)	tree6(N_AUGOP,LINE(a),COL(a),a,b,c)
#define BARNODE(a)              tree4(N_BAR,LINE(a),COL(a),a)
#define BINOPNODE(a,b,c)        tree6(N_BINOP,LINE(a),COL(a),a,b,c)
#define BREAKNODE(a,b)          tree4(N_BREAK,LINE(a),COL(a),b)
#define CASENODE(a,b,c)         tree5(N_CASE,LINE(a),COL(a),b,c)
#define CCLSNODE(a,b,c)         tree5(N_CCLS,LINE(a),COL(a),b,c)
#define CLISTNODE(a,b,c)        tree5(N_CLIST,LINE(a),COL(a),b,c)
#define CONJNODE(a,b,c)         tree6(N_CONJ,LINE(a),COL(a),a,b,c)
#define CREATENODE(a,b)         tree5(N_CREATE,LINE(a),COL(a),b)
#define CSETNODE(a,b)           tree5(N_CSET,tline,tcol,a,b)
#define ELISTNODE(a,b,c)        tree5(N_ELIST,LINE(a),COL(a),b,c)
#define EMPTYNODE               tree1(N_EMPTY)
#define FIELDNODE(a,b,c)        tree5(N_FIELD,LINE(a),COL(a),b,c)
#define IDNODE(a)               tree4(N_ID,tline,tcol,a)
#define IFNODE(a,b,c,d)         tree6(N_IF,LINE(a),COL(a),b,c,d)
#define INTNODE(a)              tree4(N_INT,tline,tcol,a)
#define INVOKNODE(a,b,c)        tree5(N_INVOK,LINE(a),COL(a),b,c)
#define KEYNODE(a,b)            tree4(N_KEY,LINE(a),COL(a),b)
#define LIMITNODE(a,b)          tree5(N_LIMIT,LINE(a),COL(a),a,b)
#define LISTNODE(a,b)           tree5(N_LIST,LINE(a),COL(a),b)
#define LOOPNODE(a,b,c)         tree6(N_LOOP,LINE(a),COL(a),a,b,c)
#define NOTNODE(a)              tree4(N_NOT,LINE(a),COL(a),a)
#define NEXTNODE(a)             tree3(N_NEXT,LINE(a),COL(a))
#define OPNODE(a)               tree4(N_OP,tline,tcol,a)
#define PROCNODE(a,b,c,d)       tree7(N_PROC,LINE(a),COL(a),a,b,c,d)
#define REALNODE(a)             tree4(N_REAL,tline,tcol,a)
#define RESNODE(a)              tree4(N_RES,tline,tcol,a)
#define RETNODE(a,b)            tree5(N_RET,LINE(a),COL(a),a,b)
#define SCANNODE(a,b,c)         tree6(N_SCAN,LINE(a),COL(a),a,b,c)
#define SECTNODE(a,b,c,d)	tree7(N_SECT,LINE(a),COL(a),a,b,c,d)
#define SLISTNODE(a,b,c)        tree5(N_SLIST,LINE(a),COL(a),b,c)
#define STRNODE(a,b)            tree5(N_STR,tline,tcol,a,b)
#define SUSPNODE(a,b)           tree4(N_SUSP,LINE(a),COL(a),b)
#define TOBYNODE(a,b,c,d)       tree6(N_TOBY,LINE(a),COL(a),b,c,d)
#define TONODE(a,b,c)           tree5(N_TO,LINE(a),COL(a),b,c)
#define UNOPNODE(a,b)           tree5(N_UNOP,LINE(a),COL(a),a,b)
