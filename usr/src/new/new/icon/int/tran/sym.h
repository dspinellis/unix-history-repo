#

/*
 * structures for symbol table entries
 */

struct lentry {                 /* local table entry */
   struct lentry *l_blink;      /*   link for bucket chain */
   char *l_name;                /*   name of variable */
   int l_flag;                  /*   variable flags */
   };

struct gentry {                 /* global table entry */
   struct gentry *g_blink;      /*   link for bucket chain */
   char *g_name;                /*   name of variable */
   int g_flag;                  /*   variable flags */
   int g_nargs;                 /*   number of args (procedure) or */
   };                           /*     number of fields (record) */

struct centry {                 /* constant table entry */
   struct centry *c_blink;      /*   link for bucket chain */
   char *c_name;                /*   pointer to string */
   int c_length;		/*   length of string */
   int c_flag;                  /*   type of literal flag */
   };

struct ientry {                 /* identifier table entry */
   struct ientry *i_blink;      /*   link for bucket chain */
   char *i_name;                /*   pointer to string */
   int i_length;		/*   length of string */
   };

/*
 * flag values
 */

#define F_GLOBAL      01        /* variable declared global externally */
#define F_PROC        04        /* procedure */
#define F_RECORD     010        /* record */
#define F_DYNAMIC    020        /* variable declared local dynamic */
#define F_STATIC     040        /* variable declared local static */
#define F_BUILTIN   0100        /* identifier refers to built-in procedure */
#define F_IMPERROR  0400        /* procedure has default error */
#define F_ARGUMENT 01000        /* variable is a formal parameter */
#define F_INTLIT   02000        /* literal is an integer */
#define F_REALLIT  04000        /* literal is a real */
#define F_STRLIT  010000        /* literal is a string */
#define F_CSETLIT 020000        /* literal is a cset */

/*
 * symbol table region pointers
 */

extern struct lentry **lhash;   /* hash area for local table */
extern struct gentry **ghash;   /* hash area for global table */
extern struct centry **chash;   /* hash area for constant table */
extern struct ientry **ihash;   /* hash area for identifier table */

extern struct lentry *ltable;   /* local table */
extern struct gentry *gtable;   /* global table */
extern struct centry *ctable;   /* constant table */
extern struct ientry *itable;   /* identifier table */

extern struct lentry *lfree;    /* free pointer for local table */
extern struct gentry *gfree;    /* free pointer for global table */
extern struct centry *ctfree;   /* free pointer for constant table */
extern struct ientry *ifree;    /* free pointer for identifier table */

extern int lsize;               /* initial size of local table */
extern int gsize;               /* initial size of global table */
extern int csize;               /* initial size of constant table */
extern int isize;               /* initial size of identifier table */
extern int ihsize;              /* initial size of identifier hash table */
extern int lhsize;              /* initial size of local hash tables */
extern int ghsize;              /* initial size of global hash tables */
extern int chsize;              /* initial size of constant hash tables */
extern int lmask;               /* mask for local table hash */
extern int gmask;               /* mask for global table hash */
extern int cmask;               /* mask for constant table hash */
extern int imask;               /* mask for identifier table hash */

/*
 * symbol table parameters
 */

#define LSIZE    100            /* default size of local table */
#define GSIZE    100            /* default size of global table */
#define CSIZE    100            /* default size of constant table */
#define ISIZE    500            /* default size of identifier table */
#define LHSIZE   128            /* default size of local hash table */
#define GHSIZE   128            /* default size of global hash table */
#define CHSIZE   128            /* default size of constant hash table */
#define IHSIZE   128            /* default size of identifier hash table */
#ifdef PDP11
#define TSIZE   5000            /* default size of parse tree space */
#define SSIZE   5000            /* default size of string space */
#endif PDP11
#ifdef VAX
#define TSIZE  15000            /* default size of parse tree space */
#define SSIZE  15000            /* default size of string space */
#endif VAX

/*
 * structure for keyword table
 */

struct keyent {
      char *keyname;
      int keyid;
      };

extern struct keyent keytab[];  /* keyword table */

/*
 * miscellaneous
 */

#define ghasher(x) (((int)x)&gmask)  /* hash function for global symbol table */
#define lhasher(x) (((int)x)&lmask)  /* hash function for local symbol table */
#define chasher(x) (((int)x)&cmask)  /* hash function for constant symbol table */
