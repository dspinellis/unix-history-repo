/*
 * Descriptor flags, note that these must match the values in h/rt.h.
 */
#ifdef VAX
#define F_NQUAL		0x80000000	/* set if NOT string qualifier */
#define F_VAR		0x40000000	/* set if variable */
#define F_TVAR		0x20000000	/* set if trapped variable */
#define F_PTR		0x10000000	/* set if value field is pointer */
#define OFFSETMASK	  0x1fffffff	/* offset mask for variables */
#endif VAX

#ifdef PORT
#define F_NQUAL		    x		   /* set if NOT string qualifier */
#define F_VAR		    x		   /* set if variable */
#define F_TVAR		    x		   /* set if trapped variable */
#define F_PTR		    x		   /* set if value field is pointer */
#define OFFSETMASK	    x		   /* offset mask for variables */
#endif PORT

#ifdef PDP11
#define F_NQUAL	   0100000		/* set if NOT string qualifier */
#define F_VAR	   0040000		/* set if variable */
#define F_TVAR	   0020000		/* set if trapped variable */
#define F_PTR	   0010000		/* set if value field is pointer */
#define OFFSETMASK 0017777		/* offset mask for variables */
#endif PDP11

/*
 * Type codes (descriptors and blocks).
 */

#define T_INTEGER	 1              /* short integer (not put in heap) */
/*
 * For 32 bit machines, e.g. the Vax, LONGINT's and INTEGER's are
 *  the same.  It would be better to have a generic integer type, and
 *  also have LONGINT's and SHORTINT's, but at the current time,
 *  LONGINT is used both to refer to integers not representable by
 *  a short, and as a generic integer type.
 */
#ifdef LONGS
#define T_LONGINT	 2		/* long integer type */
#else LONGS
#define T_LONGINT	 1		/* long integer type */
#endif LONGS
#define T_REAL		 3		/* real number */
#define T_CSET		 4		/* cset */
#define T_FILE		 5		/* file block */
#define T_PROC		 6		/* procedure block */
#define T_LIST		 7		/* list header block */
#define T_TABLE		 8		/* table header block */
#define T_RECORD	 9		/* record block */
#define T_TELEM		10		/* table element block */
#define T_LELEM		11		/* list element block */
#define T_TVSUBS	12		/* substring trapped variable */
#define JUNK_13		13		/* (no longer used) */
#define T_TVTBL		14		/* table element trapped variable */
#define T_TVPOS		15		/* &pos trapped variable */
#define T_TVRAND	16		/* &random trapped variable */
#define T_TVTRACE	17		/* &trace trapped variable */
#define T_ESTACK	18		/* expression stack block */
#define T_EBLOCK	19		/* expression heap block */
#ifdef SETS
#define T_SET		20		/* set header block */
#define T_SELEM		21		/* set element block */

#define MAXTYPE		21		/* maximum type number */
#else SETS

#define MAXTYPE		19		/* maximum type number */
#endif SETS

/*
 * Descriptor types and flags.
 */

#define D_VAR		(F_VAR | F_NQUAL)
#define D_TVAR		(F_VAR | F_TVAR | F_NQUAL)

#define D_NULL		0
#define D_INTEGER	(T_INTEGER | F_NQUAL)
#define D_LONGINT	(T_LONGINT | F_PTR | F_NQUAL)
#define D_REAL		(T_REAL | F_PTR | F_NQUAL)
#define D_CSET		(T_CSET | F_PTR | F_NQUAL)
#define D_FILE		(T_FILE | F_PTR | F_NQUAL)
#define D_PROC		(T_PROC | F_PTR | F_NQUAL)
#define D_LIST		(T_LIST | F_PTR | F_NQUAL)
#define D_TABLE		(T_TABLE | F_PTR | F_NQUAL)
#define D_RECORD	(T_RECORD | F_PTR | F_NQUAL)
#define D_TELEM		(T_TELEM | F_PTR | F_NQUAL)
#define D_LELEM		(T_LELEM | F_PTR | F_NQUAL)
#define D_TVSUBS	(T_TVSUBS | D_TVAR)
#define D_TVTBL		(T_TVTBL | D_TVAR)
#define D_TVPOS		(T_TVPOS | D_TVAR)
#define D_TVRAND	(T_TVRAND | D_TVAR)
#define D_TVTRACE	(T_TVTRACE | D_TVAR)
#define D_ESTACK	(T_ESTACK | F_PTR | F_NQUAL)
#define D_EBLOCK	(T_EBLOCK | F_PTR | F_NQUAL)
#define D_SET		(T_SET | F_PTR | F_NQUAL)
#define D_SELEM		(T_SELEM | F_PTR | F_NQUAL)
