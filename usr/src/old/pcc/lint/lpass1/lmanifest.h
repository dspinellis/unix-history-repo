/*
 *	@(#)lmanifest.h	1.2	(Berkeley)	3/29/83
 */
/*	the key:
	LDI	defined and initialized: storage set aside
	LIB	defined on a library
	LDC	defined as a common region on UNIX
	LDX	defined by an extern: if ! pflag, same as LDI
	LRV	function returns a value
	LUV	function used in a value context
	LUE	function used in effects context
	LUM	mentioned somewhere other than at the declaration
	LST	defined as a static
	*/
# define LDI 0001
# define LIB 0002
# define LDC 0004
# define LDX 0010
# define LRV 0020
# define LUV 0040
# define LUE 0100
# define LUM 0200
# define LST 0400

# define LFN 01000  /* filename record */

	/* number of chars in NAME, and filename */
#ifndef FLEXNAMES
# define LCHNM 8
# define LFNM 14
#endif

typedef struct ty {
	TWORD aty;
	short extra;
	short extra1;
	} ATYPE;

#define X_NONAME 0x8000		/* for extra1, if structure has no name */

typedef struct line {
	short decflag;
#ifndef FLEXNAMES
	char name[LCHNM];
#else
	char *name;
#endif
	short nargs;
	short fline;
	ATYPE type;
	} LINE;

union rec {
	struct line l;
	struct {
		short decflag;
#ifndef FLEXNAMES
		char fn[LFNM];
#else
		char *fn;
#endif
		} f;
	};
