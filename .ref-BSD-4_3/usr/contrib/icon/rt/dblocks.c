#include "../h/rt.h"

/*
 * Heap block size table (sizes given in bytes).  A size of -1 is used
 *  for types that have no heap blocks; a size of 0 indicates that the
 *  second word of the block contains the size; a value greater than
 *  0 is used for types with constant sized blocks.
 */

int bsizes[] = {
    -1,				/* 0, not used */
    -1,				/* 1, not used */
     sizeof(struct b_int),	/* T_LONGINT (2), long integer type */
     sizeof(struct b_real),	/* T_REAL (3), real number */
     sizeof(struct b_cset),	/* T_CSET (4), cset */
     sizeof(struct b_file),	/* T_FILE (5), file block */
     0,				/* T_PROC (6), procedure block */
     sizeof(struct b_list),	/* T_LIST (7), list header block */
     sizeof(struct b_table),	/* T_TABLE (8), table header block */
     0,				/* T_RECORD (9), record block */
     sizeof(struct b_telem),	/* T_TELEM (10), table element block */
     0,				/* T_LELEM (11), list element block */
     sizeof(struct b_tvsubs),	/* T_TVSUBS (12), substring trapped variable */
    -1,				/* 13, not used */
     sizeof(struct b_tvtbl),	/* T_TVTBL (14), table element trapped variable */
    -1,				/* T_TVPOS (15), &pos trapped variable */
    -1,				/* T_TVRAND (16), &random trapped variable */
    -1,				/* T_TVTACE (17), &trace trapped variable */
    -1,				/* T_ESTACK (18), expression stack header */
     0				/* T_EBLOCK (19), expression heap block */
#ifdef SETS
    ,
     sizeof(struct b_set),	/* T_SET  (20), set header block */
     sizeof(struct b_selem)	/* T_SELEM  (21), set element block */
#endif SETS
    };

/*
 * Table of offsets (in bytes) to first descriptor in blocks.  -1 is for
 *  non-heap types, 0 for blocks with no descriptors.
 */
int firstd[] = {
    -1,				/* 0, not used */
    -1,				/* 1, not used */
     0,				/* T_LONGINT (2), long integer type */
     0,				/* T_REAL (3), real number */
     0,				/* T_CSET (4), cset */
     3*WORDSIZE,		/* T_FILE (5), file block */
     7*WORDSIZE,		/* T_PROC (6), procedure block */
     2*WORDSIZE,		/* T_LIST (7), list header block */
     2*WORDSIZE,		/* T_TABLE (8), table header block */
     3*WORDSIZE,		/* T_RECORD (9), record block */
     2*WORDSIZE,		/* T_TELEM (10), table element block */
     5*WORDSIZE,		/* T_LELEM (11), list element block */
     3*WORDSIZE,		/* T_TVSUBS (12), substring trapped variable */
    -1,				/* 13, not used */
     2*WORDSIZE,		/* T_TVTBL (14), table element trapped variable */
    -1,				/* T_TVPOS (15), &pos trapped variable */
    -1,				/* T_TVRAND (16), &random trapped variable*/
    -1,				/* T_TVTACE (17), &trace trapped variable */
    -1,				/* T_ESTACK (18), expression stack header */
     5*WORDSIZE			/* T_EBLOCK (19), expression heap block */
#ifdef SETS
   ,
     2*WORDSIZE,		/* T_SET  (20), set header block */
     2*WORDSIZE			/* T_SELEM  (21), set element block */
#endif SETS
    };

/*
 * Table of block names used by debugging functions.
 */
char *blkname[] = {
   "illegal name",		/* 0, not used */
   "illegal name",	 	/* 1, not used */
   "long integer",		/* T_LONGINT (2), long integer type */
   "real number",		/* T_REAL (3), real number */
   "cset",			/* T_CSET (4), cset */
   "file",			/* T_FILE (5), file block */
   "procedure",			/* T_PROC (6), procedure block */
   "list header block",		/* T_LIST (7), list header block */
   "table header block",	/* T_TABLE  (8), table header block */
   "record",			/* T_RECORD (9), record block */
   "table element block",	/* T_TELEM (10), table element block  */
   "list element block",	/* T_LELEM (11), list element block */
   "substring tv",		/* T_TVSUBS (12), substring trapped variable */
   "illegal name",		/* 13, not used */
   "table element tv",		/* T_TVTBL (14), table element trapped variable */
   "illegal name",		/* T_TVPOS (15), &pos trapped variable */
   "illegal name",		/* T_TVRAND (16), &random trapped variable */
   "illegal name",		/* T_TVTACE (17), &trace trapped variable */
   "expression stack",	 	/* T_ESTACK (18), expression stack header */
   "refresh block"		/* T_EBLOCK (19), expression heap block */
#ifdef SETS
   ,
   "set header block",		/* T_SET  (20), set header block */
   "set member"			/* T_SELEM  (21), set element block */
#endif SETS
   };
