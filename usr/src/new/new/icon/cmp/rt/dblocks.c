#include "../h/rt.h"

/*
 * heap block size table (sizes given in bytes)
 */

int bsizes[] = {
    -1,                        /*               0  not used */
    -1,                        /*               1  not used */
     sizeof(struct b_int),     /*  T_LONGINT    2  long integer type */
     sizeof(struct b_real),    /*  T_REAL       3  real number */
     sizeof(struct b_cset),    /*  T_CSET       4  cset */
     sizeof(struct b_file),    /*  T_FILE       5  file block */
     0,                        /*  T_PROCEDURE  6  procedure block */
     sizeof(struct b_list),    /*  T_LIST       7  list header */
     sizeof(struct b_table),   /*  T_TABLE      8  table header */
     0,                        /*  T_RECORD     9  record block */
     sizeof(struct b_telem),   /*  T_TELMT     10  table element */
     0,                        /*  T_LISTB     11  list element block */
     sizeof(struct b_tvsubs),  /*  T_TVSUBS    12  substring trapped variable */
    -1,                        /*              13  no used */
     sizeof(struct b_tvtbl),   /*  T_TVTBL     14  table element t.v. */
    -1,			       /*  T_POS       15  &pos trapped variable */
    -1,			       /*  T_RANDOM    16  &random trapped variable */
    -1,			       /*  T_TRACE     17  &trace trapped variable */
    -1,                        /*  T_ESTACK    18  expression stack header */
     0,                        /*  T_EBLOCK    19  expression heap block */
   };

/*
 * table of offsets to first descriptor in heap blocks (0-based, in bytes)
 */
int firstd[] = {
    -1,                 /*               0  not used */
    -1,                 /*               1  not used */
     0,				/*  T_LONGINT    2  long integer type */
     0,                 	/*  T_REAL       3  real number */
     0,                 	/*  T_CSET       4  cset */
     3*ADDRSIZE,		/*  T_FILE       5  file block */
     7*ADDRSIZE,		/*  T_PROCEDURE  6  procedure block */
     2*ADDRSIZE,		/*  T_LIST       7  list header */
     2*ADDRSIZE,		/*  T_TABLE      8  table header */
     3*ADDRSIZE,		/*  T_RECORD     9  record block */
     1*ADDRSIZE,		/*  T_TELMT     10  table element */
     5*ADDRSIZE,		/*  T_LISTB     11  list element block */
     3*ADDRSIZE,		/*  T_TVSUBS    12  substring trapped variable */
    -1,				/*              13  not used */
     1*ADDRSIZE,		/*  T_TVTBL     14  table element t.v. */
    -1,				/*  T_POS       15  &pos trapped variable */
    -1,				/*  T_RANDOM    16  &random trapped variable*/
    -1,				/*  T_TRACE     17  &trace trapped variable */
    -1,				/*  T_ESTACK    18  expression stack header */
     5*ADDRSIZE			/*  T_EBLOCK    19  expression heap block */
   };

char *blkname[] = {
   "illegal name",             /*               0  not used */
   "illegal name",             /*               1  not used */
   "long integer",             /*  T_LONGINT    2  long integer type */
   "real number",              /*  T_REAL       3  real number */
   "cset",                     /*  T_CSET       4  cset */
   "file",                     /*  T_FILE       5  file block */
   "procedure",                /*  T_PROCEDURE  6  procedure block */
   "list header",              /*  T_LIST       7  list header */
   "table header",             /*  T_TABLE      8  table header */
   "record",                   /*  T_RECORD     9  record block */
   "table entry",              /*  T_TELMT     10  table element */
   "list element",             /*  T_LISTB     11  list element block */
   "substring tv",	       /*  T_TVSUBS    12  substring trapped variable */
   "illegal name",	       /*              13  not used */
   "table element tv",	       /*  T_TVTBL     14  table element t.v. */
   "illegal name",	       /*  T_POS       15  &pos trapped variable */
   "illegal name",	       /*  T_RANDOM    16  &random trapped variable */
   "illegal name",	       /*  T_TRACE     17  &trace trapped variable */
   "expression stack",         /*  T_ESTACK    18  expression stack header */
   "refresh block"             /*  T_EBLOCK    19  expression heap block */
   };
