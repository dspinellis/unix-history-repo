/* sccs id @(#)config.h	35.1 5/6/81	*/

/* 
 * this file contains parameters which each site is likely to modify
 * to create (and describe) their own configuration.
 * The following names will be stored in the (status features) list.
 *
 */

#define OS      "unix"
#define MACHINE "vax"
#define SITE    "ucbvax"


/*  TTSIZ is the absolute limit, in pages, 
 * both text and data, of the
 * size to which the lisp system may grow.
 * If you change this, you must recompile
 * alloc.c and data.c
 */
#ifdef HOLE
#define TTSIZE 10216
#else
#define TTSIZE 6120
#endif


#ifdef VMS 
#undef TTSIZE
#define TTSIZE 10216
#define FREESIZE 512 * 10000
#endif 


