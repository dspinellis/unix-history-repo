*** hash.h.orig	1993/11/24 11:20:03
--- hash.h	1993/11/24 12:38:38
***************
*** 261,266 ****
--- 261,267 ----
  #define	REAL_KEY	4
  
  /* Short hands for accessing structure */
+ #undef BSIZE
  #define BSIZE		hdr.bsize
  #define BSHIFT		hdr.bshift
  #define DSIZE		hdr.dsize
