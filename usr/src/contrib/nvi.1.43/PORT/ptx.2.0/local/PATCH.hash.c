*** hash.c.orig	1993/11/24 11:18:44
--- hash.c	1993/11/24 12:38:29
***************
*** 301,307 ****
--- 301,311 ----
  	if (file != NULL) {
  		if (stat(file, &statbuf))
  			return (NULL);
+ #ifdef STBLKSIZE_NOT_AVAILABLE
+ 		hashp->BSIZE = 4096;
+ #else
  		hashp->BSIZE = statbuf.st_blksize;
+ #endif
  		hashp->BSHIFT = __log2(hashp->BSIZE);
  	}
  
