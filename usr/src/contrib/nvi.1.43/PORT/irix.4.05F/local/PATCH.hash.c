*** hash.c.orig	Fri Jun 24 09:12:29 1994
--- hash.c	Mon Aug 15 16:10:47 1994
***************
*** 301,307 ****
--- 301,311 ----
  	if (file != NULL) {
  		if (stat(file, &statbuf))
  			return (NULL);
+ #ifndef sgi
  		hashp->BSIZE = statbuf.st_blksize;
+ #else
+ 		hashp->BSIZE = 4096;
+ #endif /* sgi */
  		hashp->BSHIFT = __log2(hashp->BSIZE);
  	}
  
