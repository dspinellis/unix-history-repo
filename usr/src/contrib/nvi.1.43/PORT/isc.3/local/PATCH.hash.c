*** PORT/db/hash/hash.c.orig	Sat Jun 25 01:12:29 1994
--- PORT/db/hash/hash.c	Wed Nov  2 16:51:38 1994
***************
*** 301,307 ****
--- 301,311 ----
  	if (file != NULL) {
  		if (stat(file, &statbuf))
  			return (NULL);
+ #ifdef STBLKSIZE_NOT_AVAILABLE
+ 		hashp->BSIZE = 512;
+ #else
  		hashp->BSIZE = statbuf.st_blksize;
+ #endif
  		hashp->BSHIFT = __log2(hashp->BSIZE);
  	}
  
