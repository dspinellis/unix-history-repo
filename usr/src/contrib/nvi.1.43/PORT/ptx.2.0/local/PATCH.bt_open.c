*** bt_open.c.orig	1993/11/24 11:16:51
--- bt_open.c	1993/11/24 12:38:12
***************
*** 256,262 ****
--- 256,266 ----
  		 * Don't overflow the page offset type.
  		 */
  		if (b.psize == 0) {
+ #ifdef STBLKSIZE_NOT_AVAILABLE
+ 			b.psize = 4096;
+ #else
  			b.psize = sb.st_blksize;
+ #endif
  			if (b.psize < MINPSIZE)
  				b.psize = MINPSIZE;
  			if (b.psize > MAX_PAGE_OFFSET + 1)
