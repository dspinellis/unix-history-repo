*** bt_open.c.orig	Thu Sep  1 07:18:03 1994
--- bt_open.c	Wed Nov  2 16:49:57 1994
***************
*** 259,265 ****
--- 259,269 ----
  		 * Don't overflow the page offset type.
  		 */
  		if (b.psize == 0) {
+ #ifdef STBLKSIZE_NOT_AVAILABLE
+ 			b.psize = 512;
+ #else
  			b.psize = sb.st_blksize;
+ #endif
  			if (b.psize < MINPSIZE)
  				b.psize = MINPSIZE;
  			if (b.psize > MAX_PAGE_OFFSET + 1)
