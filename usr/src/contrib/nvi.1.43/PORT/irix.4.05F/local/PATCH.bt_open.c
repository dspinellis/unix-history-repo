*** bt_open.c.orig	Thu Jun 16 18:11:00 1994
--- bt_open.c	Mon Aug 15 16:10:47 1994
***************
*** 255,261 ****
--- 255,265 ----
  		 * Don't overflow the page offset type.
  		 */
  		if (b.psize == 0) {
+ #ifndef sgi
  			b.psize = sb.st_blksize;
+ #else
+ 			b.psize = 4096;
+ #endif /* sgi */
  			if (b.psize < MINPSIZE)
  				b.psize = MINPSIZE;
  			if (b.psize > MAX_PAGE_OFFSET + 1)
