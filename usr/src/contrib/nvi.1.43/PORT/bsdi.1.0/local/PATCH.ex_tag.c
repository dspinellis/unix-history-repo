*** ex_tag.c.orig	Fri Aug 27 16:49:08 1993
--- ex_tag.c	Fri Aug 27 16:50:05 1993
***************
*** 409,415 ****
  	 * large.
  	 */
  	if (fstat(fd, &sb) || (map = mmap(NULL, (size_t)sb.st_size,
! 	    PROT_READ, MAP_PRIVATE, fd, (off_t)0)) == (caddr_t)-1) {
  		(void)close(fd);
  		return (1);
  	}
--- 409,415 ----
  	 * large.
  	 */
  	if (fstat(fd, &sb) || (map = mmap(NULL, (size_t)sb.st_size,
! 	    PROT_READ, MAP_FILE | MAP_PRIVATE, fd, (off_t)0)) == (caddr_t)-1) {
  		(void)close(fd);
  		return (1);
  	}
