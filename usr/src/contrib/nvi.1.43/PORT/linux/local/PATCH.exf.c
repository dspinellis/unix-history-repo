*** exf.c.orig	Mon Aug 15 14:01:57 1994
--- exf.c	Mon Aug 15 14:18:31 1994
***************
*** 601,606 ****
--- 601,620 ----
  		return (1);
  	}
  
+ #if __linux__
+ 	/*
+ 	 * In libc 4.5.x, fdopen(fd, "w") clears the O_APPEND flag (if set).
+ 	 * This bug is fixed in libc 4.6.x.
+ 	 *
+ 	 * This code works around this problem for libc 4.5.x users.
+ 	 * Note that this code is harmless if you're using libc 4.6.x.
+ 	 */
+ 	if (LF_ISSET(FS_APPEND) && lseek(fd, (off_t)0, SEEK_END) < 0) {
+ 		msgq(sp, M_SYSERR, name);
+ 		return (1);
+ 	}
+ #endif
+ 
  	/* Use stdio for buffering. */
  	if ((fp = fdopen(fd, "w")) == NULL) {
  		(void)close(fd);
