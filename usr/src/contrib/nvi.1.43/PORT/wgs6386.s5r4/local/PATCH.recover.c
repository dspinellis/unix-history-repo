*** recover.c.orig	Thu Jul 21 17:33:25 1994
--- recover.c	Wed Aug  3 19:35:16 1994
***************
*** 47,53 ****
--- 47,57 ----
   */
  #include <sys/file.h>
  
+ #if 0
  #include <netdb.h>		/* MAXHOSTNAMELEN on some systems. */
+ #else
+ #define MAXHOSTNAMELEN 256
+ #endif
  
  #include <bitstring.h>
  #include <dirent.h>
