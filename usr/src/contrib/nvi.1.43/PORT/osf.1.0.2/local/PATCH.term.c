*** term.c.orig	Wed Nov  9 22:11:22 1994
--- term.c	Wed Nov  9 22:11:20 1994
***************
*** 53,59 ****
  #include <unistd.h>
  
  #include "compat.h"
! #include <curses.h>
  #include <db.h>
  #include <regex.h>
  
--- 53,63 ----
  #include <unistd.h>
  
  #include "compat.h"
! /*
!  * XXX
!  * DON'T INCLUDE <curses.h> HERE, IT BREAKS OSF1 V2.0 WHERE IT
!  * CHANGES THE VALUES OF VERASE/VKILL/VWERASE TO INCORRECT ONES.
!  */
  #include <db.h>
  #include <regex.h>
  
