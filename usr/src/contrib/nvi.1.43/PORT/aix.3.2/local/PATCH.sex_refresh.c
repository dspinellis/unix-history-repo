*** sex_refresh.c.orig	Fri May 27 14:27:38 1994
--- sex_refresh.c	Fri May 27 14:26:42 1994
***************
*** 56,61 ****
--- 56,69 ----
  #include "sex_screen.h"
  
  #ifndef SYSV_CURSES
+ #ifdef A_NORMAL
+ #undef A_NORMAL
+ #endif
+ 
+ #ifdef A_STANDOUT
+ #undef A_STANDOUT
+ #endif
+ 
  #define	A_NORMAL	1
  #define	A_STANDOUT	2
  #define	vidattr(attr)	Xvidattr(sp, attr)
