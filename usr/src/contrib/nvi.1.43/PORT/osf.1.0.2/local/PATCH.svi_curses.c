*** svi_curses.c.orig	Thu Jul 21 15:49:29 1994
--- svi_curses.c	Thu Jul 21 15:49:25 1994
***************
*** 170,175 ****
--- 170,184 ----
  #endif
  		t.c_cc[VQUIT] = _POSIX_VDISABLE;
  		t.c_cc[VSUSP] = _POSIX_VDISABLE;
+ 
+ 		/*
+ 		 * XXX
+ 		 * OSF1 doesn't turn off the <literal-next>, <discard> or
+ 		 * <status> characters when curses switches into raw mode.
+ 		 */
+ 		t.c_cc[VDISCARD] = _POSIX_VDISABLE;
+ 		t.c_cc[VLNEXT] = _POSIX_VDISABLE;
+ 		t.c_cc[VSTATUS] = _POSIX_VDISABLE;
  
  		(void)tcsetattr(STDIN_FILENO, TCSASOFT | TCSADRAIN, &t);
  	}
