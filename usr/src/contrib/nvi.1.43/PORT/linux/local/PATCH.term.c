*** common/term.c.orig	Wed Nov  2 17:35:55 1994
--- common/term.c	Wed Nov  2 17:35:50 1994
***************
*** 148,153 ****
--- 148,162 ----
  	 * 8-bit character set, as long as nul isn't a character.
  	 */
  	(void)setlocale(LC_ALL, "");
+ 	/*
+ 	 * In libc 4.5.26, setlocale(LC_ALL, ""), doesn't setup
+ 	 * table for ctype(3c) correctly.
+ 	 * This bug is fixed in libc 4.6.x.
+ 	 *
+ 	 * This code works around this problem for libc 4.5.x users.
+ 	 * Note that this code is harmless if you're using libc 4.6.x.
+ 	 */
+ 	(void)setlocale(LC_CTYPE, "");
  	key_init(sp);
  
  	gp = sp->gp;
