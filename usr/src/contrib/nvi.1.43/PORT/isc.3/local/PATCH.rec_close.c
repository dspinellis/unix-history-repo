*** PORT/db/recno/rec_close.c.orig	Fri Aug 19 01:23:29 1994
--- PORT/db/recno/rec_close.c	Wed Nov  2 17:02:42 1994
***************
*** 175,180 ****
--- 179,187 ----
  		return (RET_ERROR);
  	if ((off = lseek(t->bt_rfd, (off_t)0, SEEK_CUR)) == -1)
  		return (RET_ERROR);
+ #ifdef FTRUNCATE_NOT_AVAILABLE
+ # define ftruncate chsize
+ #endif
  	if (ftruncate(t->bt_rfd, off))
  		return (RET_ERROR);
  	F_CLR(t, R_MODIFIED);
