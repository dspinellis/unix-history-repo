*** vi/v_increment.c.orig	Sun Nov 20 12:54:56 1994
--- vi/v_increment.c	Fri Dec  2 20:15:45 1994
***************
*** 245,251 ****
  			}
  			ulval -= change;
  		}
! 		nlen = snprintf(nbuf, sizeof(nbuf), ntype, wlen, ulval);
  		/*
  		 * XXX
  		 * UNIX sprintf(3) functions lose the leading 0[Xx] if
--- 245,252 ----
  			}
  			ulval -= change;
  		}
! 		/* XXX: wlen - 2: SunOS printf(3) is broken, kluge it. */
! 		nlen = snprintf(nbuf, sizeof(nbuf), ntype, wlen - 2, ulval);
  		/*
  		 * XXX
  		 * UNIX sprintf(3) functions lose the leading 0[Xx] if
