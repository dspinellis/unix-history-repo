NOTE: the preceding context may be different under 2.10.2, but you should
be able to figure it out anyway.

*** header.old.h	Fri Apr 27 11:30:49 1984
--- header.h	Thu May 10 15:19:55 1984
***************
*** 34,38
  	int	intnumlines;		/* Integer version	*/
  	char	keywords[BUFLEN];	/* Keywords:		*/
  	char	approved[BUFLEN];	/* Approved:		*/
  	char	*unrec[NUNREC];		/* unrecognized lines	*/
  };

--- 34,41 -----
  	int	intnumlines;		/* Integer version	*/
  	char	keywords[BUFLEN];	/* Keywords:		*/
  	char	approved[BUFLEN];	/* Approved:		*/
+ #ifdef DOXREFS
+ 	char	xref[BUFLEN];		/* Xref:		*/
+ #endif
  	char	*unrec[NUNREC];		/* unrecognized lines	*/
  };
