*** header.old.h	Tue Apr 30 14:33:33 1985
--- header.h		Tue Apr 30 14:33:35 1985
***************
*** 35,39
  	char	approved[BUFLEN];	/* Approved:		*/
  	char	nf_id[BUFLEN];		/* Nf-ID:		*/
  	char	nf_from[BUFLEN];	/* Nf-From:		*/
  	char	*unrec[NUNREC];		/* unrecognized lines	*/
  };

--- 35,42 -----
  	char	approved[BUFLEN];	/* Approved:		*/
  	char	nf_id[BUFLEN];		/* Nf-ID:		*/
  	char	nf_from[BUFLEN];	/* Nf-From:		*/
+ #ifdef DOXREFS
+ 	char	xref[BUFLEN];		/* Xref:		*/
+ #endif DOXREFS
  	char	*unrec[NUNREC];		/* unrecognized lines	*/
  };
