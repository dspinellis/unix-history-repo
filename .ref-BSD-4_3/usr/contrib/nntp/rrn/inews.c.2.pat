*** inews.old.c	Tue Apr 30 14:34:19 1985
--- inews.c	Tue Apr 30 14:34:33 1985
***************
*** 416,421
  /*
   *	Link ARTICLE into dir for ngname and update active file.
   */
  localize(ngname)
  char	*ngname;
  {

--- 416,424 -----
  /*
   *	Link ARTICLE into dir for ngname and update active file.
   */
+ #ifdef DOXREFS
+ long
+ #endif
  localize(ngname)
  char	*ngname;
  {
***************
*** 453,458
  			mknewsg(cp, ngname);
  
  		sprintf(bfr, "%s/%ld", cp, ngsize+1);
  #ifdef VMS
  		if ((f2 = creat(bfr, 0666)) >=0 ) {
  			f1 = open(article, 0);

--- 456,466 -----
  			mknewsg(cp, ngname);
  
  		sprintf(bfr, "%s/%ld", cp, ngsize+1);
+ #ifdef LINKART
+ 		if (mylink(ARTICLE, bfr) == 0) break;
+ 				/* on first file inits ARTICLE, on subsequent */
+ 				/* files "links" to first article */
+ #else !LINKART
  #ifdef VMS
  		if ((f2 = creat(bfr, 0666)) >=0 ) {
  			f1 = open(article, 0);
***************
*** 468,473
  		if (link(ARTICLE, bfr) == 0)
  			break;
  #endif !VMS
  		e = errno;	/* keep log from clobbering it */
  		logerr("Cannot install article as %s", bfr);
  		if (e != EEXIST) {

--- 476,482 -----
  		if (link(ARTICLE, bfr) == 0)
  			break;
  #endif !VMS
+ #endif !LINKART
  		e = errno;	/* keep log from clobbering it */
  		logerr("Cannot install article as %s", bfr);
  		if (e != EEXIST) {
***************
*** 494,499
  		strcpy(firstbufname, bfr);
  	sprintf(bfr, "%s/%ld ", ngname, ngsize+1);
  	addhist(bfr);
  	return TRUE;
  }
  

--- 503,509 -----
  		strcpy(firstbufname, bfr);
  	sprintf(bfr, "%s/%ld ", ngname, ngsize+1);
  	addhist(bfr);
+ #ifndef DOXREFS
  	return TRUE;
  #else DOXREFS
  	return ngsize+1;
***************
*** 495,500
  	sprintf(bfr, "%s/%ld ", ngname, ngsize+1);
  	addhist(bfr);
  	return TRUE;
  }
  
  /*

--- 505,513 -----
  	addhist(bfr);
  #ifndef DOXREFS
  	return TRUE;
+ #else DOXREFS
+ 	return ngsize+1;
+ #endif DOXREFS
  }
  
  /*
***************
*** 507,512
  	char c;
  	struct srec srec;	/* struct for sys file lookup	*/
  	int is_invalid = FALSE;
  
  	/* Fill up the rest of header. */
  	if (mode != PROC) {

--- 520,529 -----
  	char c;
  	struct srec srec;	/* struct for sys file lookup	*/
  	int is_invalid = FALSE;
+ #ifdef DOXREFS
+ 	register char *nextxref = header.xref; 
+ 	int numxrefs = 0;
+ #endif DOXREFS
  
  	/* Fill up the rest of header. */
  	if (mode != PROC) {
***************
*** 527,532
  	if (!is_ctl && mode != CREATENG)
  		is_invalid = ngfcheck(mode == PROC);
  
  	/* Write article to temp file. */
  	tfp = xfopen(mktemp(ARTICLE), "w");
  	if ( (c=getc(infp)) == ' ' || c == '\t' ) {

--- 544,556 -----
  	if (!is_ctl && mode != CREATENG)
  		is_invalid = ngfcheck(mode == PROC);
  
+ #ifdef LINKART
+ 	*ARTICLE = '\0';	/* tell mylink() to snarf the name */
+ #else !LINKART
+ #ifdef DOXREFS
+ 	/* Open temp file for article, but link before writing */
+ 	tfp = xfopen(mktemp(ARTICLE), "w");
+ #else DOXREFS
  	/* Write article to temp file. */
  	tfp = xfopen(mktemp(ARTICLE), "w");
  	if ( (c=getc(infp)) == ' ' || c == '\t' ) {
***************
*** 545,550
  		putc('\n',tfp);
  	fclose(tfp);
  	fclose(infp);
  
  	if (is_invalid) {
  		logerr("No valid newsgroups found, moved to junk");

--- 569,576 -----
  		putc('\n',tfp);
  	fclose(tfp);
  	fclose(infp);
+ #endif DOXREFS
+ #endif LINKART
  
  	if (is_invalid) {
  		logerr("No valid newsgroups found, moved to junk");
***************
*** 550,555
  		logerr("No valid newsgroups found, moved to junk");
  		if (localize("junk"))
  			savehist(histline);
  		xxit(1);
  	}
  

--- 576,582 -----
  		logerr("No valid newsgroups found, moved to junk");
  		if (localize("junk"))
  			savehist(histline);
+ #ifndef DOXREFS
  		xxit(1);
  #endif
  	}
***************
*** 551,556
  		if (localize("junk"))
  			savehist(histline);
  		xxit(1);
  	}
  
  	if (time((time_t)0) > (cgtdate(header.subdate) + DFLTEXP) ){

--- 578,584 -----
  			savehist(histline);
  #ifndef DOXREFS
  		xxit(1);
+ #endif
  	}
  #ifdef DOXREFS
  	else
***************
*** 552,558
  			savehist(histline);
  		xxit(1);
  	}
! 
  	if (time((time_t)0) > (cgtdate(header.subdate) + DFLTEXP) ){
  		logerr("Article too old, moved to junk");
  		if (localize("junk"))

--- 580,588 -----
  		xxit(1);
  #endif
  	}
! #ifdef DOXREFS
! 	else
! #endif
  	if (time((time_t)0) > (cgtdate(header.subdate) + DFLTEXP) ){
  		logerr("Article too old, moved to junk");
  		if (localize("junk"))
***************
*** 557,562
  		logerr("Article too old, moved to junk");
  		if (localize("junk"))
  			savehist(histline);
  		xxit(1);
  	}
  

--- 587,593 -----
  		logerr("Article too old, moved to junk");
  		if (localize("junk"))
  			savehist(histline);
+ #ifndef DOXREFS
  		xxit(1);
  #endif
  	}
***************
*** 558,563
  		if (localize("junk"))
  			savehist(histline);
  		xxit(1);
  	}
  
  	if (is_ctl) {

--- 589,595 -----
  			savehist(histline);
  #ifndef DOXREFS
  		xxit(1);
+ #endif
  	}
  #ifdef DOXREFS
  	else
***************
*** 559,565
  			savehist(histline);
  		xxit(1);
  	}
! 
  	if (is_ctl) {
  		control(&header);
  		localize("control");

--- 591,599 -----
  		xxit(1);
  #endif
  	}
! #ifdef DOXREFS
! 	else
! #endif
  	if (is_ctl) {
  #ifndef DOXREFS
  		control(&header);
***************
*** 561,566
  	}
  
  	if (is_ctl) {
  		control(&header);
  		localize("control");
  	} else {

--- 595,601 -----
  	else
  #endif
  	if (is_ctl) {
+ #ifndef DOXREFS
  		control(&header);
  #endif
  		localize("control");
***************
*** 562,567
  
  	if (is_ctl) {
  		control(&header);
  		localize("control");
  	} else {
  		if (s_find(&srec, FULLSYSNAME) == FALSE)

--- 597,603 -----
  	if (is_ctl) {
  #ifndef DOXREFS
  		control(&header);
+ #endif
  		localize("control");
  	} else {
  		if (s_find(&srec, FULLSYSNAME) == FALSE)
***************
*** 566,571
  	} else {
  		if (s_find(&srec, FULLSYSNAME) == FALSE)
  			xerror("Cannot find my name '%s' in %s", FULLSYSNAME, SUBFILE);
  		for (ptr = nbuf; *ptr;) {
  			if (ngmatch(ptr, srec.s_nbuf) || index(ptr,'.') == NULL)
  				localize(ptr);

--- 602,611 -----
  	} else {
  		if (s_find(&srec, FULLSYSNAME) == FALSE)
  			xerror("Cannot find my name '%s' in %s", FULLSYSNAME, SUBFILE);
+ #ifdef DOXREFS
+ 		sprintf(nextxref,"%s ",FULLSYSNAME);
+ 		nextxref += strlen(nextxref);
+ #endif
  		for (ptr = nbuf; *ptr;) {
  #ifndef DOXREFS
  			if (ngmatch(ptr, srec.s_nbuf) || index(ptr,'.') == NULL)
***************
*** 567,572
  		if (s_find(&srec, FULLSYSNAME) == FALSE)
  			xerror("Cannot find my name '%s' in %s", FULLSYSNAME, SUBFILE);
  		for (ptr = nbuf; *ptr;) {
  			if (ngmatch(ptr, srec.s_nbuf) || index(ptr,'.') == NULL)
  				localize(ptr);
  			while (*ptr++)

--- 607,613 -----
  		nextxref += strlen(nextxref);
  #endif
  		for (ptr = nbuf; *ptr;) {
+ #ifndef DOXREFS
  			if (ngmatch(ptr, srec.s_nbuf) || index(ptr,'.') == NULL)
  				localize(ptr);
  #else DOXREFS
***************
*** 569,574
  		for (ptr = nbuf; *ptr;) {
  			if (ngmatch(ptr, srec.s_nbuf) || index(ptr,'.') == NULL)
  				localize(ptr);
  			while (*ptr++)
  				;
  		}

--- 610,624 -----
  #ifndef DOXREFS
  			if (ngmatch(ptr, srec.s_nbuf) || index(ptr,'.') == NULL)
  				localize(ptr);
+ #else DOXREFS
+ 			if (ngmatch(ptr, srec.s_nbuf) ||
+ 			    index(ptr,'.') == NULL) {
+ 				sprintf(nextxref,"%s:%ld ",ptr,localize(ptr));
+ 				numxrefs++;
+ 				while (*nextxref)
+ 				       nextxref++;
+ 			}
+ #endif DOXREFS
  			while (*ptr++)
  				;
  		}
***************
*** 577,582
  			localize("junk");
  		}
  	}
  
  	broadcast();
  	savehist(histline);

--- 627,638 -----
  			localize("junk");
  		}
  	}
+ #ifdef DOXREFS
+ 	if (numxrefs >= 2)
+ 	    *(nextxref-1) = '\0';       /* wipe out the last space */
+ 	else
+ 	    header.xref[0] = '\0';      /* wipe out the whole thing */
+ #endif
  
  #ifdef LINKART
  	tfp = xfopen(ARTICLE,"w");	/* open 1st article localized */
***************
*** 578,583
  		}
  	}
  
  	broadcast();
  	savehist(histline);
  	xxit(0);

--- 634,669 -----
  	    header.xref[0] = '\0';      /* wipe out the whole thing */
  #endif
  
+ #ifdef LINKART
+ 	tfp = xfopen(ARTICLE,"w");	/* open 1st article localized */
+ #endif
+ 
+ #if defined(LINKART) || defined(DOXREFS)
+ 	/* Now that xref is constructed, write article to temp file. */
+ 	/* (We ought to detect no room at this point and clean up.) */ 
+ 	if ( (c=getc(infp)) == ' ' || c == '\t' ) {
+ 		header.intnumlines++;
+ 		sprintf(header.numlines,"%d",header.intnumlines);
+ 	}
+ 	lhwrite(&header, tfp);
+ 	/* Kludge to get around article truncation problem */
+ 	if (c == ' ' || c == '\t' )
+ 		putc('\n', tfp);
+ 	putc(c,tfp);
+ 	while (fgets(bfr, BUFLEN, infp) != NULL)
+ 		fputs(bfr, tfp);
+ 
+ 	if (bfr[strlen(bfr)-1] != '\n')
+ 		putc('\n',tfp);
+ 	fclose(tfp);
+ 	fclose(infp);
+ #endif LINKART || DOXREFS
+ 
+ #ifdef DOXREFS
+ 	if (is_ctl)	/* moved here cuz checkgroups uses ARTICLE! */
+ 		control(&header);
+ #endif
+ 
  	broadcast();
  	savehist(histline);
  	xxit(0);
***************
*** 853,855
  	}
  	return(NULL);
  }

--- 939,963 -----
  	}
  	return(NULL);
  }
+ 
+ #ifdef LINKART
+ mylink(tmpart,linkfrom)
+ char *tmpart, *linkfrom;
+ {
+     struct stat statbuf;
+ 
+     if (stat(linkfrom,&statbuf)==0)
+ 	return -1;
+     if (!*tmpart)                       /* first article? */
+ 	strcpy(tmpart,linkfrom);        /* just remember name */
+     else {
+ 	FILE *linkfp = fopen(linkfrom,"w");
+ 
+ 	if (!linkfp)
+ 	    return -1;
+ 	fprintf(linkfp,"%s\n",tmpart);  /* do "symbolic link" */
+ 	fclose(linkfp);
+     }
+     return 0;
+ }
+ #endif LINKART
