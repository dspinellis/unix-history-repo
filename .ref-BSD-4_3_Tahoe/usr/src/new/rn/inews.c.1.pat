*** inews.c.1.std	Tue Oct  2 16:09:59 1984
--- inews.c.1	Fri Sep 21 14:50:49 1984
***************
*** 483,488
  /*
   *	Link ARTICLE into dir for ngname and update active file.
   */
  localize(ngname)
  char	*ngname;
  {

--- 483,491 -----
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
*** 515,520
  	}
  	for (;;) {
  		sprintf(bfr, "%s/%ld", dirname(ngname), ngsize+1);
  		if (link(ARTICLE, bfr) == 0) break;
  		e = errno;	/* keep log from clobbering it */
  		fprintf(stderr, "Cannot install article as %s\n", bfr);

--- 518,528 -----
  	}
  	for (;;) {
  		sprintf(bfr, "%s/%ld", dirname(ngname), ngsize+1);
+ #ifdef LINKART
+ 		if (mylink(ARTICLE, bfr) == 0) break;
+ 				/* on first file inits ARTICLE, on subsequent */
+ 				/* files "links" to first article */
+ #else
  		if (link(ARTICLE, bfr) == 0) break;
  #endif
  		e = errno;	/* keep log from clobbering it */
***************
*** 516,521
  	for (;;) {
  		sprintf(bfr, "%s/%ld", dirname(ngname), ngsize+1);
  		if (link(ARTICLE, bfr) == 0) break;
  		e = errno;	/* keep log from clobbering it */
  		fprintf(stderr, "Cannot install article as %s\n", bfr);
  		log("Cannot install article as %s", bfr);

--- 524,530 -----
  				/* files "links" to first article */
  #else
  		if (link(ARTICLE, bfr) == 0) break;
+ #endif
  		e = errno;	/* keep log from clobbering it */
  		fprintf(stderr, "Cannot install article as %s\n", bfr);
  		log("Cannot install article as %s", bfr);
***************
*** 542,547
  		strcpy(firstbufname, bfr);
  	sprintf(bfr, "%s/%ld ", ngname, ngsize+1);
  	addhist(bfr);
  	return TRUE;
  }
  

--- 551,557 -----
  		strcpy(firstbufname, bfr);
  	sprintf(bfr, "%s/%ld ", ngname, ngsize+1);
  	addhist(bfr);
+ #ifndef DOXREFS
  	return TRUE;
  #else DOXREFS
  	return ngsize+1;
***************
*** 543,548
  	sprintf(bfr, "%s/%ld ", ngname, ngsize+1);
  	addhist(bfr);
  	return TRUE;
  }
  
  /*

--- 553,561 -----
  	addhist(bfr);
  #ifndef DOXREFS
  	return TRUE;
+ #else DOXREFS
+ 	return ngsize+1;
+ #endif DOXREFS
  }
  
  /*
***************
*** 553,558
  	register char *ptr;
  	register FILE *tfp;
  	int badgroup = 0, goodgroup = 0;
  
  	/* Fill up the rest of header. */
  	if (mode != PROC) {

--- 566,574 -----
  	register char *ptr;
  	register FILE *tfp;
  	int badgroup = 0, goodgroup = 0;
+ #ifdef DOXREFS
+ 	register char *nextxref = header.xref; 
+ #endif DOXREFS
  
  	/* Fill up the rest of header. */
  	if (mode != PROC) {
***************
*** 565,570
  	if (mode==PROC)
  		log("from %s relay %s", header.from, header.relayversion);
  
  	/* Write article to temp file. */
  	tfp = xfopen(mktemp(ARTICLE), "w");
  	lhwrite(&header, tfp);

--- 581,593 -----
  	if (mode==PROC)
  		log("from %s relay %s", header.from, header.relayversion);
  
+ #ifdef LINKART
+ 	*ARTICLE = '\0';	/* tell mylink() to snarf the name */
+ #else !LINKART
+ #ifdef DOXREFS
+ 	/* Open temp file for article, but link before writing */
+ 	tfp = xfopen(mktemp(ARTICLE), "w");
+ #else DOXREFS
  	/* Write article to temp file. */
  	tfp = xfopen(mktemp(ARTICLE), "w");
  	lhwrite(&header, tfp);
***************
*** 577,582
  	}
  	fclose(tfp);
  	fclose(infp);
  
  	if (is_ctl) {
  		control(&header);

--- 600,607 -----
  	}
  	fclose(tfp);
  	fclose(infp);
+ #endif DOXREFS
+ #endif LINKART
  
  	if (is_ctl) {
  		control(&header);
***************
*** 593,598
  			}
  		}
  	} else {
  		for (ptr = nbuf; *ptr;) {
  			if (*ptr == '-') {
  				while (*ptr++)

--- 618,627 -----
  			}
  		}
  	} else {
+ #ifdef DOXREFS
+ 		sprintf(nextxref,"%s ",SYSNAME);
+ 		nextxref += strlen(nextxref);
+ #endif
  		for (ptr = nbuf; *ptr;) {
  			if (*ptr == '-') {
  				while (*ptr++)
***************
*** 610,615
  			}
  			else
  				goodgroup++;
  			if (*nbuf)
  				localize(ptr);
  			while (*ptr++)

--- 639,645 -----
  			}
  			else
  				goodgroup++;
+ #ifndef DOXREFS
  			if (*nbuf)
  				localize(ptr);
  #else DOXREFS
***************
*** 612,617
  				goodgroup++;
  			if (*nbuf)
  				localize(ptr);
  			while (*ptr++)
  				;
  		}

--- 642,653 -----
  #ifndef DOXREFS
  			if (*nbuf)
  				localize(ptr);
+ #else DOXREFS
+ 			if (*nbuf)
+ 				sprintf(nextxref,"%s:%ld ",ptr,localize(ptr));
+ 			while (*nextxref)
+ 				nextxref++;
+ #endif DOXREFS
  			while (*ptr++)
  				;
  		}
***************
*** 616,621
  				;
  		}
  	}
  
  #ifdef NOFORWARD
  	if (*nbuf)

--- 652,663 -----
  				;
  		}
  	}
+ #ifdef DOXREFS
+ 	if (goodgroup < 2)
+ 	    header.xref[0] = '\0';
+ 	else
+ 	    *(nextxref-1) = '\0';
+ #endif
  
  #ifdef LINKART
  	tfp = xfopen(ARTICLE,"w");	/* open 1st article localized */
***************
*** 617,622
  		}
  	}
  
  #ifdef NOFORWARD
  	if (*nbuf)
  #endif

--- 659,683 -----
  	    *(nextxref-1) = '\0';
  #endif
  
+ #ifdef LINKART
+ 	tfp = xfopen(ARTICLE,"w");	/* open 1st article localized */
+ #endif
+ 
+ #if defined(LINKART) || defined(DOXREFS)
+ 	/* Now that xref is constructed, write article to temp file. */
+ 	/* (We ought to detect no room at this point and clean up.) */ 
+ 	lhwrite(&header, tfp);
+ 	while (fgets(bfr, BUFLEN, infp) != NULL) {
+ 		/*
+ 		if (!strncmp(bfr, "From ", 5))
+ 			putc('>', tfp);
+ 		*/
+ 		fputs(bfr, tfp);
+ 	}
+ 	fclose(tfp);
+ 	fclose(infp);
+ #endif LINKART || DOXREFS
+ 
  #ifdef NOFORWARD
  	if (*nbuf)
  #endif
***************
*** 861,863
  		mclose(fd);
  	}
  }

--- 922,946 -----
  		mclose(fd);
  	}
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
+     if (!*tmpart)
+ 	strcpy(tmpart,linkfrom);
+     else {
+ 	FILE *linkfp = fopen(linkfrom,"w");
+ 
+ 	if (!linkfp)
+ 	    return -1;
+ 	fprintf(linkfp,"%s\n",tmpart);
+ 	fclose(linkfp);
+     }
+     return 0;
+ }
+ #endif LINKART
