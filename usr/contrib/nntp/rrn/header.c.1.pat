NOTE: the preceding context may be different under 2.10.2, but you should
be able to figure it out anyway.

*** header.old.c	Fri Apr 27 11:30:49 1984
--- header.c	Mon Feb 27 10:44:03 1984
***************
*** 107,112
  #define NUMLINES	19
  #define KEYWORDS	20
  #define APPROVED	21
  #define OTHER		99
  
  char *malloc();

--- 107,115 -----
  #define NUMLINES	19
  #define KEYWORDS	20
  #define APPROVED	21
+ #ifdef DOXREFS
+ #define XREF		98
+ #endif DOXREFS
  #define OTHER		99
  
  char *malloc();
***************
*** 201,206
  				seenrelay = 1;
  			}
  			break;
  		case OTHER:
  			if (unreccnt < NUNREC) {
  				hp->unrec[unreccnt] = malloc(strlen(bfr) + 1);

--- 204,214 -----
  				seenrelay = 1;
  			}
  			break;
+ #ifdef DOXREFS
+ 		case XREF:
+ 			getfield(hp->xref);
+ 			break;
+ #endif DOXREFS
  		case OTHER:
  			if (unreccnt < NUNREC) {
  				hp->unrec[unreccnt] = malloc(strlen(bfr) + 1);
***************
*** 398,403
  		return KEYWORDS;
  	if (its("Approved: "))
  		return APPROVED;
  	return OTHER;
  }
  

--- 406,415 -----
  		return KEYWORDS;
  	if (its("Approved: "))
  		return APPROVED;
+ #ifdef DOXREFS
+ 	if (its("Xref: "))
+ 		return XREF;
+ #endif DOXREFS
  	return OTHER;
  }
  
***************
*** 507,512
  		fprintf(fp, "Keywords: %s\n", hp->keywords);
  	if (*hp->approved)
  		fprintf(fp, "Approved: %s\n", hp->approved);
  	for (iu = 0; iu < NUNREC; iu++) {
  		if (hp->unrec[iu])
  			fprintf(fp, "%s", &hp->unrec[iu][0]);

--- 519,528 -----
  		fprintf(fp, "Keywords: %s\n", hp->keywords);
  	if (*hp->approved)
  		fprintf(fp, "Approved: %s\n", hp->approved);
+ #ifdef DOXREFS
+ 	if (wr == 1 && *hp->xref)
+ 		fprintf(fp, "Xref: %s\n", hp->xref);
+ #endif DOXREFS
  	for (iu = 0; iu < NUNREC; iu++) {
  		if (hp->unrec[iu])
  			fprintf(fp, "%s", &hp->unrec[iu][0]);
