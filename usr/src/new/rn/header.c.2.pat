*** header.old.c	Tue Apr 30 14:33:53 1985
--- header.c		Tue Apr 30 14:33:56 1985
***************
*** 119,124
  #define APPROVED	21
  #define NFID		22
  #define NFFROM		23
  #define OTHER		99
  
  char *malloc();

--- 119,127 -----
  #define APPROVED	21
  #define NFID		22
  #define NFFROM		23
+ #ifdef DOXREFS
+ #define XREF		98
+ #endif DOXREFS
  #define OTHER		99
  
  char *malloc();
***************
*** 214,219
  				getfield(hp->relayversion);
  			}
  			break;
  		case OTHER:
  			if (unreccnt < NUNREC) {
  				if ((hp->unrec[unreccnt] = malloc((unsigned)(strlen(bfr) + 1))) != NULL ) {

--- 217,227 -----
  				getfield(hp->relayversion);
  			}
  			break;
+ #ifdef DOXREFS
+ 		case XREF:
+ 			getfield(hp->xref);
+ 			break;
+ #endif DOXREFS
  		case OTHER:
  			if (unreccnt < NUNREC) {
  				if ((hp->unrec[unreccnt] = malloc((unsigned)(strlen(bfr) + 1))) != NULL ) {
***************
*** 422,427
  		return NFID;
  	if (its("Nf-From: "))
  		return NFFROM;
  	return OTHER;
  }
  

--- 430,439 -----
  		return NFID;
  	if (its("Nf-From: "))
  		return NFFROM;
+ #ifdef DOXREFS
+ 	if (its("Xref: "))
+ 		return XREF;
+ #endif DOXREFS
  	return OTHER;
  }
  
***************
*** 536,541
  		fprintf(fp, "Nf-ID: %s\n", hp->nf_id);
  	if (*hp->nf_from)
  		fprintf(fp, "Nf-From: %s\n", hp->nf_from);
  	for (iu = 0; iu < NUNREC; iu++) {
  		if (hp->unrec[iu])
  			fprintf(fp, "%s", &hp->unrec[iu][0]);

--- 548,557 -----
  		fprintf(fp, "Nf-ID: %s\n", hp->nf_id);
  	if (*hp->nf_from)
  		fprintf(fp, "Nf-From: %s\n", hp->nf_from);
+ #ifdef DOXREFS
+ 	if (wr == 1 && *hp->xref)
+ 		fprintf(fp, "Xref: %s\n", hp->xref);
+ #endif DOXREFS
  	for (iu = 0; iu < NUNREC; iu++) {
  		if (hp->unrec[iu])
  			fprintf(fp, "%s", &hp->unrec[iu][0]);
