
/* read in image header information - if `headskip' flag is non-zero, read
 * in that many words, leaving unaltered the input nrows and ncols
 */
#include <stdio.h>
#include <math.h>

#include "shimg.h"  /* flag definitions */

#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#define MAX(a,b) (((a) > (b)) ? (a) : (b))

#define HALFBOX 10

short *readheader(fd,headskip,nrows,ncols,fitsflag)
	int fd, headskip, fitsflag;
	int *nrows, *ncols;
{
	char *malloc(), field[80];
	short *header;
	int headlen = 1536;  /* standard SAO Nova CCD header length */
	int nbytes, rfitscard();

	if(headskip) headlen = headskip;
	if(fitsflag) headlen = FITSBUFLEN;

	if((header = (short *)malloc(headlen)) == NULL) {
		fprintf(stderr,"Can't malloc() memory for header!\n");
		exit(1);
	}
	if((nbytes = read(fd,header,headlen)) != headlen) {
		fprintf(stderr,"?? Only %d bytes in header?\n",nbytes);
		exit(1);
	}
    /* see if FITS decode necessary */
	if(fitsflag) {
	  rfitsheader(header,ncols,nrows);
          /* skip past furthur header records until END card */
	  while(rfitscard(header,"END     ",field,1,0) == 0)
	    read(fd,header,FITSBUFLEN);
	}
    /* pull the parameters from CCD style header */
	else if(headskip == 0) {
		*ncols = header[512+127];
		*nrows = header[512+126];
	}
	return(header);
}

rfitsheader(fitshead,ncols,nrows)
     char *fitshead;
     int *ncols, *nrows;
{
     int sscanf(), strlen(), strncmp(), strcmp(), atoi();
     int rfitscard(), naxis;
     char field[21];

     rfitscard(fitshead,"SIMPLE  ",field,0,1);
     if(sscanf(field,"%s",field) != 1) 
       fitserror(fitshead,"Malformed key field");
     if(strcmp(field,"T") != 0)
       fitserror(fitshead,"Only SIMPLE = T capability");

     rfitscard(fitshead+80,"BITPIX  ",field,0,1);
     if(atoi(field) != 16)
       fitserror(fitshead+80,"Only 16-bit images at this time");

     rfitscard(fitshead+160,"NAXIS   ",field,0,1);
     if((naxis = atoi(field)) < 2) 
       fitserror(fitshead+160,"NAXIS less than 2");
     else if(naxis > 2) {
       fprintf(stderr,"** WARNING ** Only first 2 axes will be read. ");
       fprintf(stderr,"(naxis read was %d)\n",naxis);
     }

     rfitscard(fitshead+240,"NAXIS1  ",field,0,1);
     if((*ncols = atoi(field)) <= 0)
       fitserror(fitshead+240,"NAXIS1 value error");

     rfitscard(fitshead+320,"NAXIS2  ",field,0,1);
     if((*nrows = atoi(field)) <= 0)
       fitserror(fitshead+240,"NAXIS2 value error");

}

rfitscard(cardbuf,keyword,keyfield,anywhere,fatal)
     char *cardbuf, *keyword, *keyfield;
     int anywhere;       /* if non-zero, search entire buffer for keyword */
     int fatal;          /* if non-zero, fatal if key not found */
{
     char errmsg[80];
     int sscanf();
     register int i;
     
     for(i=0; i<(anywhere ? FITSBUFLEN : 80); i+=80) {
       if(strncmp(cardbuf+i,keyword,8) == 0) 
	 return(sscanf(cardbuf+i+10,"%20c",keyfield));
     }
     if(fatal) {
       sprintf(errmsg,"No `%s' keyword",keyword);
       fitserror(cardbuf,errmsg);   /* fatal error exit */
     }
     return(0);
}

fitserror(card,message)
     char *card, *message;
{
     card[79] = 0;
     fprintf(stderr,"FITS format error: %s\ncard is:\n%s\n",message, card);
     exit(1);
}

short *readpict(fd,nrows,ncols,fitsflag)
	int fd, nrows, ncols, fitsflag;
{
	short *picture;
        char *malloc();
	register int nbytes = nrows * ncols * 2;
	register int nread = 0, ntotal = 0;
	register char *pict;

	if(fitsflag) nbytes =
	  ((nbytes/FITSBUFLEN) + ((nbytes % FITSBUFLEN) ? 1 : 0))
	    * FITSBUFLEN;
	if((pict = malloc(nbytes)) == NULL) {
		fprintf(stderr,"Can't allocate picture memory!\n");
		exit(1);
	}
	picture = (short *)pict;
	if(fitsflag) {   /* do successive reads until enough bytes */
	  while(ntotal < nbytes) {
	    if((nread = read(fd,pict,FITSBUFLEN)) != FITSBUFLEN) {
		 fprintf(stderr,"Bad record of %d bytes read?\n",nread);
		 exit(1);
	    }
#ifdef VAX
	    if(fitsflag == 1)  /* do not swap bytes if disk fits format */
			swab(pict,pict,nread);
#endif
	    ntotal += nread;
	    pict += nread;
	  }
	  /* read past EOF, if from tape */
	  if((pict = malloc(FITSBUFLEN)) != NULL) 
	     while(read(fd,pict,FITSBUFLEN) > 0) ;
	}
	else if((nbytes = read(fd,picture,nbytes)) != (nrows * ncols)<<1) {
		fprintf(stderr,"only %d bytes in picture?\n",nbytes);
		exit(1);
	}
	return(picture);
}

/* VERY SIMPLE 16-bit to n-bit scaling for now. We assume we know a
 * little something about the picture, so as get best contrast soonest.
 */

scalepict( byteimage, picture, pmaxval, pminval,
		ncolors, pixoffset, lshift, nrows, ncols, flags)
	unsigned char *byteimage;
	short *picture;
	int pmaxval, pminval, ncolors, pixoffset, lshift, nrows, ncols;
	unsigned short flags;
{
	register unsigned char *image = byteimage;
	register short *pict = picture;
	register unsigned char *lookup;
	register int npix;
	register int pint = pmaxval;
	register int pmin = pminval;

	int i,j;
	double xpinterval;
	char *malloc();

	if((lookup = (unsigned char *)malloc(65536)) == NULL) {
	    fprintf(stderr,"Can't allocate lookup table?\n");
	    exit(1);
	}

	for(npix = 0; npix <= pmin+32768;  ) 
	  lookup[npix++] = pixoffset;

	pmin = pixoffset+ncolors-1;
	for(npix = pint+32768; npix < 65536; )
	  lookup[npix++] = pmin;

	pmin = MAX(-50,pminval);  /* disallow large negative pixels */

	if(flags & SOP_Linear) {
	/* disallow large positive pixels here, too */
	  pint = MIN(1000+pmin,pint); 

	  pint = (pint - pmin)/ncolors;  /* reus as interval measure */
/* code added by egm */
	  pint = MAX(pint,1);
/* end of egm code */
	  ncolors--;
	  for(npix=pmin; npix <= pmaxval; npix++) 
		lookup[npix+32768] =
		    (MIN(ncolors,(npix-pmin)/pint)<<lshift) + pixoffset;
	} 
  /* a little work will generalize this to logarithmic mapping */
	else if(flags & SOP_Sqrt) {
	  xpinterval = sqrt((double)(pint - pmin))/(double)ncolors;
	  ncolors--;
	  for(npix=pmin; npix <= pmaxval; npix++) {
	     pint = (int)((sqrt((double)(npix-pmin))/xpinterval) + 0.5);
             lookup[npix+32768] = 
	           MIN(ncolors, pint<<lshift) + pixoffset;
	 }
	}
	else {
	  fprintf(stderr,"Unknown scaling type request!\n");
	  free(lookup);
	  exit(1);
	}
	npix = nrows*ncols;
	image = byteimage;
	pict = picture;
	while(npix--) *image++ = *(lookup + *pict++ + 32768);
	free(lookup);
	return;
}

/* return max, min of data in picture (approximately - sample areas likely
 * to be of interest.
 */
maxminpict( picture, nrows, ncols, pmaxval, pminval)
	short *picture;
	int nrows, ncols;
	int *pmaxval, *pminval;  /* RETURNED */

{
	register short *pict = picture;
	register int npix;
	register int pmax = -32768, pmin = 32767;
	register int i, j;
	
	j = ncols*nrows/2;
	for(i=ncols/8; i<(7*ncols)/8; i++) {
	    npix = pict[i+j];
	    pmax = MAX(pmax,npix);
	    pmin = MIN(pmin,npix);
	}
	i = (7*ncols*nrows)/8 + (ncols>>1);
	for(j=(nrows*ncols/8)+(ncols>>1); j<i; j+=ncols) {
	    npix = pict[j];
	    pmax = MAX(pmax,npix);
	    pmin = MIN(pmin,npix);
	}
	/* find min,max from 5 regions (center, 4 areas around it) */
	for(i=ncols/2-HALFBOX; i<ncols/2+HALFBOX; i++) {
	    for(j=nrows/8-HALFBOX; j<nrows/8+HALFBOX; j++) {
	        npix = pict[j*ncols+i];
		pmax = MAX(pmax,npix);
		pmin = MIN(pmin,npix);
	    }
	    for(j=nrows/2-HALFBOX; j<nrows/2+HALFBOX; j++) {
	        npix = pict[j*ncols+i];
		pmax = MAX(pmax,npix);
		pmin = MIN(pmin,npix);
	    }
	    for(j=7*nrows/8-HALFBOX; j<7*nrows/8+HALFBOX; j++) {
	        npix = pict[j*ncols+i];
		pmax = MAX(pmax,npix);
		pmin = MIN(pmin,npix);
	    }
	}
	for(j=nrows/2-HALFBOX; j<nrows/2+HALFBOX; j++) {
	    for(i=ncols/8-HALFBOX; i<ncols/8+HALFBOX; i++) {
	        npix = pict[j*ncols+i];
		pmax = MAX(pmax,npix);
		pmin = MIN(pmin,npix);
	    }
	    for(i=7*nrows/8-HALFBOX; i<7*ncols/8+HALFBOX; i++) {
	        npix = pict[j*ncols+i];
		pmax = MAX(pmax,npix);
		pmin = MIN(pmin,npix);
	    }
	}

/*	printf("pixel range is %d to %d\n",pmin,pmax);  */

	*pminval = pmin;
	*pmaxval = pmax;

	return;
}

/* print out a piece of the picture */
/* 
 * We really shoud use the environment to get the reverse video
 * escape sequence. Oh well....
 */
prpict(pict, wx, wy, xzero, yzero, ncols, nrows, npcol, nprow)
     short *pict;
     int wx, wy, xzero, yzero, ncols, nrows, npcol, nprow;
{
     int i, j, k, l;

     printf("\n\nRow %d, Col %d:\n\n       ", wy + yzero, wx + xzero);
     i = MIN( ncols-npcol, MAX( 0, wx + xzero - npcol/2));
     for(k=i; k<i+npcol; k++) 
         if(k == wx + xzero) printf("  %c[7m%4d%c[0m",27,k,27);
	 else printf("  %4d",k);
     printf("\n       ");
     for(k=i; k<i+npcol; k++) printf("  ----");
     j = MIN( nrows-nprow, MAX( 0, wy + yzero - nprow/2));
     for(k=j; k<j+nprow; k++) {
         if(k == wy + yzero) printf("\n %c[7m%4d%c[0m |",27,k,27);
	 else printf("\n%5d |",k);
         for(l=i; l<i+npcol; l++)
		if((l == wx + xzero) && (k == wy + yzero))
		   printf(" %c[7m%5d%c[0m",27,pict[ k*ncols + l], 27);
                else printf("%6d",pict[ k*ncols + l]);
     }
     printf("\n");
     return;
}
