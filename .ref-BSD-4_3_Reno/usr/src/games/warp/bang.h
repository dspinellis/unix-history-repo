/* $Header: bang.h,v 7.0 86/10/08 15:12:03 lwall Exp $ */

/* $Log:	bang.h,v $
 * Revision 7.0  86/10/08  15:12:03  lwall
 * Split into separate files.  Added amoebas and pirates.
 * 
 */

EXT long blast[YSIZE][XSIZE];
EXT bool blasted;
EXT bool xblasted[XSIZE];
EXT bool yblasted[YSIZE];

EXT char bangy[YSIZE*XSIZE];
EXT char bangx[YSIZE*XSIZE];
EXT char bangs[YSIZE*XSIZE];

EXT long bangm[YSIZE*XSIZE];

#define MAXBDIST 40
EXT int xx[MAXBDIST];
EXT int yy[MAXBDIST];

EXT int nxtbang;
EXT bool banging;

void make_plink();
void make_blast();
void do_bangs();
void bang_init();
