# include "CON.h"
# include "FL.h"
/*		*/
# define BLKSIZ 512  /*  tape block size */
int count , sector , isector , icount ;
char *bufptr , input[128] , *ibufptr ;
int wo = 0 ;
/*		*/
main() {
/*
*  Stand-alone program to copy VAX LSI RX11 floppy diskette to
*  memory and back to floppy.
*  Floppy sector no.'s start at 1 for controller - start at 0
*	for user input.
*  Floppy track no.'s start at 0.
*/
register int kk ;

putlin("flpcpy : Floppy-to-Memory-to-Floppy Copy") ;
putnl() ;
 
doff :
putstr("floppy sector offset : ") ;
getcon(input) ;
sector = a2l(input) ;
if ((sector < 0) || (sector >= MAXSEC)) goto doff ;
 
gknt :
putstr("no. of input sectors : ") ;
getcon(input) ;
count = a2l(input) ;
if (count < 0) goto gknt ;
if (count == 0) count = MAXSEC ;
 
if (init()) {
	putlin("init error") ;
	return(-1) ;
	}
 
isector = sector ;
icount = count ;
 
if (wo) goto wflop ;
while (count>0) {
if ((count%100) == 0)  {
	kk = count/100 ;
	l2a(kk,input) ;
	putstr(input) ;
	putstr(" ") ;
	}
	if (flio(FL_RS)) { /* read */
		putlin("floppy input error") ;
		return(-1) ;
		}
	}
 
putnl() ;
putlin("floppy read complete") ;
 
wflop :
putlin("mount new floppy - type any key when ready") ;
getcon(input) ;
 
count = icount ;
sector = isector ;
bufptr = ibufptr ;
 
while (count > 0) {
if ((count%100) == 0)  {
	kk = count/100 ;
	l2a(kk,input) ;
	putstr(input) ;
	putstr(" ") ;
	}
	if (flio(FL_WS)) {
		putlin("floppy output error") ;
		return(-1) ;
		}
	}
putnl() ;
putlin("floppy write complete") ;
 
end :
return(0) ;
}
 
/*		*/
 
init() {
/*
*  Initialization.
*/
extern char *end ;
 
ibufptr = bufptr = (char *)(((int)&end+511) & 017777777000) ;
return(0) ;
}
 
/*		*/
 
flio(func)
int func ;
{
/*
*  Function to read/write 1 sector from floppy disc.
*  'sector' is sector no.-1 to read into 'input[]'.
*  Return (-1) for error, else return (0) .
*/
register int j , s , t ;
register unsigned int c ;
 
/* compute start track & sector from current sector 'sector'. */
t = sector/RXSTRK ; /* track no. */
s = sector%RXSTRK + 1 ; /* sector */
sector++ ;
 
 
fltwait() ;
mtpr(TXDB,func) ; /* Floppy Read/Write Sector command */
fltwait() ;
mtpr(TXDB,s|FL_DATA) ; /* supply sector no. to floppy interface */
fltwait() ;
mtpr(TXDB,t|FL_DATA) ; /* track no. */
 
if (func == FL_RS) { /* Read Sector */
	/* wait for read to complete */
	if (fldone()) return(-1) ;
	/* loop to read sector bytes from interface */
	for (j = 0 ; j < RXBYSEC ; j++) {
		flrwait() ; /* wait till ready */
		(*bufptr++) = mfpr(RXDB) ; /*get data byte-assume from floppy*/
		}
	}
else {
	if (func == FL_WS) { /* Write Sector */
		for (j = 0 ; j < RXBYSEC ; j++) {
			/* send byte over interface */
			fltwait() ;
			c = (*bufptr++) & 0xff ;
			mtpr(TXDB,(c|FL_DATA)) ;
			}
		fltwait() ;
		if (fldone()) return(-1) ;
		}
	}
count-- ;
return(0) ;
}
