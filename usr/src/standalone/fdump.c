# include "CON.h"
# include "FL.h"
/*		*/
char input[130]  ;  /*  term input buffer */
int count , dskoff  , error ;
char *bufptr ;
/*		*/
main() {
/*
*  Stand-alone program to dump RX11 floppy  disk to VAX LSI console
*  printer in hex format.
*  Use specifies start sector(from 0) and no. of sectors.
*/

putlin("fdump : floppy-to-console hex dump") ;
putnl() ;
 
doff :
putstr("start 128-byte block no. : ") ;
getcon(input) ;
dskoff = a2l(input) ;
if (dskoff < 0) goto fini ;
if (dskoff > MAXSEC-1) goto doff ;
 
gknt :
putstr("no. blocks : ") ;
getcon(input) ;
count = a2l(input) ;
if (count < 0) goto gknt ;
if (count == 0) count = 1 ;
 
error = 0 ;
 
if (init()) {
	putlin("init error") ;
	return(-1) ;
	}
 
putlin("                      HI  < - -  LO") ;
putnl() ;
 
while ((error == 0) && (count--)) {
	if (flrs(dskoff,bufptr)) {
		putlin("floppy i/o error") ;
			ioerr :
				error++ ;
				continue ;
			}
	if (prsec()) {
		goto ioerr ;
		}
	putnl() ;
	putnl() ;
	dskoff++ ; /* next block */
}
 
goto doff ;
 
fini :
return(0) ;
}
 
/*		*/
 
init()
{
extern char *end ;

bufptr = (char *)(((int)&end+511) & 017777777000) ;
return(0) ;
}
 
/*		*/
 
prsec()
{
/*
*  Print 128 bytes on VAX LSI console as hex
*  characters.
*  Translate bytes in 'bufptr[]' to hex char's
*  and out to console, 64 char's per line.
*  (32 bytes per line)
*/
register int i , j ;
int k , addr ;
char c , *hp , *fr ;
char tmp[258] , ltmp[65] ;
 
ltmp[64] = '\0' ;
hp = "block # \0\0\0\0\0\0\0" ;
l2a(dskoff-1,&hp[8]) ;
putlin(hp) ;
putnl() ;
 
hxcnvt(bufptr,128,tmp) ; /* convert bytes to hex char's */
 
for (i = 0,addr = 0 ; (i < 256); i+=64,addr +=64 ) {
	hp = ltmp ;
	fr = (&tmp[i+63]) ;
	for ( k = 32 ; k ; k--) {
		(*hp++) = *(fr-1) ;
		(*hp++) = (*fr--) ;
		fr-- ;
		}
	putstr(ltmp) ;
	l2x(addr,input) ;
	putstr("  : ") ;
	putlin(input) ;
	}
return(0) ;
}
 
/*		*/
 
hxcnvt(in,knt,out)
register char *in , *out ;
int knt ;
{
/*
*  Convert 'knt' bytes in char array 'in' to 'knt*2'
*  hex char's and store in char array 'out'.
*/
register unsigned int bit4 , byte ;
 
byte = 0 ;
while (knt--) {
	byte = (*in++) ;
	bit4 = (byte>>4) & 017 ;
	(*out++) = (bit4<10?bit4+0x30:bit4+0x57) ;
	bit4 = byte & 017 ;
	(*out++) = (bit4<10?bit4+0x30:bit4+0x57) ;
	}
return(0) ;
}
