#include "MBA.h"
#include "RP.h"
#include "TM.h"
# include "CON.h"
# define BLKMAX  32768
# define MIN(a,b) (a<b?a:b)
#define MAXTER 10  /*  max. no. consec. tape errors */
/*		*/
int blksiz , tfoff , tboff , dboff , tunit , dens ;
int count , *tmMBA , *rpMBA , dblks , wsize ;
unsigned int ins , outs ;
unsigned int nread  , bytoff , error ;
int *RPptr , *TMptr ;
int rpcyl,rptrk,rpsec;
char input[150] , *bufptr , retry ;
/*		*/
main() {
register int i ;
/*
*  Stand-alone program to copy TM[23]/TE16 mag tape to
*  RP06/RM03 disk.
*  User specifies tape and disk MBA no.'s, tape and disk unit no.'s,
*  tape block size, tape file offset, tape block offset in file,
*  and disk sector offset.
*/

putlin("tdcopy : TM03 tape-to-disk copy") ;
putnl() ;
 
tmbno :
count = getval("tape MBA #") ;
if ((count < 0) || (count > MAXMBA-1)) goto tmbno ;
tmMBA = TMptr = (int *)(M_BASE + (count*NEXSPC)) ; /* TM02 MBA addr */
 
tuni :
tunit = getval("tape unit #") ;
if ((tunit < 0) || (tunit > 7)) goto tuni ;
TMptr = (int *)((int)TMptr + (tunit*EXTSIZ) + M_extern) ; /*  ptr. to MBA
		ext. reg set for this TM02 unit */
 
/*
*  ignore density prompt for tape read - TM02 recognizes and adjusts
*  for NRZ or PE input (800 or 1600 BPI)
gden :
dens = getval("tape density(8 or 16)") ;
if (dens == 0) dens = TM_D800 ;
else if ((dens != 8)&&(dens!=16)) goto gden ;
*/
dens = (dens==16?TM_D1600:TM_D800) ;
 
toff :
tfoff = getval("tape file offset") ;
if (tfoff < 0) goto toff ;
 
gtbo :
tboff = getval("tape block offset") ;
if (tboff < 0) goto gtbo ;
putnl() ;
 
gdm :
count = getval("disk MBA #") ;
if ((count < 0) || (count > MAXMBA-1)) goto gdm ;
rpMBA = RPptr = (int *)(M_BASE + (count*NEXSPC)) ; /* RP06 MBA addr */
 
dun :
count = getval("disk unit") ;
if ((count > 7) || (count < 0)) goto dun ;
RPptr = (int *)((int)RPptr +  (count*EXTSIZ) + M_extern) ; /* ptr to MBA ext reg set for
			this RP06 unit */
 
doff :
dboff = getval("disk block offset") ;
putnl() ;
 
gknt :
count = getval("no. of input blocks") ;
if (count < 0) goto gknt ;
 
if (init()) {
	putlin("init() error") ;
	return(-1) ;
	}
 
if ((dboff < 0) || (dboff > MAXSEC-1)) goto doff ;
 
if (tapfil(TMptr,tfoff)) {
	tioerr :
	putlin("tape positioning error") ;
	return(-1) ;
	}
if (tapfsp(TMptr,tboff)) goto tioerr ;
 
/* read in 1st block of tape file to determine block size */
nread = 1 ;
blksiz = 65536 ;
for (retry=0,i=10,error=0;i;i--) {
	if ((tread()&TM_FCE) != TM_FCE) {
		if (++error > MAXTER)
			goto tioerr;
		}
	else break ;
	}
retry++;
blksiz = (*(TMptr+TM_fc)) & 0xffff ;
pdstr(" = tape block size",blksiz) ;
nread = M_BCMAX/blksiz ; /* no. of tape reads to fill input buffer */
wsize = nread * blksiz ; /* no. bytes each write to disc */
dblks = wsize/512 ; /* no. of disc blocks on each write */
*(TMptr+TM_cs1) = TM_DCLR | TM_GO ;
twait(TMptr) ;
if (tapbsp(TMptr,1)) goto tioerr ;
putnl() ;
 
for (error = 0 ; (error == 0) && (count > 0) ; count -= nread ) {
	if (i=tread(TMptr)) {
		putlin("tape i/o error") ;
		TME_print(i) ;
		goto ioerr ;
		}
	ins += nread ;  /*  count of tape blocks input */
	if (i=dwrite(RPptr)) {
		putlin("disk i/o error") ;
		RPE_print(i) ;
		ioerr :
			error++ ;
			continue ;
		}
	outs += dblks ;  /*  count of disk blocks output */
}
 
fini :
/*  no rewind on normal termination */
if (error == 0) putlin("normal termination") ;
else taprew(TMptr) ;
pdstr(" input blocks read",ins) ;
pdstr(" output blocks written",outs) ;
return(0) ;
}
 
/*		*/
 
init() {
/*
*  Initialization.
*  Initialize MBA's and tape/disk units.
*  Initialize TM2/TE16 for drive 0 , 800 BPI, PDP11, etc.
*  Set up MBA  map registers to map a max.
*    transfer of 'M_BCMAX' bytes.
*/
register int *mp0 , *mp1 , i , page ;
extern char *end ;
 
*(rpMBA+M_cr) = MBAinit ; /* init. RP06 MBA */
if (rpMBA != tmMBA)
	*(tmMBA+M_cr) = MBAinit ;
if ((*(RPptr+RP_sr) & RP_MOL) == 0) {
	putlin("disk unit not online") ;
	return(-1) ;
	}
 
*(RPptr+RP_cr) = RP_RIP | RP_GO ; /* preset */
dwait(RPptr) ;
if (i=derror(RPptr)) {
	putlin("disk init error") ;
	RPE_print(i) ;
	return(-1) ;
	}
*(RPptr+RP_off) = RP_FMT ; /* set format bit */
 
i = *(RPptr+RP_dt)&0777 ;  /* get disk type */
if (i==RP6typ) { /* RP06 */
	rpsec=RP6SEC; rpcyl=RP6CYL; rptrk=RP6TRK;
	}
else {
	if (i==RM3typ) {
		rpsec=RM3SEC; rpcyl=RM3CYL; rptrk=RM3TRK;
		}
	else return(-1);
	}
 
/* set TM03 unit no., density, PDP11 normal mode, odd parity
	abort on error */
*(TMptr+TM_tc) = tunit | dens | TM_PNOR | TM_EABO ;
*(TMptr+TM_cs1) = TM_DCLR | TM_GO ;
twait(TMptr) ;
if (i=terror(TMptr)) {
	tierr :
	putlin("tape init error") ;
	TME_print(i) ;
	return(-1) ;
	}
if ((*(TMptr+TM_ds) & TM_MOL) == 0) {
	putlin("tape unit not online") ;
	return(-1) ;
	}
*(TMptr+TM_cs1) = TM_RWND | TM_GO ;
twait(TMptr) ;
if (i=terror(TMptr)) goto tierr ;
 
bufptr = (char *)((((int)&end)+511)&017777777000) ;
page = ((int)((int)bufptr>>9)&017777777) | 0x80000000 ; /* SBI page no. */
mp0 = (int *)((int)tmMBA + (int)(M_map*4)) ;
mp1 = (int *)((int)rpMBA + (int)(M_map*4)) ;
for (i = ((M_BCMAX+511)/512)+1 ; i ; i--,page++) {
	(*mp0++) = page ;
	(*mp1++) = page ;
	}
return(0) ;
}
 
/*		*/
 
tread()
{
/*
*  Function to read TM2/TE16 tape drive, 'blksiz' bytes each
*  read, and 'nread' reads.
*/
register int i, j , k , *t , *m , teknt ;
 
m = tmMBA ;
t = TMptr ;
for (i=MIN(nread,count),j=0,teknt=0 ; i ; i--,j++) {
	trloop :
	*(m+M_var) = j * blksiz ;
	*(m+M_bc) = (-blksiz) ; /* MBA byte count reg */
	*(t+TM_cs1) = TM_REDF | TM_GO ; /* read forward */
	twait(t) ; /* wait for ready */
	if (k=terror(t)) {
		if ((++teknt > MAXTER) || (retry == 0)) return(k);
		*(t+TM_cs1) = TM_DCLR | TM_GO ;
		twait(t);
		if (k=tapbsp(t,1)) return(k);
		goto trloop;
		}
	}
return(0) ; /* normal return */
}
 
/*		*/
 
dwrite()
{
/*
*  Function to write 'wsize' bytes to disc
*  from buffer '*bufptr'.
*/
register int i , j , *m , *d ;
 
m = rpMBA ;
d = RPptr ;
*(d+RP_cr) = RP_DC | RP_GO ; /* drive clear */
dwait(d) ;
*(d+RP_cyl) = dboff/RP6ST ; /* cylinder no. */
i = dboff%RP6ST ;
j = (i/rpsec)<<8 ; /* track */
*(d+RP_stk) = j | (i%rpsec) ; /* sector : track */
*(m+M_bc) = (count<nread?-(count*512):(-wsize)) ; /* byte count */
*(m+M_var) = 0 ; /* virt addr reg = map no. + byte off */
*(d+RP_cr) = RP_WR | RP_GO ;
dwait(d) ;
if (i=derror(d)) return(i) ; /* error */
dboff += dblks ;  /*  point to next disc sector */
return(0) ; /* normal return */
}
