# include "MBA.h"
# include "RP.h"
# include "CON.h"
# define LOW(i)((short)i)
# define HIGH(i)(i>>16)
# define BLKSIZ 512
# define BUFSIZ ((rpsec*BLKSIZ) + (rpsec*hdsiz))
# define MAXUNI  7 /* max. no. units on MBA */
# define MAXERR  20
# define MB_ERR  0x3080  /*  normal error bits  */
# define HDcoff 0
# define HDtoff  3
int track , cylndr , dunit , ecount , daterr , wcerr ;
int badsec , hdsiz , dtype , bsize , bufsiz;
char input[132] ;
int *RPptr ; /* ptr to start of RP reg's for desired drive */
int *Mptr ; /* ptr to MBA reg set */
int rptrk,rpcyl,rpsec;
char wcrflg ;
char *ibuf , *obuf ; /*  buffer pointers */
int Offset[] = {  /*  Centerline offsets  */
	0 , 0x08 , 0x88 , 0x10 , 0x90 , 0x18 , 0x98 , -1
	} ;
 
main() {
/*
*  Stand-alone program to format RP06/RM03 disk.
*  User specifies disk unit.
*/
register int bb;

putlin("format : Format RP06/RM03 Disk") ;
putnl() ;
 
gmba :
putstr("MBA no. : ") ;
getcon(input) ;
dunit = a2l(input) ;
if (dunit < 0) goto fini ;
if (dunit > MAXMBA) goto gmba ;
Mptr = RPptr =  (int *)(M_BASE + (dunit*NEXSPC)) ;
 
dun :
putstr("unit : ");
getcon(input) ;
dunit = a2l(input) ;
if (dunit < 0) goto fini ;
if (dunit > MAXUNI) goto dun ;
 
if (init()) {
	putlin("init error") ;
	goto dun ;
	}
 
bb = bsize = BUFSIZ;
for (cylndr = 0 ; cylndr < rpcyl ; cylndr++) {
	for (track = 0 ; track < rptrk ; track++) {
		if (format(0,bb,0)) return(1) ;
		if (wcheck(0,bb,0)) return(1) ;
		trkinc() ;
		}
	cylinc() ;
	trkclr() ;
	}
 
putnl() ;
putstr("# Bad Sectors : ") ;
l2x(badsec,input) ;
putlin(input) ;
putstr("# Write Check errors : ") ;
l2x(wcerr,input) ;
putlin(input) ;
putstr("# Data Check errors : ") ;
l2x(daterr,input) ;
putlin(input) ;
putstr("# Other errors : ") ;
l2x(ecount,input) ;
putlin(input) ;
putnl() ;
 
goto dun ;
 
fini :
return(0) ;
}
 
/*		*/
 
init() {
/*
*  Initialization.
*  Initialize MBA (disk) .
*  Set up MBA  map register to map a 
*    transfer of 'BUFSIZ' bytes.
*/
extern char *end ;
register int *mp0 , i , page;
register short *shp;
short flg;
 
*(Mptr+M_cr) = MBAinit ; /* MBA init */
/*  start of RP reg's for drive */
/*  get ptr. to RP ext reg set */
RPptr = (int *)((int)RPptr + (dunit*EXTSIZ) + M_extern) ;
if (((page = (*(RPptr+RP_sr))) & RP_MOL) == 0) {
	putlin("unit not online") ;
	return(-1) ;
	}
if (page & RP_WRL) {
	putlin("unit write-protected") ;
	return(1) ;
	}
 
*(RPptr+RP_cr) = RP_RIP | RP_GO ; /* drive preset - sets vv */
dwait(RPptr) ;
*(RPptr+RP_off) = RP_FMT | RP_ECI ; /* set format and ECC inhibit */
 
i = *(RPptr+RP_dt)&0777 ;  /* get disk type */
if (i==RP6typ) { /* RP06 */
	rpsec=RP6SEC; rpcyl=RP6CYL; rptrk=RP6TRK;
	hdsiz = RP6HD;
	dtype = RP6typ;
	}
else {
	if (i==RM3typ) {
		rpsec=RM3SEC; rpcyl=RM3CYL; rptrk=RM3TRK;
		hdsiz = RM3HD;
		dtype = RM3typ;
		}
	else return(-1);
	}
 
/*  output buffer */
obuf = (char *)((((int)&end)+511)&017777777000) ;
/*  input buffer */
ibuf = (char *)(((int)obuf + BUFSIZ + 511) & 017777777000) ;
/*  output buffer uses 1st 128 map reg's -
  input buffer uses last 128 map reg's */
page = (int)((int)obuf>>9) & 017777777 ;
page |= 0x80000000;
mp0 = (int *)((int)Mptr + (int)(M_map*4)) ;
for (i = 0 ; i < 128 ; i++)
	(*mp0++) = page++ ;
page = (int)((int)ibuf>>9) & 017777777 ;
page |= 0x80000000;
for (i = 0 ; i<128 ; i++ )
	(*mp0++) = page++ ;
 
/*  initialize output buffer with sector header and data :
	4 sector header words :
		cylinder
		track/sector
		key word 1 (not on RM03)
		key word 2 (not on RM03)
	256 data words
*/
flg = (dtype==RM3typ?RM_BSB:0);
for (shp = (short *)obuf , i = 0 ; i<rpsec ; i++) {
	*(shp++) = RP_FMT|flg ;
	*(shp++) = i;  /*  sect/trk  */
	if (dtype == RP6typ) {
		*(shp++) = 0;
		*(shp++) = 0;
		}
	for (page = 0 ; page < 256 ; page++)
		(*shp++) = page;
	}

ecount = daterr = wcerr = badsec = 0 ;
bufsiz=BLKSIZ;
return(0) ;
}
 
/*		*/
 
rshd(baddr,nb,s)
int baddr , nb , s ;
{
/*
*  Function to read 'nb' bytes into buffer pointed to
*    by map and offset in 'baddr' - start at sector 's'
*/
register int i , j ;
register int *M , *R ;
 
R = RPptr ;
M = Mptr ;
 
*(R+RP_cyl) = cylndr ; /* cylinder no. */
*(R+RP_stk) = (track<<8) | s ; /* track:sector */
*(M+M_bc) = (-nb) ;
*(M+M_var) = baddr ; /* virt addr reg = map no. + byte off */
rini :
*(R+RP_cr) = RP_DC | RP_GO ; /*  RP06 drive clear function code */
dwait(R) ;
*(R+RP_cr) = RP_RHD | RP_GO ; /* read sector header and data */
 
dwait(R) ; /* wait for i/o to finish */
if (i = mbaerr(M)) {
	return(-1) ;
	}
if (i = derror(R)) { /* error */
	putlin("- - - - - - -") ;
	putstr("read SHD error") ;
	stmes(i) ;
	dadmes(R) ;
	/* return if any header errors */
	if (i & (RP_HCRC|RP_HCE|RP_FER)) return(-1) ; /* error */
	}
/* normal return is to return header-error-free status */
return(i) ;
}
 
/*		*/
 
hcheck(insec,ousec)
register short *insec , *ousec ;
{
/*
* Compare sector header and data info on disk against that which
*  was written from output buffer
*/
register int j ;
 
/*  cyl and sect/track */
if (((*insec++)!=(*ousec++)) ||
  ((*insec++)!=(*ousec++)) ) return(1);
/* if RP06,  keywords */
if ((dtype==RP6typ) && ( ((*insec++)!=(*ousec++)) ||
  ((*insec++)!=(*ousec++)) )) return(1);
for (j = 0 ; j < 256 ; j++)
	if ((*insec++) != (*ousec++))
		return(1) ;
return(0) ;
}
 
/*		*/
 
format(baddr,nb,s)
int baddr , nb , s ;
{
/*
*  Function to write 'nb' bytes worth of sector and data info
*  from buffer whose map/offset is 'baddr' to disk.
*/
register int i , j ;
register int *R , *M ;
 
R = RPptr ;
M = Mptr ;
 
*(R+RP_cr) = RP_DC | RP_GO ;
dwait(R) ;
*(R+RP_cyl) = cylndr ;
*(R+RP_stk) = (track<<8) | s ; /* sector : track */
*(M+M_bc) = (-nb) ; /* byte count */
*(M+M_var) = baddr ; /* virt addr reg = map no. + byte off */
*(R+RP_cr) = RP_WHD | RP_GO ; /* write sector header and data */
 
dwait(R) ; /* wait for i/o to finish */
if (i = mbaerr(M)) {
	return(1) ;
	}
if (i = derror(R)) {
	putlin("- - - - - -") ;
	putstr("write SHD error") ;
	stmes(i) ;
	dadmes(R) ;
	if (wcrflg) return(1) ; /* return if in 'write check' recovery */
	if (++ecount > MAXERR) return(-1) ;
	}
return(0) ; /* normal return */
}
 
/*		*/
 
trkinc()
{
/*
*  Increment track no. in output buffer sector headers.
*/
register char *sp ;
register int i , j;
 
j = hdsiz + 512;
for (i = 0 , sp = obuf ; i < rpsec ; i++ ,  sp += j)
	sp[HDtoff]++ ;
}
 
/*		*/
 
cylinc() {
/*
*  Increment cylinder no. in output buffer sector headers.
*/
register short *sp ;
register int i , j;
 
j = hdsiz + 512;
for (i = 0 , sp = (short *)obuf ; i < rpsec ; i++ , sp = (short *)((int)sp +  j))
	sp[HDcoff]++ ;
}
 
/*		*/
 
trkclr()
{
/*
*  Clear track no. in output buffer sector headers.
*/
register char *sp ;
register int i , j;
 
j = hdsiz + 512;
for (i = 0 , sp = obuf ; i < rpsec ; i++ , sp += j)
	*(sp+HDtoff) = 0;
}
 
/*		*/
 
wcheck(baddr,nb,s)
int baddr , nb , s ;
{
/*
*  Function to write check 'nb' bytes worth of sector and data info
*  from buffer whose map reg is 'baddr' - start at sector 's'.
*/
register int i , j ;
register int *R , *M ;
 
R = RPptr ;
M = Mptr ;
 
*(R+RP_cr) = RP_DC | RP_GO ;
dwait(R) ;
*(R+RP_cyl) = cylndr ;
*(R+RP_stk) = (track<<8) | s ; /* sector : track */
*(M+M_bc) = (-nb) ; /* byte count */
*(M+M_var) = baddr ; /* virt addr reg = map no. + byte off */
*(R+RP_cr) = RP_WCH | RP_GO ; /* write check sector header and data */
 
dwait(R) ; /* wait for i/o to finish */
if (i = mbaerr(M)) {
	if (i & (M_WCKU | M_WCKL)) { /* Write Check Error */
		wcerr++ ;
		putlin("* Write Check Error *") ;
		dadmes(R) ;
		i = *(M+M_bc) | 0xffff0000 ;
		l2x(nb+i,input) ;
		putstr("no. bytes read : ") ;
		putlin(input) ;
		if (wcrflg) return(1) ;
		wckrcv() ;
		}
	}
if (i = derror(R)) {
	putlin("- - - - - -") ;
	putstr("write error") ;
	stmes(i) ;
	dadmes(R) ;
	ecount++ ;
	}
if (ecount > MAXERR) return(-1) ;
return(0) ; /* normal return */
}
 
/*		*/
 
wckrcv()
{
/*
*  Try to recover from a 'write check' error during a
*  'Write Check Header and Data' function.
*  'RPptr' is ptr to RP register set.
*  'Mptr' is ptr to MBA reg set.
*  MBA byte count reg has neg. no. bytes remaining - transfer stops
*  on error, even in middle of a sector.
*  Loop with a 'Write Sector Header and Data' followed by a 'Read
*  Sector Header and Data' - each loop iteration uses a different head 
*  centerline offset.
*  If all fails, report irrecoverable error and finish off the
*  'write check data and header' on the rest of the track.
*/
register int i , j , k , l , m ;
 
wcrflg++ ;  /*  flag says 'write check' recovery i-o is in progress */
/* If no. bytes read thus far is a multiple of 'BLKSIZ+RPHEAD', then
*  error was in last byte of sector and sector has to be backed up
*  by 1.
*/
j = curpos() ; /* return current cyl , trk ,sector */
i = ((*(Mptr+M_bc))>>16) + bsize ;  /*  no. bytes read by 'write check' */
if ((i%(BLKSIZ+hdsiz)) == 0) {
	putlin("wckrcv : backup 1 sector") ;
	j = backup(j) ;
	}
j = j & 0x1f ; /* sector only */
m = j * (BLKSIZ+hdsiz) ; /* page no. and offset */
/*
*  Loop with a 'Write Sector Header and Data' followed by a
*  'Read Sector Header and Data' - each iteration uses a
*  different centerline offset - 1st offset is 0.
*/
for (i = 0 ; (l = Offset[i]) >= 0 ; i++) {
	k = *(RPptr+RP_off) & 0xff00 ;
	*(RPptr+RP_off) = k | l ;
	*(RPptr+RP_cr) = RP_DC | RP_GO ;
	dwait(RPptr) ;
	*(RPptr+RP_cr) = RP_OFF | RP_GO ;
	dwait(RPptr) ;
	/*  Format 1 sector */
	if (format(m,BLKSIZ+hdsiz,j)) {
		putlin("wckrcv: format error");
		continue;
		}
	/*  Format was successful
	    Read sector. */
	if (rshd(128<<9,BLKSIZ+hdsiz,j) < 0) {
		putlin("wckrcv: rshd() error");
		continue ;
		}
	/*  successful 'Read Sector Header and Data' */
	/*  Compare sector written from output buffer
		against sector just read */
	if (hcheck(ibuf,obuf+m)) {
		putlin("wckrcv : sector compare failure") ;
		continue ;
		}
	/*  good read and good compare */
	putstr("    head offset : ") ;
	l2x(l,input) ;
	putlin(input) ;
	goto wckfin ;
	}
 
putlin("wckrcv : bad sector") ;
badsec++ ;
 
wckfin :
 
*(RPptr+RP_cr) = RP_RTC | RP_GO ; /* return to centerline */
dwait(RPptr) ;
/*  Cyl , trk and sector are set to values to continue where
*  original 'Write Check Header and Data' left off
*/
j = curpos() & 0xff ;  /*  sector  */
wcrflg = 0 ;
i = j * (BLKSIZ+hdsiz) ; /* offset */
/*  continue rest of 'wcheck89' */
wcheck(i,(rpsec-j)*(BLKSIZ+hdsiz),j) ;
return(0) ;
}
 
/*		*/
 
curpos()
{
/*
*  Function to return current cyl, trk and sector for RP whose
*  register set is pointed to by 'RPptr'.
*/
register int i , j ;
 
i = *(RPptr+RP_cyl)<<16 ;
j = *(RPptr+RP_stk) & 0x1f1f ;
return(i|j) ;
}
 
/*		*/
 
backup(cts)
register int cts ;
{
/*
*  Function to backup 1 sector.
*  Cyl, track and sector are in 'cts' .
*/
register int trk , sec ;
 
sec = cts & 0x1f ;  /*  sector */
if (sec > 0) return(--cts) ;
 
cts = cts & 0xffffff00 ;
cts = cts | (rpsec-1) ;
 
trk = (cts>>16) & 0x1f ;  /*  track */
if (trk>0) return(cts-0x100) ;  /*  dec track */
 
cts = cts & 0xffff00ff ;
cts = cts | ((rptrk-1)<<8) ;
 
return(cts-0x10000) ;  /*  dec cyl no.  */
}
 
/*		*/
 
dread(baddr,nb,s)
int baddr , nb , s ;
{
/*
*  Function to read 'nb' bytes into buffer pointed to
*    by map 'baddr' - start at sector 's'.
*/
register int i , *R , *M , j;
 
R = RPptr ;
M = Mptr ;
 
*(R+RP_cyl) = cylndr ; /* cylinder no. */
*(R+RP_stk) = (track<<8) | s ;  /* track:sector */
*(M+M_bc) = (-nb) ;
*(M+M_var) = baddr ; /* virt addr reg = map no. + byte off */
rini :
*(R+RP_cr) = RP_DC | RP_GO ; /*  RP06 drive clear function code */
dwait(R) ;
i = *(R+RP_off) & (~(RP_ECI)) ; /* enable ECC */
*(R+RP_off) = i ;
*(R+RP_cr) = RP_RED | RP_GO ; /* read */
 
dwait(R) ; /* wait for i/o to finish */
*(R+RP_off) = *(R+RP_off) | RP_ECI ; /* ECC inhibit on */
if (i = mbaerr(M)) {
	return(-1) ;
	}
if (i = derror(R)) { /* error */
	putlin("- - - - - - -") ;
	putstr("read error") ;
	stmes(i) ;
	dadmes(R) ;
	if (wcrflg)
		if (i & (RP_HCE|RP_HCRC|RP_FER|RP_PAR))
			return(1) ;
	if (i & (~(RP_DCK))) {
		ecount++ ;
		}
	if (i & RP_DCK) { /* Data Check Error */
		daterr++ ;
		putlin("* Data Check *") ;
		if (i & RP_ECH) { /* ECC Hard Error */
			putlin("ECC non-recov") ;
			if (wcrflg) return(1) ;
			ecount++ ;
			}
		else { /* ECC recoverable */
			ECCrcv(R) ;
			}
		if (*(M+M_bc)) { /* more i-o to complete */
			j = (*(R+RP_stk));
			i = (j>>8) & 0x1f;
			j = j & 0x1f;
			if (j>=rpsec) /*sector */
				j = 0;
			if (i>=rptrk) /* track */
				i = 0;
			*(R+RP_stk) = (i<<8) | j;
			goto rini ;
			/* status reg cleared by Drive Clear */
			}
		}
	if (wcrflg) return(0) ;
	if (ecount > MAXERR) return(-1) ;
	}
return(0) ; /* normal return */
}
 
/*		*/
 
ECCrcv(dptr)
register int *dptr ;
{
/*
*  Do ECC error recovery on disk whose register set is pointed
*  to by 'dptr'.
*  'mbaerr()' has cleared MBA status reg.
*  With ECC enabled, disk read has stopped after sector with bad
*  data. After correction of data, return to read routine which will
*  continue read of track if more sectors to do.
*  Return 0.
*/
register unsigned int pos , pat , ll;
register unsigned short *wordp ;
struct www {
	union {
		int x123;
		short wlo , whi ;
		} sss;
	};
char tmp[50] ;
 
pat = (*(dptr+RP_Epat)) & 0xffff ; /* ECC pattern reg */
pos = (*(dptr+RP_Epos)) & 0xffff ; /* ECC position reg */
putstr("pat : ") ;
ul2x(pat,tmp) ;
putlin(tmp) ;
putstr("pos : ") ;
ul2x(pos,tmp) ;
putlin(tmp) ;
wordp = (unsigned short *)ibuf ;   /* ptr to input buffer */
 
/*
*  'bufsiz' bytes are read on each read into buffer pointed to
*  by 'ibuf'. MBA byte count reg has neg. no. of bytes remaining
*  in read if this read error was not in the last sector to be
*  read.
*/
/* calculate buffer location of faulty data */
wordp = (unsigned short *)((int)wordp + (bufsiz + ((*(Mptr+M_bc))>>16) - BLKSIZ)) ; /* sector in buffer */
wordp = (unsigned short *)((int)wordp + ((pos-1)>>4)) ; /* word within sector */
 
/* burst pattern may be across word boundary */
ll = (*wordp) + ((*(wordp+1))<<16) ;
putstr("bad data  : ") ;
ul2x(ll,tmp) ;
putlin(tmp) ;
pat = pat<<((pos%16)-1) ;
ll = ll^pat ; /* correction */
putstr("good data : ") ;
ul2x(ll,tmp) ;
putlin(tmp) ;
 
/* put good data back in buffer */
*wordp = LOW(ll);
*(wordp+1) = HIGH(ll);
 
return(0) ;
}
 
/*		*/
 
mbaerr(mba)
register int *mba ;
{
register int i ;
 
if (i = (*(mba+M_sr))) {
	if (i == 0x2000) return(0) ;
	if (i != MB_ERR) {
		putlin("- - - - - -") ;
		putstr("MBA error") ;
		stmes(i) ;
		*(mba+M_sr) = (-1) ;
		return(i) ;
		}
	}
return(0) ;
}
