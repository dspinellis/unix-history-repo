# define min(ii,jj) (ii<jj?ii:jj)
# define RP6CYL  815  /*  no. RP06 cylinders/pack */
# define RP6TRK  19  /*  no. tracks/cyl  */
# define RP6SEC  22  /*  no. sectors/track  */
# define RP6ST  (rpsec*rptrk)  /*  no. sectors/cyl  */
# define MAXSEC  (rpcyl*rptrk*rpsec)  /*  sectors/pack  */
# define M0  0x20010000  /* phys addr MBA 0  */
# define M_st  2  /* offset for MBA status reg */
# define M0_cr  (M0+4)  /*  MBA 0 control reg addr */
# define M0_sr (M0+8)  /* mba 0 status reg */
# define M0_map  (M0+0x800)  /* start MBA 0 map reg's */
# define M0_var  (M0+0xc)  /* MBA 0 virt addr reg */
# define M0_bc  (M0+0x10)  /*  MBA 0 byte count reg */
# define MBAinit 01  /*  MBA init bit */
# define MBA_DTC  0x2000  /* MBA date transfer complete, status reg */
/*		*/
# define RP  (M0+0x400)  /*  base for RP06 reg's, drive 0  */
/*		*/
# define RP_cr  0  /* RP06 control reg offset, longword */
# define RP_sr  1  /*  RP06 status reg offset */
# define RP_er1  02  /*  RP error reg 1 */
# define RP_stk  5  /*  RP06 sector/track reg offset */
# define RP_dt  06   /* drive type reg */
# define RP_er2 010  /* RP error reg 2 */
# define RP_off  011  /*  RP offset reg */
# define RP_cyl  012  /*  RP06 cylinder reg offset */
# define RP_er3 015  /*  RP error reg 3 */
# define RP_Epos  016  /* RP ECC position reg */
# define RP_Epat  017  /*  RP ECC pattern reg */
/*		*/
# define RP_GO  1  /*  go bit */
# define RP_RED  070  /*  RP06 read function code */
# define RP_DC  010  /*  drive clear function code */
# define RP_FMT  0x1000  /*  format bit for offset reg */
# define RP_RIP 020  /*  Read-in Preset RP06 function code */
# define RP_DRY  0200  /*  drive ready, status reg */
# define RP_ERR 040000  /* composite error, status reg */
# define RP_MOL 0x1000  /*  medium-online bit in status reg */
# define RP_ECH 0x40  /* ECC Hard Error */
# define RP_DCK 0x8000  /*  Data Check error */
/*		*/
# define RXCS	32  /*  receiver control/staus */
# define RXDB  33  /*  receiver data */
# define TXCS  34  /*  transmitter control/status */
# define TXDB  35  /*  transmitter data */
# define RXCS_DONE  0x80  /*  receiver done */
# define TXCS_RDY  0x80  /*  transmitter ready */
/*		*/
# define BLKSIZ 512
# define NB  128
# define BUFSIZ (NB*BLKSIZ) /* buffer 1 track */
# define MAXUNI  1
# define MAXERR  20
# define NL  012
# define CR 015
# define CDEL 0x23
# define LDEL 0x40
/*		*/
# define RP6typ  022
# define RM3typ  024
# define RM3CYL  823
# define RM3TRK  5
# define RM3SEC  32
/*		*/
struct { int reg , reg2 , reg3 ; } ;
int count , dskoff , dunit , ecount , daterr ;
int *RPptr ; /* ptr to start of RP reg's for desired drive */
int rpcyl,rptrk,rpsec;
char *bufptr ;
 
 
 
main() {
/*
*  Stand-alone program to read disk (error checker).
*  User specifies disk unit, start disk block and no. of blocks.
*/
int getcon() , putstr() , a2l() , l2a() ;
char input[132] ;

putlin("dread : Read RP06/RM03 Disk") ;
putnl() ;
 
dun :
putstr("disk unit : ");
getcon(input) ;
dunit = a2l(input) ;
if (dunit < 0) goto fini ;
if (dunit > MAXUNI) goto dun ;
 
doff :
putstr("start block : ") ;
if (getcon(input)) goto fini ;
dskoff = a2l(input) ;
if (dskoff < 0) goto doff ;
 
gknt :
putstr("no. blocks : ") ;
if (getcon(input)) goto fini ;
count = a2l(input) ;
if (count < 0) goto gknt ;
putnl() ;
 
if (init()) {
	putlin("init error") ;
	goto dun ;
	}
 
if (dskoff > MAXSEC-1) goto doff ;
if (count == 0) count = MAXSEC ;
 
while (count > 0) {
 
	if (dread()) {
		putstr("disk read error") ;
		dadmes(RPptr) ;
		}
	dskoff += NB ; /* next start place */
	count -= NB ;
}
 
putstr("# Data Check errors : ") ;
l2x(daterr,input) ;
putlin(input) ;
putstr("# Other errors : ") ;
l2x(ecount,input) ;
putlin(input) ;
 
goto dun ;
 
fini :
return(0) ;
}
 
/*		*/
 
putstr(csp)
register char *csp ;
{
if (putcon(csp)) return(-1) ;
return(0) ;
}
 
/*		*/
 
putlin(sptr)
register char *sptr ;
{
if (putcon(sptr)) return(-1) ;
if (putnl()) return(-1) ;
return(0) ;
}
 
/*		*/
 
nullcon(nn)
register nn ;
{
/*
*  Output 'nn' nulls to console terminal -
*  used for delay.
*/
while (nn--) putc(0) ;
}
 
/*		*/
 
putnl()
{
if (putcon("\r\n")) return(-1) ;
return(0) ;
}
 
/*		*/
 
putcon(csp)
register char *csp ;
{
/*
*  Function to output null-terminated string pointed to 
*  by 'csp' to the VAX LSI terminal.
*/
register c ;
 
c = 0 ;
while (c = (*csp++)) putc(c) ;
return(0) ;
}
 
/*		*/
 
putc(c)
{
/*  wait for LSI printer to be ready */
while ((mfpr(TXCS) & TXCS_RDY) == 0) ;
/*  output character */
mtpr(TXDB,c&0177) ;
}
 
/*		*/
 
getcon(cs)
register char *cs ;
{
/*
*  Function to return char's from VAX LSI keyboard to
*  char array 'cs' - input stops when CR or LF received -
*  null char appended to end of input
*/
register int c , c2 ;
int getc() ;
char *ocs;
 
	ocs=cs;
inloop :
	c = getc() ; /* get 1 char from terminal */
	putc(c) ;  /*  echo char */
	if (c==CDEL) {cs--; goto inloop;}
	if (c==LDEL) {cs=ocs;putc(CR);putc(0);putc(NL);goto inloop;}
	if ((c == NL) || (c == CR)) {
		putc(CR) ;
		putc(0) ;
		putc(NL) ;
		(*cs++) = '\0' ;
		return(0) ;
		}
	else {
		(*cs++) = c ;
		goto inloop ;
		}
}
 
/*		*/
 
getc()
{
/*
*  Return char from VAX LSI terminal char buffer
*/
int mfpr() ;
 
/*  Wait for receiver done (user entered char)
*/
while ((mfpr(RXCS) & RXCS_DONE) == 0) ;
return (mfpr(RXDB) & 0177) ;  /* return char from receiver buffer */
}
 
/*		*/
 
mtpr(regno,value)
{
	asm("	mtpr	8(ap),4(ap)") ;
}
 
/*		*/
 
mfpr(regno)
{
	asm("	mfpr	4(ap),r0") ;
}
 
/*		*/
 
a2l(as)
register char *as ;
{
/*
*  Convert null-terminated ascii string to binary
*  and return value.
*  1st char in string :
*	0 -> octal
*	x -> hex
*	else decimal
*/
register value , base , sign , digit ;
 
digit = value = sign = 0 ;
base = 10 ;  /* default base */
 
aloop :
if ((digit = (*as++)) == 0) return(value) ; /* null */
 
if (digit == '-') {
	sign++ ;
	goto aloop ;
	}
 
if (digit == '0') base = 8 ;  /* octal base  */
else { if (digit == 'x') base = 16 ;  /* hex base */
	else value = (digit-060) ; /* 060 = '0' */
	}
 
while (digit = (*as++)) {
	if (digit < '0') return(0) ;
	switch (base) {
		case 8 : {
			if (digit > '7') return(0) ;
			digit -= 060 ;
			break ;
			}
		case 10 : {
			if (digit > '9') return(0) ;
			digit -= 060 ;
			break ;
			}
		case 16 : {
			if (digit <= '9') {
				digit -= 060 ;
				break ;
				}
			if ((digit >= 'A') && (digit <= 'F')) {
				digit = (digit - 0101 + 10) ;
					break ;
				}
			if ((digit >= 'a') && (digit <= 'f')) {
				digit = digit - 0141 + 10 ;
				break ;
				}
			return(0) ;
			break ;
			}
		}
	value = (value * base) + digit ;
	}
return (sign ? -value : value) ;
}
 
/*		*/
 
init() {
/*
*  Initialization.
*  Initialize MBA 0 (disk) .
*  Set up MBA 0  map register to map a 
*    transfer of 'BUFSIZ' bytes.
*/
extern char end ;
register int page , *mp0 , i ;
 
M0_cr->reg = MBAinit ; /* MBA 0 init */
RPptr = RP + (dunit * 32 * 4) ; /* start of RP reg's for drive */
if ((*(RPptr+RP_sr) & RP_MOL) == 0) {
	putlin("unit not online") ;
	return(-1) ;
	}
*(RPptr+RP_cr) = RP_RIP | RP_GO ; /* drive preset - sets vv */
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
bufptr = (int)(&end+511)&017777777000 ; /* buffer ptr */
page = (int)((int)bufptr>>9) & 017777777 ;
mp0 = M0_map ;
for (i = 0 ; i < NB ; i++)
	(*mp0++) = 0x80000000 | page++ ;
(*mp0++)=0;
ecount = daterr = 0 ;
return(0) ;
}
 
/*		*/
 
dread()
{
/*
*  Function to read 'BUFSIZ' bytes to buffer pointed to
*    by 'bufptr'.
*/
register int i , j ;
 
*(RPptr+RP_cyl) = dskoff/RP6ST ; /* cylinder no. */
i = dskoff%RP6ST ;
j = (i/rpsec)<<8 ; /* track */
*(RPptr+RP_stk) = j | (i%rpsec) ; /* sector : track */
i = min(NB,count);
M0_bc->reg = (-(i*BLKSIZ)) ;
M0_var->reg = 0 ; /* virt addr reg = map no. + byte off */
rini :
*(RPptr+RP_cr) = RP_DC | RP_GO ; /*  RP06 drive clear function code */
dwait() ;
*(RPptr+RP_cr) = RP_RED | RP_GO ; /* read */
 
dwait() ; /* wait for i/o to finish */
if (i = mbaerr(M0)) {
	/* don't abort on MBA errors - stat reg has been cleared */
	}
if (i = derror(RPptr)) { /* error */
	putlin("- - - - - - -") ;
	putstr("read error") ;
	stmes(i) ;
	dadmes(RPptr) ;
	if (i & RP_DCK) { /* Data Check Error */
		daterr++ ;
		putlin("Data Check") ;
		if (i & RP_ECH) { /* ECC Hard Error */
			putlin("ECC non-recov") ;
			ecount++ ;
			}
		else { /* ECC recoverable */
			ECCrcv(RPptr) ;
			}
		if (M0_bc->reg) { /* more i-o to complete */
			j = (*(RPptr+RP_stk));
			i = (j>>8) & 0x1f;
			j = j & 0x1f;
			if (j>=rpsec) /*sector */
				j = 0;
			if (i>=rptrk) /* track */
				i = 0;
			*(RPptr+RP_stk) = (i<<8) | j;
			goto rini ;
			/* status reg cleared by Drive Clear */
			}
		}
	else { /* non-Data Check error */
		ecount++ ;
		}
	if (ecount > MAXERR) return(-1) ;
	}
return(0) ; /* normal return */
}
 
/*		*/
 
dwait() {
/*
* Function to wait MBA 0 RP06 disc unit to be ready.
*/
while ((*(RPptr+RP_sr)&RP_DRY) == 0) ;
}
 
/*		*/
 
derror()
{
/*
*  Function to check for MBA 0 RP06 error.
*/
if (*(RPptr+RP_sr) & RP_ERR) return(*(RPptr+RP_er1) & 0177777) ;
return(0) ;
}
 
/*		*/
 
halt()
{
asm("	halt") ;
}
 
/*		*/
 
l2a(val,rptr)
register int val ;
register char *rptr ;
{
register int i ;
register char *tp ;
int knt ;
char tmp[20] , sign ;
 
knt = sign = 0 ;
if (val < 0) {
	sign++ ;
	val = (-val) ;
	}
 
tp = tmp ;
loop :
	knt++ ;
	i = val/10  ;  /*  quotient & base 10 */
	(*tp++) = val%10 + '0' ; /*  ascii remainder  */
	val = i ;
	if (val == 0) {
		/*  done  dividing  */
		if (sign) { knt++ ; (*tp++) = '-' ; }
		for (i = knt ; i ; i--)
			(*rptr++) = tmp[i-1] ;
		(*rptr++) = '\0' ;
		return(knt) ;
		}
	else goto loop ;
}
 
/*		*/
 
dadmes(dptr)
register int *dptr ;
{
register char *mesp ;
register int i ;
 
mesp = " cyl     trk     sec    " ;
i = l2x(*(dptr+RP_cyl) & 01777,&mesp[5]) ;
blnkit(&mesp[5+i],4-i) ;
 
i = l2x((*(dptr+RP_stk)>>8) & 037 , &mesp[13]) ;
blnkit(&mesp[13+i],3-i) ;
 
i = l2x(*(dptr+RP_stk)&037,&mesp[21]) ;
blnkit(&mesp[21+i],3-i) ;
 
putlin(mesp) ;
}
 
/*		*/
 
l2x(val,rptr)
register int val ;
register char *rptr ;
{
register int i , j ;
register char *tp ;
int knt ;
char tmp[20] , sign ;
 
knt = sign = 0 ;
if (val < 0) {
	sign++ ;
	val = (-val) ;
	}
 
tp = tmp ;
loop :
	knt++ ;
	i = val/16  ;  /*  quotient & base 16 */
	j = val%16 ;
	(*tp++) = j + (j<10?0x30:0x57) ;
	val = i ;
	if (val == 0) {
		/*  done  dividing  */
		if (sign) { knt++ ; (*tp++) = '-' ; }
		for (i = knt ; i ; i--)
			(*rptr++) = tmp[i-1] ;
		(*rptr++) = '\0' ;
		return(knt) ;
		}
	else goto loop ;
}
 
/*		*/
 
blnkit(mp,cc)
register char *mp ;
register int cc ;
{
 
while (cc--)
	(*mp++) = ' ' ;
}
 
/*		*/
 
stmes(code)
int code ;
{
putstr(" : err reg : ") ;
RPE_print(code) ;
}
 
/*		*/
 
mbaerr(mba)
register int *mba ;
{
register int i ;
 
if ((i = (*(mba+M_st))) != MBA_DTC) {
	putlin("- - - - - -") ;
	putstr("MBA error : status reg : ") ;
	MBAS_print(i) ;
	*(mba+M_st) = (-1) ;
	return(i) ;
	}
return(0) ;
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
*  data. After correction of data, return to 'dread()' which will
*  continue read of track if more sectors to do.
*  Return 0.
*/
register unsigned int pos , pat ;
register unsigned short *wordp ;
unsigned int ll ;
struct { short wlo , whi ; } ;
char tmp[50] ;
 
pat = (*(dptr+RP_Epat)) & 0xffff ; /* ECC pattern reg */
pos = (*(dptr+RP_Epos)) & 0xffff ; /* ECC position reg */
putstr("pat : ") ;
ul2x(pat,tmp) ;
putlin(tmp) ;
putstr("pos : ") ;
ul2x(pos,tmp) ;
putlin(tmp) ;
wordp = bufptr ;   /* ptr to buffer */
 
/*
*  'BUFSIZ' bytes are read on each read into buffer pointed to
*  by 'bufptr'. MBA byte count reg has neg. no. of bytes remaining
*  in read if this read error was not in the last sector to be
*  read.
*/
/* calculate buffer location of faulty data */
wordp = (char *)wordp + (BUFSIZ + ((M0_bc->reg)>>16) - BLKSIZ) ; /* sector in buffer */
wordp = wordp +  ((pos-1)>>4) ; /* word within sector */
 
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
*wordp = ll.wlo ;
*(wordp+1) = ll.whi ;
 
return(0) ;
}
 
/*		*/
 
ul2x(val,rptr)
register unsigned int val ;
register char *rptr ;
{
register unsigned int i , j ;
register char *tp ;
int knt ;
char tmp[20] ;
unsigned int udiv() , urem() ;
 
knt =  0 ;
 
tp = tmp ;
loop :
	knt++ ;
	/* use unsigned integer divide & remainder routines */
	i = udiv(val,16)  ;  /*  quotient & base 16 */
	j = urem(val,16) ;
	(*tp++) = j + (j<10?0x30:0x57) ;
	val = i ;
	if (val == 0) {
		/*  done  dividing  */
		for (i = knt ; i ; i--)
			(*rptr++) = tmp[i-1] ;
		(*rptr++) = '\0' ;
		return(knt) ;
		}
	else goto loop ;
}
