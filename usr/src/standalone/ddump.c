# define RP6CYL  815  /*  no. RP06 cylinders/pack */
# define RP6TRK  19  /*  no. tracks/cyl  */
# define RP6SEC  22  /*  no. sectors/track  */
# define RP6ST  (RP6SEC*RP6TRK)  /*  no. sectors/cyl  */
# define MAXSEC  (RP6CYL*RP6TRK*RP6SEC)  /*  sectors/pack  */
# define M0  0x20010000  /* phys addr MBA 0  */
# define M0_cr  (M0+4)  /*  MBA 0 control reg addr */
# define M0_sr (M0+8)  /* mba 0 status reg */
# define M0_map  (M0+0x800)  /* start MBA 0 map reg's */
# define M0_var  (M0+0xc)  /* MBA 0 virt addr reg */
# define M0_bc  (M0+0x10)  /*  MBA 0 byte count reg */
# define MBAinit 01  /*  MBA init bit */
/*		*/
# define RP  (M0+0x400)  /*  base for RP06 reg's, drive 0  */
/*		*/
# define RP_cr  0  /* RP06 control reg offset, longword */
# define RP_sr  1  /*  RP06 status reg offset */
# define RP_er1  02  /*  RP error reg 1 */
# define RP_stk  5  /*  RP06 sector/track reg offset */
# define RP_er2 010  /* RP error reg 2 */
# define RP_off  011  /*  RP offset reg */
# define RP_cyl  012  /*  RP06 cylinder reg offset */
# define RP_er3 015  /*  RP error reg 3 */
/*		*/
# define RP_GO  1  /*  go bit */
# define RP_RED  070  /*  RP06 read function code */
# define RP_DC  010  /*  drive clear function code */
# define RP_FMT  0x1000  /*  format bit for offset reg */
# define RP_RIP 020  /*  Read-in Preset RP06 function code */
# define RP_DRY  0200  /*  drive ready, status reg */
# define RP_ERR 040000  /* composite error, status reg */
# define RP_MOL 0x1000  /*  medium-online bit in status reg */
/*		*/
# define RXCS	32  /*  receiver control/staus */
# define RXDB  33  /*  receiver data */
# define TXCS  34  /*  transmitter control/status */
# define TXDB  35  /*  transmitter data */
# define RXCS_DONE  0x80  /*  receiver done */
# define TXCS_RDY  0x80  /*  transmitter ready */
/*		*/
# define BLKSIZ 512
# define MAXUNI  1
# define NL  012
# define CR 015
/*		*/
char input[BLKSIZ]  ;  /*  disc input buffer */
struct { int reg , reg2 , reg3 ; } ;
int count , dskoff , dunit , error , numbyt ;
unsigned short bytoff ;
int *RPptr ; /* ptr to start of RP reg's for desired drive */
main() {
/*
*  Stand-alone program to dump RP06 disk to VAX LSI console
*  printer in hex format.
*  User specifies disk unit, start disk block and no. of blocks.
*/
int getcon() , putstr() , a2l() , l2a() ;

putlin("ddump : disk-to-console hex dump") ;
putnl() ;
 
dun :
putstr("disk unit : ");
getcon(input) ;
dunit = a2l(input) ;
if ((dunit > MAXUNI) || (dunit < 0)) goto dun ;
 
doff :
putstr("start disk block : ") ;
if (getcon(input)) goto fini ;
dskoff = a2l(input) ;
if (dskoff < 0) goto fini ;
if (dskoff > MAXSEC-1) goto doff ;
 
gknt :
putstr("no. blocks : ") ;
if (getcon(input)) goto fini ;
count = a2l(input) ;
if (count < 0) goto gknt ;
if (count == 0) count = 1 ;
 
gbyt :
putstr("no. bytes : ") ;
if (getcon(input)) goto fini ;
numbyt = a2l(input) ;
if ((numbyt < 0) || (numbyt > BLKSIZ)) goto gbyt ;
if (numbyt == 0) numbyt = BLKSIZ ;
 
error = 0 ;
 
if (init()) goto dun ;
 
putlin("                      HI  < - -  LO") ;
nullcon(3) ;
putnl() ;
 
while ((error == 0) && (count--)) {
	if (dread()) {
			putlin("disk i/o error") ;
			ioerr :
				error++ ;
				continue ;
			}
	if (prblk()) {
		putlin("console i/o error") ;
		goto ioerr ;
		}
	putnl() ;
	putnl() ;
	dskoff++ ; /* next sector */
}
 
goto doff ;
 
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
 
inloop :
	c = getc() ; /* get 1 char from terminal */
	putc(c) ;  /*  echo char */
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
*    transfer of 'BLKSIZ' bytes.
*/
register int page , *mp0 ;
 
M0_cr->reg = MBAinit ; /* MBA 0 init */
RPptr = RP + (dunit * 32 * 4) ; /* start of RP reg's for drive */
if ((*(RPptr+RP_sr) & RP_MOL) == 0) {
	putlin("unit not online") ;
	return(-1) ;
	}
*(RPptr+RP_cr) = RP_RIP | RP_GO ; /* drive preset - sets vv */
*(RPptr+RP_off) = RP_FMT ; /* set format bit */
 
bytoff = (int)(&input[0])&0777 ; /* byte offset of buffer addr */
page = ((int)&input[0] >> 9) & 07777777 ; /* start page of buffer */
mp0 = M0_map ;
(*mp0++) = 0x80000000 | page++ ;
(*mp0++) = 0x80000000 | page++ ;
(*mp0++) = 0 ;
return(0) ;
}
 
/*		*/
 
dread()
{
/*
*  Function to read 'BLKSIZ' bytes to buffer 'input[]'.
*/
register int i , j ;
 
*(RPptr+RP_cr) = RP_DC | RP_GO ; /*  RP06 drive clear function code */
*(RPptr+RP_cyl) = dskoff/RP6ST ; /* cylinder no. */
i = dskoff%RP6ST ;
j = (i/RP6SEC)<<8 ; /* track */
*(RPptr+RP_stk) = j | (i%RP6SEC) ; /* sector : track */
M0_bc->reg = (-BLKSIZ) ;
M0_var->reg = bytoff ; /* virt addr reg = map no. + byte off */
*(RPptr+RP_cr) = RP_RED | RP_GO ; /* read */
 
dwait() ; /* wait for i/o to finish */
if (derror()) return(-1) ; /* error */
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
if (*(RPptr+RP_sr) & RP_ERR) return(-1) ;
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
 
echo(lngword)
register int lngword ;
{
char tmp[30] ;
 
l2a(lngword,tmp) ;
putlin(tmp) ;
}
 
/*		*/
 
prblk()
{
/*
*  Print 512 bytes on VAX LSI console as hex
*  characters.
*  Translate bytes in 'input[]' to hex char's
*  and out to console, 64 char's per line.
*/
register int i , j ;
int k ;
char c , *hp , *fr ;
char tmp[1025] , ltmp[65] ;
 
ltmp[64] = '\0' ;
hp = "block # \0\0\0\0\0\0\0" ;
l2a(dskoff,&hp[8]) ;
putstr(hp) ;
putnl() ;
putnl() ;
 
hxcnvt(input,512,tmp) ; /* convert bytes to hex char's */
 
j = numbyt ;
for (i = 0 ; (i < 1024) && (j > 0) ; i=+64 ) {
	hp = ltmp ;
	fr = (&tmp[i+63]) ;
	for ( k = 32 ; k ; k--) {
		(*hp++) = *(fr-1) ;
		(*hp++) = (*fr--) ;
		fr-- ;
		}
	putstr(ltmp) ;
	putc(CR) ;
	nullcon(5) ;
	putc(NL) ;
	j -= 32 ;
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
