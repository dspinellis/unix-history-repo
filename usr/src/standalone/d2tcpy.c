# define MIN(a,b) (a<b?a:b)
# define RP6CYL  815  /*  no. RP06 cylinders/pack */
# define RP6TRK  19  /*  no. tracks/cyl  */
# define RP6SEC  22  /*  no. sectors/track  */
# define RP6ST  (RP6TRK*RP6SEC)  /*  no. sectors/cyl  */
# define MAXSEC  (RP6CYL*RP6TRK*RP6SEC)  /*  sectors/pack  */
# define M0  0x20010000  /* phys addr MBA 0  */
# define M1  0x20012000  /*  phys addr MBA 1  */
# define M0_cr  (M0+4)  /*  MBA 0 control reg addr */
# define M1_cr  (M1+4)  /*  MBA 1 control reg addr  */
# define M0_map  (M0+0x800)  /* start MBA 0 map reg's */
# define M1_map  (M1+0x800)  /* start MBA 1 map reg's  */
# define M0_var  (M0+0xc)  /* MBA 0 virt addr reg */
# define M1_var  (M1+0xc)  /*  MBA 1 virt addr reg  */
# define M0_bc  (M0+0x10)  /*  MBA 0 byte count reg */
# define M1_bc  (M1+0x10)  /*  MBA 1 byte count reg  */
# define MBAinit 01  /*  MBA init bit */
/*		*/
# define TM  (M1+0x400)  /*  base for TM02/TE16 reg's */
# define RP  (M0+0x400)  /*  base for RP06 reg's, drive 0  */
/*		*/
# define TM_tc  (TM+0x24)  /*  TM02 tape control reg */
# define TM_cs1  (TM+0)  /*  TM02 control 1 reg */
# define TM_ds  (TM+4)  /*  status reg */
# define TM_fc  (TM+0x14)  /*  TM02 frame count reg */
/*		*/
# define RP_cr  0  /* RP06 control reg offset, longword */
# define RP_sr  1  /*  RP06 status reg offset */
# define RP_stk  5  /*  RP06 sector/track reg offset */
# define RP_off  011  /*  RP offset reg */
# define RP_cyl  10  /*  RP06 cylinder reg offset */
/*		*/
# define RP_GO  1  /*  go bit */
# define RP_RD	070	/* RP06 read function code */
# define RP_WR  060  /*  RP06 write function code */
# define RP_DC  010  /*  drive clear function code */
# define RP_FMT  0x1000  /*  format bit in RP offset reg */
# define RP_RIP  020  /*  Read-in Preset function code */
# define RP_MOL  0x1000  /*  medium online bit in status */
# define RP_DRY  0200  /*  drive ready, status reg */
# define RP_ERR 040000  /* composite error, status reg */
/*		*/
# define TM_GO  1  /*  TM02 go bit */
# define TM_DCLR  010  /*  TM02 drive clear function code */
# define TM_SFWD  030  /*  space forward function code */
# define TM_WRTF	060	/* write forward function code */
# define TM_REDF  070  /*  read forward function code */
# define TM_RWND  06  /*  TM02 Rewind  */
# define TM_WEOF	026	/* write end of file function */
# define TCHAR 01300  /*  TM02/TE16 drive 0 ,800 BPI,PDP11,etc. */
# define TM_DRDY  0200  /*  TM02/drive ready,status reg */
# define TM_ERR 040000  /*  TM02 composite error, status reg */
/*		*/
# define RXCS	32  /*  receiver control/staus */
# define RXDB  33  /*  receiver data */
# define TXCS  34  /*  transmitter control/status */
# define TXDB  35  /*  transmitter data */
# define RXCS_DONE  0x80  /*  receiver done */
# define TXCS_RDY  0x80  /*  transmitter ready */
/*		*/
# define BUFSIZ 5120  /*  max tape block size = input buffer size */
# define MAXUNI  1
# define NL  012
# define CR 015
/*		*/
char input[BUFSIZ]  ;  /*  tape input buffer */
struct { int reg ; } ;
int blksiz ,tapoff , dskoff ,  wsize , dblks ;
int count;
unsigned int ins , outs ;
unsigned short nread  , bytoff , error ;
int *RPptr , dunit ; /* ptr to start of RP reg's for desired drive */
main() {
/*
*  Stand-alone program to copy TM02/TE16 drive 0 mag tape to
*  RP06 disc.
*  User specifies tape block size (must be multiple of 512), tape
*  offset (tape blocks), disc unit no., disc offset (512-byte
*  blocks) and count of tape blocks to be transferred.
*/
int getcon() , putstr() , a2l() , l2a() ;

putlin("d2tcpy : Disk-to-Tape Copy") ;
putnl() ;
 
bsize :
if (putstr("tape block size : ")) goto fini ;
if (getcon(input)) goto fini ;
putnl() ;
blksiz = a2l(input) ;
if ((blksiz <= 0) || (blksiz > BUFSIZ)) goto bsize ;
if (blksiz%512) goto bsize ;  /*  block size must be 512 multiple */
M0_cr->reg = MBAinit ; /* MBA 0 init */
M1_cr->reg = MBAinit ; /* MBA 1 init */
 
TM_tc->reg = TCHAR ; /* TE16,drive 0 ,800 BPI, PDP11, odd parity,
			abort on error */
TM_cs1->reg = TM_DCLR | TM_GO ; /* clear tape drive 0 */
if (terror()) goto taperr ;
 
toff :
if (putstr("tape offset : ")) goto fini ;
if (getcon(input)) goto fini ;
putnl() ;
tapoff = a2l(input) ;
if (tapoff < 0) goto toff ;
if (taprew()) goto taperr ;
if (tapspc(tapoff)) goto taperr ;
 
dun :
if (putstr("disk unit : ")) goto fini ;
if (getcon(input)) goto fini ;
putnl() ;
dunit = a2l(input) ;
if ((dunit > MAXUNI) || (dunit < 0)) goto dun ;
 
doff :
if (putstr("disc offset : ")) goto fini ;
if (getcon(input)) goto fini ;
putnl() ;
dskoff = a2l(input) ;
if ((dskoff < 0) || (dskoff > MAXSEC-1)) goto doff ;
 
gknt :
if (putstr("no. of input blocks : ")) goto fini ;
if (getcon(input)) goto fini ;
putnl() ;
count = a2l(input) ;
if (count < 0) goto gknt ;
 
error = 0 ;
 
if (init()) goto bsize ;
 
while ((error == 0) && (count>0)) {
	if (dread()) {
		putlin("disc i/o error") ;
		goto ioerr ;
		}
	ins++ ;  /*  count of disk blocks input */
	if (twrite()) {
		taperr :
			putlin("tape i/o error") ;
			ioerr :
				error++ ;
				continue ;
			}
	outs =+ dblks ;  /*  count of tape blocks output */
	count -= nread ;/*  dec no. blocks read */
}
 
fini :
TM_cs1->reg = TM_WEOF | TM_GO;
twait();
TM_cs1->reg = TM_RWND | TM_GO;
twait();
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
 
taprew()
{
/*
*  Function to rewind TM02/TE16 drive
*/
TM_cs1->reg = TM_RWND | TM_GO ;
twait() ;
if (terror()) return(-1) ;
return(0) ;
}
 
/*		*/
 
tapspc(fblk)
{
/*
*  Function to space forward 'fblk' blocks on TM02/TE16 drive
*/
if (fblk) {
	TM_fc->reg = (-fblk) ; /* no. blocks */
	TM_cs1->reg = TM_SFWD | TM_GO ;   /* space forward */
	twait() ;
	if (terror()) return(-1) ;
	}
return(0) ;
}
 
/*		*/
 
twait()
{
/*
*  Function to wait until TM02/TE16 is not busy
*/
while ((TM_ds->reg & TM_DRDY) == 0) ;
}
 
/*		*/
 
terror()
{
/*
*  Function to check for TM02 error
*  Return (-1) if error,
*	esle return (0).
*/
if (TM_ds->reg & TM_ERR) return(-1) ;
return(0) ;
}
 
/*		*/
 
init() {
/*
*  Initialization.
*  Initialize MBA 0 (disk) and MBA 1 (tape).
*  Initialize TM02/TE16 for drive 0 , 800 BPI, PDP11, etc.
*  Set up MBA 0 and MBA 1 map registers to map a max.
*    transfer of 'BUFSIZ' bytes.
*/
register int *mp0 , *mp1 , i , page ;
 
 
nread = BUFSIZ/blksiz ; /* no. of tape reads to fill input buffer */
wsize = nread * blksiz ; /* no. bytes each write to disc */
dblks = wsize/512 ; /* no. of disc blocks on each write */
RPptr = RP + (dunit * 32 * 4) ; /* start of RP reg's for drive */
if ((*(RPptr+RP_sr) & RP_MOL) == 0) {
	putlin("unit not online") ;
	return(-1) ;
	}
 
*(RPptr+RP_cr) = RP_RIP | RP_GO ; /* preset :/
*(RPptr+RP_off) = RP_FMT ; /* set format bit */
bytoff = (int)(&input[0])&0777 ; /* byte offset of buffer addr */
page = ((int)&input[0] >> 9) & 07777777 ; /* start page of buffer */
mp0 = M0_map ; /* phys addr of MBA 0 map reg base */
mp1 = M1_map ; /*  "	"	"  1	"	" */
 
i = (((BUFSIZ+511)>>9)&0777) + 1 ; /* max. no. of pages */
 
while (i--)
	(*mp0++) = (*mp1++) = 0x80000000 | page++ ; /* map entry */
 
}
 
/*		*/
 
twrite()
{
/*
*  Function to write TM02/TE16 tape drive, 'blksiz' bytes each
*  write, and 'nread' writes.
*/
register int i, j ;
 
j = 0;
for (i = MIN(nread,count) ; i ; i--) {
	M1_var->reg = bytoff + j++ * blksiz;
	M1_bc->reg = (-blksiz) ; /* MBA 1 byte count reg */
	TM_fc->reg = (-blksiz);	/* tape frame counter register */
	TM_cs1->reg = TM_WRTF | TM_GO ; /* write forward */
	twait() ; /* wait for ready */
	if (terror()) return(-1) ; /* return on error */
	}
return(0) ; /* normal return */
}
 
/*		*/
 
dread()
{
/*
*  Function to read 'wsize' bytes (512 multiple) from disc
*  to buffer 'input[]".
*/
register int i , j ;
 
*(RPptr+RP_cr) = RP_DC | RP_GO ; /* drive clear */
*(RPptr+RP_cyl) = dskoff/RP6ST ; /* cylinder no. */
i = dskoff%RP6ST ;
j = (i/RP6SEC)<<8 ; /* track */
*(RPptr+RP_stk) = j | (i%RP6SEC) ; /* sector : track */
M0_bc->reg = (count<nread?-(count*512):(-wsize)) ; /* byte count */
M0_var->reg = bytoff ; /* virt addr reg = map no. + byte off */
*(RPptr+RP_cr) = RP_RD | RP_GO ; /* write */
 
dskoff =+ dblks ;  /*  point to next disc sector */
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
