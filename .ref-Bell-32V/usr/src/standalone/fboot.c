# include "rel.h"
 
# define RP6CYL  815  /*  no. RP06 cylinders/pack */
# define RP6TRK  19  /*  no. tracks/cyl  */
# define RP6SEC  22  /*  no. sectors/track  */
# define RM3CYL 823  /*  RM03  */
#define RM3TRK 5
#define RM3SEC 32
 
# define RP6ST  (rptrk*rpsec)
# define MAXSEC  (rpcyl*rptrk*rpsec)  /*  sectors/pack  */
# define M0  0x20010000  /* phys addr MBA 0  */
# define M1  0x20012000  /*  phys addr MBA 1  */
# define M_cr  1  /*  MBA 0 control reg addr , longword offset */
# define M_sr 2  /* MBA 0 status reg offset */
# define M_map  0x200  /* start MBA 0 map reg's longword offset */
# define M_var  3  /* MBA 0 virt addr reg offset */
# define M_bc  4  /*  MBA 0 byte count reg offset */
# define MBAinit 01  /*  MBA init bit */
/*		*/
# define EXTREG  0x400  /*  base for external reg's on MBA  */
/*		*/
# define RP_cr  0  /* RP06 control reg offset, longword */
# define RP_sr  1  /*  RP06 status reg offset */
# define RP_stk  5  /*  RP06 sector/track reg offset */
# define RP_typ 06  /* drive type reg */
# define RP_off  011  /*  RP offset reg */
# define RP_cyl  10  /*  RP06 cylinder reg offset */
/*		*/
# define RP_GO  1  /*  go bit */
# define RP_RED  070  /*  RP06 read function code */
# define RP_DC  010  /*  drive clear function code */
# define RP_DRY  0200  /*  drive ready, status reg */
# define RP_ERR 040000  /* composite error, status reg */
# define RP_RIP 020  /* RP readin preset function code */
# define RP_FMT  0x1000  /* format bit in RP offset reg */
# define RP_MOL  0x1000  /* medium on-line bit in RP status reg */
/*		*/
# define RXCS	32  /*  receiver control/staus */
# define RXDB  33  /*  receiver data */
# define TXCS  34  /*  transmitter control/status */
# define TXDB  35  /*  transmitter data */
# define RXCS_DONE  0x80  /*  receiver done */
# define TXCS_RDY  0x80  /*  transmitter ready */
/*		*/
# define MAG410 0410
# define MAG411 0411
# define MAXUNI  1
# define NL  012
# define CR 015
# define CDEL 0x23
# define LDEL 0x40
# define BLKSIZ 512
# define HDRSIZ 040  /*  task header size  */
# define INOSIZ  64  /*  no. bytes per inode entry */
# define INOBLK  (BLKSIZ/INOSIZ)  /*  no. inodes/disk block */
# define NAMSIZ 14  /*  no. char's in dir name field */
# define DIRSIZ  16  /*  no. bytes/directory entry */
# define SLASH  057  /* '/'  */
# define NADDR  13  /* no. addr blocks in inode entry */
# define KSP 0
/*		*/
# define IFMT 0170000
# define IFDIR 040000
# define IFREG 0100000
# define ROOTINO  2  /*  root dir inode no. */

/*		*/
#define RP6typ  022  /*  RP06 */
#define RM3typ  024  /*  RM03 type */
struct dirent { short ino ; char dname[NAMSIZ] ; } ;
struct inod {
	short i_mode ;
	short fill1[3] ;
	int i_size ;
	char i_addr[40] ;
	int fill2[3] ;
	} ;
struct thdr {
	int hmagic ;
	int htsiz ;
	int hdsiz ;
	int hbsiz ;
	int hsmsiz ;
	int hentry ;
	int hrelb ;
	int hrflg ;
	} ;
int filsiz ;
char *entry ;
char buf[BLKSIZ] , *namep ;
char dbuf[BLKSIZ] ;
short mode ;
int addr[NADDR] ;
int *mbap ; /* MBA ptr */
int *RPptr ; /* RP06 ptr */
int rpsec,rpcyl,rptrk; /* get set up in 'init' for RP06 or RM03 */
char *bufloc;
int dtmp1[128] , dtmp2[128] , dtmp3[128] ;
 
/*
*  This program, '/fboot', is always read in by code in the RP
*  boot block (block 0). It boots in the file specified by the
*  user.
*/
 
main()
{
/* set stack ptr and move code up to high end of core */
reloc() ;
/* jump to re-located code */
asm("	.text") ;
asm("	.globl	_main1") ;
asm("	jmp	*$(_main1+2)") ;
asm("	.align	2") ;
}
 
/*		*/
 
main1()
{
char ntmp[10*NAMSIZ] ;
register int inum ;
register struct inod *inp ;
struct inod *getiblk() ;
struct dirent *dsearch() , *pde ;
int j ;
 
floop :
 
putnl() ;
putstr("file : ") ;
getcon(ntmp) ; /* null-terminated filename */
putnl() ;
namep = ntmp ;
inum = ROOTINO ; /* root dir inode no. */
 
while ((*namep) == SLASH) namep++ ;
if ((*namep) == '\0') goto floop ;
 
mbap = M0 ;
RPptr = M0 + EXTREG ;
 
if (init()) {
	putlin("init error") ;
	return(-1) ;
	}
 
giblk :
 
/* get disk block that has inode entry for inode no. 'inum' */
inp = getiblk(inum) ; /* 'cp' = ptr. to inode entry in 'buf[]' */
if (inp == (-1)) {
	ioerr :
		putlin("disk i/o error") ;
		goto floop ;
	}
 
/* Get 'mode' and 'addr[]' entries for selected inode & save off */
iexpand(inp) ;
 
/*  inode mode is in 'mode'.
*   block no.'s are in 'addr[]'.
*   If inode is a directory, search all its blocks for pathname
*    component pointed to by 'namep'.
*   If inode is regular file, load blocks of file into core
*    for execution.
*/
 
if ((j = mode&IFMT) == IFDIR) { /* Directory */
	if (*namep == '\0') goto floop ; /* no more pathname */
	pde = dsearch() ; /* search all directory blocks */
	if (pde == (-1)) goto ioerr ; /* i/o error */
	if (pde == 0) goto floop ; /* pathname component not found */
	/* Found pathname component (directory or file) - go get
	  its inode and continue search.
	*/
	inum = pde->ino ;
	goto giblk ;
	}
else {
	if (j == IFREG) { /* regular file */
		if (*namep != '\0') goto floop ; /* should have been
			last component in pathname */
		if (lodfil()) goto floop ; /* load file whose
		  inode 'addr' blocks are in 'addr[]' */
/*
*  Code to check for task header, move code down to 0,
*  get task entry address, clear core, etc.
*/
 
		fexec() ;
		goto floop ; /* returned from execution - go to loop */
		}
	}
goto floop ; /* wasn't directory or file */
}
 
/*		*/
 
struct inod *getiblk(in)
register int in ;
{
/*
*  Read in disk inode block which contains inode
*  no. 'in' - return ptr. to start of inode entry.
*/
register i ;
 
i = (in + (INOBLK*2) - 1)/INOBLK ;
if (dread(i,buf)) /*  read block 'i' into 'buf[]' */
	return(-1) ; /* i/o error */
return((int)buf + (((in-1) & (INOBLK-1))*INOSIZ)) ;
}
 
/*		*/
 
iexpand(ip)
register struct inod *ip ;
{
/*
*  Get mode, file size and block no.'s from disk inode and store
*  away.
*  'ip' = ptr. to inode entry.
*/
register int i ;
register char *f , *t ;
 
mode = ip->i_mode ;
filsiz = ip->i_size ;
f = (&ip->i_addr[0]) ;
t = (&addr[0]) ;
 
for (i = NADDR ; i ; i--) {
	(*t++) = (*f++) ;
	(*t++) = (*f++) ;
	(*t++) = (*f++) ;
	(*t++) = '\0' ;
	}
return(0) ;
}
 
/*		*/
 
struct dirent *dsearch()
{
/*
*  Search blocks of directory inode 'inum', whose block
*  no.'s are in 'addr[]', for pathname component pointed
*  to by 'namep'.
*  Return (-1) for error.
*  Return (0)  for no match.
*  Return ptr. to directory entry for match.
*/
register char c ;
register struct dirent *cp ;
register int i , j ;
struct dirent *pfind() ;
 
/*  direct  */
cp = pfind(addr,NADDR-3) ; /* search 'NADDR-3' blocks(in 'naddr[]') */
if (cp == (-1)) return(-1) ; /* i/o error */
if (cp == 0) return(0) ;  /* zero block encountered - no more blocks */
if (cp == 1) goto level1 ; /* more blocks to search */
/* 'cp' must be ptr. to matched directory */
found :
	/*  point to next pathname component */
while (((c = (*namep)) != '/') && (c !='\0')) namep++ ;
if (c == '/') while (*namep == '/') namep++ ;
return(cp) ;
 
/*
*/
 
level1 : /*  1st level indirection */
 
if ((j = addr[NADDR-3]) == 0) return(0) ;
if (dread(j,dtmp1)) return(-1) ; /* i/o error */
cp = pfind(dtmp1,128) ;
if (cp == (-1)) return(-1) ;
if (cp == 0) return(0) ;
if (cp == 1) goto level2 ;
goto found ;
 
level2 :
 
if ((j = addr[NADDR-2]) == 0) return(0) ;
if (dread(j,dtmp1)) return(-1) ;
for (i = 0 ; i < 128 ; i++) {
	if (dtmp1[i] == 0) return(0) ;
	if (dread(dtmp1[i],dtmp2)) return(-1) ;
	cp = pfind(dtmp2,128) ;
	if (cp == (-1)) return(-1) ;
	if (cp == 0) return(0) ;
	if (cp == 1) continue ;
	goto found ;
	}
 
level3 :
 
if ((j = addr[NADDR-1]) == 0) return(0) ;
if (dread(j,dtmp1)) return(-1) ;
for (i = 0 ; i < 128 ; i++) {
	if (dread(dtmp1[i],dtmp2)) return(-1) ;
	for (j = 0 ; j < 128 ; j++) {
		if (dread(dtmp2[i],dtmp3)) return(-1) ;
		cp = pfind(dtmp3,128) ;
		if (cp == (-1)) return(-1) ;
		if (cp == 0) return(0) ;
		if (cp == 1) continue ;
		goto found ;
		}
	}
return(0) ;
}
 
/*		*/
 
struct dirent *pfind(ia,knt)
int ia[] ;
register int knt ;
{
/*
*  'ia' :  array of integer block no.'s.
*  'knt' :  no. entries in 'ia'
*  A zero block in 'ia[]' -> no match -> return(0) .
*  Return (-1) on i/o error.
*  Return (1) if no zero blocks and all blocks searched but
*    no match.
*  Return ptr. to directory entry for match.
*/
 
register int *bp , bn ;
register struct dirent *ix ;
 
bp = ia ;
while (knt--) {
	if ((bn = (*bp++)) == 0) return(0) ;
	if (dread(bn,dbuf)) return(-1) ;
	for (ix = dbuf ; ix < &dbuf[BLKSIZ] ; ix++) {
		if (dnmatch(ix->dname,namep)) return(ix) ;
		}
	}
return(1) ;
}
 
/*		*/
 
dnmatch(p1,p2)
register char *p1 , *p2 ;
{
/*
*  'p1' : ptr. to directory filename field
*  'p2' : ptr. to null-terminated file pathname
*  Return(1) if match,
*   else, return(0).
*/
register int i ;
register char c1 , c2 ;
 
for (i = NAMSIZ ; i ; i--) {
	c1 = (*p1++) ;
	c2 = (*p2++) ;
	if (((c1 == '\0') && (c2 == '\0')) || (c2 == SLASH)) return(1) ;
	if (c1 != c2) return(0) ;
	}
if ((c2 == '\0') || (c2 == SLASH)) return(1) ;
return(0) ;
}
 
/*		*/
 
dread(dbn,cbuf)
char *cbuf ;
{
/*
*  Read physical block no. 'dbn' from disk and
*    load into array 'cbuf[]'.
*  Return (-1) for i/o error.
*  Else return (0).
*/
register int i , j ;
 
*(RPptr+RP_cr) = RP_DC | RP_GO ; /*  RP06 drive clear function code */
*(RPptr+RP_cyl) = dbn/RP6ST ; /* cylinder no. */
i = dbn%RP6ST ;
j = (i/rpsec)<<8 ; /* track */
*(RPptr+RP_stk) = j | (i%rpsec) ; /* sector : track */
*(mbap+M_bc) = (-BLKSIZ) ;
i = (int)cbuf&0777;
*(mbap+M_var) = i ;
*(mbap+M_map) = (i = 0x80000000 | ((int)((int)cbuf>>9)&07777777)) ;
*(mbap+M_map+1) = (++i) ;
*(RPptr+RP_cr) = RP_RED | RP_GO ; /* read */
 
dwait() ; /* wait for i/o to finish */
if (derror()) { /* error */
	putlin("fboot : disk read error");
	return(-1);
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
if (*(RPptr+RP_sr) & RP_ERR) return(-1) ;
return(0) ;
}
 
/*		*/
 
init()
{
/*
*  Initialization.
*/
register int i;
 
*(mbap+M_cr) = MBAinit ; /* MBA initialize */
if ((*(RPptr+RP_sr) & RP_MOL) == 0 ){
	putlin("unit not online") ;
	return(-1) ;
	}
*(RPptr+RP_cr) = RP_RIP | RP_GO ; /* readin preset */
*(RPptr+RP_off) = RP_FMT ; /* format bit in offset reg */
bufloc = 0 ;
i = *(RPptr+RP_typ)&0777 ;  /* get disk type */
if (i==RP6typ) { /* RP06 */
	rpsec=RP6SEC; rpcyl=RP6CYL; rptrk=RP6TRK;
	}
else {
	if (i==RM3typ) {
		rpsec=RM3SEC; rpcyl=RM3CYL; rptrk=RM3TRK;
		}
	else return(-1);
	}
return(0) ;
}
 
/*		*/
 
lodfil()
{
/*
*  Function to load a file into low core - disk blocks no.'s
*  which comprise file are in 'addr[]'.
*  Return (-1) if i/o error,
*  else return (0).
*/
 
register int i , j , k , n ;
int dtmp1[128] , dtmp2[128] ;
 
/*  direct  */
for (i = 0 ; i < NADDR-3 ; i++) {
	if ((j = addr[i]) == 0) return(0) ;
	if (dread(j,bufloc)) return(-1) ;
	bufloc += BLKSIZ ;
	}
 
level1 : /*  1st level indirection */
 
if ((j = addr[NADDR-3]) == 0) return(0) ;
/*  read in <=128 blocks into low core */
if ((k = r128(j)) < 0) return(-1) ; /* i/o error */
if (k > 0) return(0) ; /* no more blocks */
 
level2 :
 
if ((j = addr[NADDR-2]) == 0) return(0) ;
if (dread(j,dtmp1)) return(-1) ;
for (i = 0 ; i < 128 ; i++) {
	if ((j = dtmp1[i]) == 0) return(0) ;
	if ((k = r128(j)) < 0) return(-1) ;
	if (k > 0) return(0) ;
	}
 
level3 :
 
if ((j = addr[NADDR-1]) == 0) return(0) ;
if (dread(j,dtmp1)) return(-1) ;
for (i = 0 ; i < 128 ; i++) {
	if ((k = dtmp1[i]) == 0) return(0) ;
	if (dread(k,dtmp2)) return(-1) ;
	for (j = 0 ; j < 128 ; j++) {
		if ((k = dtmp2[j]) == 0) return(0) ;
		if ((n = r128(k)) < 0) return(-1) ;
		if (n > 0) return(0) ;
		}
	}
return(0) ;
}
 
/*		*/
 
r128(blk)
int blk ;
{
/*
*  Read in disk block no. 'blk' into buffer.
*  Then read in the 128 disk blocks whose block no.'s are
*  in the buffer into low core.
*  Stop on a zero block no. - return(1).
*  Return(-1) on i/o error.
*  Return(0) if all 128 blocks read.
*/
int btmp[128] ;
register int i , j ;
 
if (dread(blk,btmp)) return(-1) ;
 
for (i = 0 ; i < 128 ; i++) {
	if ((j = btmp[i]) == 0) return(1) ; /* no more blocks */
	if (dread(j,bufloc)) return(-1) ; /* i/o error */
	bufloc += BLKSIZ ; /* next core block loc */
	}
return(0) ;
}
 
/*		*/
 
halt() {
	asm("	halt") ;
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
	if (c==LDEL) {putc(CR);putc(0);putc(NL);cs=ocs; goto inloop;}
	if ((c == NL) || (c == CR)) {
		putc(CR) ;
		putc(0);
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
 
fexec()
{
/*
*  Part of stand-alone programs that load programs into low core
*  and execute them.
*  This function :
*	1) Checks if user specified file has a header - if so,
*	   move start of exec. file down to 0. If file is 0410,
*	   move data up to page boundary.
*	2) Clear core.
*	3) Jump to user task (calls).
*/
register struct thdr *hdp ;
register char *corep1 , * corep2 ;
register int k ;
int i , stxt , sdat , sbss , clrmin ;
register char *clrmax ;
 
hdp = 0 ;
clrmax = RELOC - 0x400 ; /* last addr+1 to clear */
i = hdp->hmagic ; /* task type code from task header */
entry = 0 ; /* default entry addr if no header */
clrmin = filsiz ;
if ((i != MAG410) && (i != MAG411)) goto clrcor ; /* NO HEADER */
/* file has task header */
entry = hdp->hentry & 017777777777 ;
stxt = hdp->htsiz ; /* no. of text bytes in file */
sdat = hdp->hdsiz ; /* no. data bytes */
sbss = hdp->hbsiz ; /* no. bytes in bss area */
filsiz = stxt + sdat ; /* file size = text + data */
clrmax = filsiz + sbss ; /* new upper limit to clear */
/*  move file down to loc 0 */
corep1 = 0 ;
corep2 = sizeof(struct thdr) ;
for (k = filsiz ; k ; k--)
	(*corep1++) = (*corep2++) ;
clrmin = stxt ;
/* If 0410 file , move data up to page boundary */
if ((i == MAG410) && sdat ) { /*  0410  */
	i = corep2 = ((stxt + 511) & 017777777000) ; /* page boundary */
	corep2 += sdat ; /* end+1 of new data area */
	corep1 = filsiz ; /* end+1 of current data area */
	for (k = sdat ; k ; k--) /* move data up */
		*(--corep2) = *(--corep1) ;
	clrmax += (i-stxt) ; /* adjust upper limit for moved data */
	clrmin = i + sdat ;
	}
/*  clear  core  */
clrcor :
for (corep1 = clrmin ; corep1 < clrmax ; corep1++)
	*corep1 = 0 ;
 
/*  execute  user  task  */
 
asm("	.globl	_entry") ;
asm("	calls	$0,*_entry") ;
 
/*		*/
 
return(0) ;
}
 
/*		*/
 
reloc()
{
extern edata ;
register int *to , *from , i ;
 
mtpr(KSP,RELOC) ; /* set stack */
from = 0 ;
to = RELOC ;
for (i = (int)&edata-RELOC ; i > 0 ; i -= (sizeof *from))
	(*to++) = (*from++) ;
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
