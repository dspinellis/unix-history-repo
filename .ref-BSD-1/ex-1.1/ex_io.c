#include "ex.h"
#include "ex_re.h"
#include "ex_io.h"
/*
 * Ex - a text editor
 * Bill Joy UCB June 1977
 */

extern	int tfile -1;

#define	READ	0
#define	WRITE	1

STATIC	char ichanged;
STATIC	int nleft;
STATIC	int ninbuf;
STATIC	int tline;

STATIC	long cntch, cntnull, cntodd;
STATIC	int cntln;

STATIC	char ibuff[512];
STATIC	int iblock;
STATIC	char obuff[512];
STATIC	int oblock;

STATIC	char tfname[40];
STATIC	char tftail[] "/ExXXXXX";
STATIC	char havetmp;

fileinit()
{
	register char *p;
	register int i, j;
	struct stb stbuf;
	int serrno;
	char dumbcnt;

	cleanup();
	close(tfile);
	tline = 0200 * 3;
	blocks[0] = 1;
	blocks[1] = 2;
	blocks[2] = -1;
	dirtcnt = 0;
	iblock = -1;
	oblock = -1;
	strcpy(tfname, value(DIRECTORY));
	if (stat(tfname, &stbuf)) {
dumbness:
		serrno = errno;
		lprintf("PANIC: \"%s\":", tfname);
		flush();
		dumbcnt = 0;
		setexit();
		if (dumbcnt == 0) {
			errno = serrno;
			dumbcnt++;
			ioerror();
		}
		exit(1);
	}
	if ((stbuf.flags & FILETYP) != FDIRECT) {
		errno = ENOTDIR;
		goto dumbness;
	}
	ichanged = 0;
	strcat(tfname, tftail);
	for (p = strend(tfname), i = 5, j = getpid(); i > 0; i--, j =/ 10)
		*--p = j % 10 | '0';
	tfile = creat(tfname, 0600);
	if (tfile < 0)
		goto dumbness;
	havetmp = 1;
	close(tfile);
	tfile = open(tfname, 2);
	if (tfile < 0)
		goto dumbness;
	brk(fendcore);
}

cleanup()
{
	if (havetmp)
		unlink(tfname);
	havetmp = 0;
}

onhup()
{

	if (chngflag == 0 || preserve())
		cleanup();
	exit(1);
}

clrstats()
{

	ninbuf = 0;
	cntch = 0;
	cntln = 0;
	cntnull = 0;
	cntodd = 0;
}

iostats()
{

	close(io);
	io = -1;
	if (value(HUSH) == 0) {
		if (value(TERSE))
			printf(" %d/%ld", cntln, cntch);
		else
			printf(" %d line%s, %ld character%s", cntln, plural(cntln), cntch, cntch == 1 ? "" : "s");
		if (cntnull || cntodd) {
			printf(" (");
			if (cntnull) {
				printf("%ld null", cntnull);
				if (cntodd)
					printf(", ");
			}
			if (cntodd)
				printf("%ld dirty", cntodd);
			putchar(')');
		}
		putnl();
	}
	return (cntnull != 0 || cntodd != 0);
}

plural(i)
	int i;
{

	return (i == 1 ? "" : "s");
}

getline(tl)
	int tl;
{
	register char *bp, *lp;
	register nl;
	char scratch[LBSIZE];

	lp = scratch;
	bp = getblock(tl, READ);
	nl = nleft;
	tl =& ~0177;
	while (*lp++ = *bp++)
		if (--nl == 0) {
			bp = getblock(tl =+ 0200, READ);
			nl = nleft;
		}
	return (unpack(scratch));
}

putline()
{
	register char *bp, *lp;
	register nl;
	int tl;
	char scratch[LBSIZE];

	dirtcnt++;
	lp = scratch;
	pack(lp);
	change();
	tl = tline;
	bp = getblock(tl, WRITE);
	nl = nleft;
	tl =& ~0177;
	while (*bp++ = *lp++) {
		if (--nl == 0) {
			bp = getblock(tl =+ 0200, WRITE);
			nl = nleft;
		}
	}
	nl = tline;
	tline =+ (((lp - scratch) + 07) >> 2) & 077776;
	return (nl);
}

STATIC	char *nextip;

getfile()
{
	register c;
	register char *lp, *fp;

	lp = linebuf;
	fp = nextip;
	do {
		if (--ninbuf < 0) {
			ninbuf = read(io, genbuf, LBSIZE) - 1;
			if (ninbuf < 0) {
				if (lp != linebuf) {
					printf("[Incomplete last line] ");
					break;
				}
				return (EOF);
			}
			fp = genbuf;
		}
		if (lp >= &linebuf[LBSIZE])
			error(" Line too long@- limit 512 characters");
		c = *fp++;
		if (c == 0) {
			cntnull++;
			continue;
		}
		if (c & 0200) {
			cntodd++;
			c =& 0177;
			if (c == 0)
				continue;
		}
		*lp++ = c;
	} while (c != '\n');
	cntch =+ lp - linebuf;
	*--lp = 0;
	nextip = fp;
	cntln++;
	return (0);
}

putfile()
{
	int *a1;
	register char *fp, *lp;
	register int nib;

	a1 = addr1;
	clrstats();
	cntln = addr2 - a1 + 1;
	if (cntln == 0)
		return;
	nib = 512;
	fp = genbuf;
	do {
		lp = getline(*a1++);
		for (;;) {
			if (--nib < 0) {
				if (write(io, genbuf, nib = fp-genbuf) != nib)
					wrerror();
				cntch =+ nib;
				nib = 511;
				fp = genbuf;
			}
			if ((*fp++ = *lp++) == 0) {
				fp[-1] = '\n';
				break;
			}
		}
	} while (a1 <= addr2);
	if (write(io, genbuf, nib = fp-genbuf) != nib)
		wrerror();
	cntch =+ nib;
}

wrerror()
{

	if (eq(file, savedfile) && value(EDITED))
		change();
	ioerror();
}

ioerror()
{

	switch (errno) {
		case ENOENT:
			error(" No such file or directory");
		case EIO:
			error(" Physical I/O error");
		case EACCESS:
			error(" Permission denied");
		case ENOTDIR:
			error(" Not a directory");
		case EISDIR:
			error(" Is a directory");
		case ENFILE:
			error(" File table overflow");
		case ENOSPC:
			error(" No space left on device");
		case EQUOT:
			error(" Quota exceeded");
		case EROFS:
			error(" Read-only file system");
		default:
			error(" I/O error %d", errno);
	}
}

int	read();
int	write();

getblock(atl, iof)
	int atl;
	int (*iof)();
{
	register int bno, off;
	
	bno = (atl >> 7) & 0777;
	off = (atl << 2) & 0770;
	if (bno >= 506)
		error(" Tmp file too large");
	nleft = 512 - off;
	if (bno == iblock) {
		ichanged =| iof;
		return (ibuff + off);
	}
	if (bno == oblock)
		return (obuff + off);
	if (iof == READ) {
		if (ichanged)
			blkio(iblock, ibuff, write);
		ichanged = 0;
		iblock = bno;
		blkio(bno, ibuff, read);
		return (ibuff + off);
	}
	if (oblock >= 0)
		blkio(oblock, obuff, write);
	oblock = bno;
	return (obuff + off);
}

blkio(b, buf, iofcn)
	int (*iofcn)();
{

	seek(tfile, b, 3);
	if ((*iofcn)(tfile, buf, 512) != 512)
		ioerror();
}

synctmp()
{
	register int *bp, *a, cnt;

	if (ichanged)
		blkio(iblock, ibuff, write);
	ichanged = 0;
	if (oblock != -1)
		blkio(oblock, obuff, write);
	time(&header.Atime);
	header.Auid = getuid() & mask;
	*zero = header.Atime[1];
	for (a = zero, bp = blocks; a <= dol; a =+ 256, bp++) {
		if (*bp < 0) {
			tline = (tline + 0177) &~ 0177;
			*bp = ((tline >> 7) & 0777);
			tline =+ 0200;
			oblock = *bp + 1;
			bp[1] = -1;
		}
		seek(tfile, *bp, 3);
		cnt = (dol - a + 2) << 1;
		if (cnt > 512)
			cnt = 512;
		if (write(tfile, a, cnt) != cnt) {
oops:
			*zero = 0;
			printf("SYNC error:");
			ioerror();
		}
		*zero = 0;
	}
	header.Alines = dol - zero;
	seek(tfile, 0, 3);
	if (write(tfile, &header, sizeof header) != sizeof header)
		goto oops;
/*
	seek(tfile, 504, 0);
	write(tfile, "LOST", 5);
*/
}

TSYNC()
{

	if (dirtcnt > 12) {
		synctmp();
		dirtcnt = 0;
	}
}
