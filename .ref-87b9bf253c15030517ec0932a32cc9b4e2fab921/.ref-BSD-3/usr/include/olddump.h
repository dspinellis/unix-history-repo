#define MAXSIZE	500		/* max size in blocks of dumped files */
#define NILIST	100		/* max files extracted at once */
#define BFACT	20		/* tape blocking factor */

int tden 1600;			/* tape density */
int tlen 2200;			/* tape length (feet) */

char *dump_cmd[] = {		/* default args for dump */
	"dump",
	"i",
	"/dev/rp0",
	0
};

char *rest_cmd[] = {		/* defaults for restor */
	"restor",
	"t",
	0
};

char	*tape	"/dev/rmt1";
char	dfile[]	"/dev/dtab";
char	tfile[]	"/tmp/dtmp";
char	name[100];

#define NDTAB	10
struct {
	char	dt_name[16];
	time_t	dt_date;
} dtab[NDTAB];

struct thdr {
	ino_t	isize;
	ino_t	maxi;
	daddr_t	fsize;
	time_t	cdate;
	time_t	ddate;
	long	tsize;
	int	nflg;
};

struct fhdr {
	short	xmagic;
	ino_t	xino;
	short	xmode;
	short	xnlink;
	short	xuid;
	short	xgid;
	daddr_t	xaddr;
	off_t	xsize;
	time_t	xatime;
	time_t	xmtime;
	time_t	xctime;
};
#define	FMAGIC	012345
#define	SMAGIC	031415

#define DAPTB	127	/* (BSIZE-2*sizeof(short))/sizeof(daddr_t)) */

FILE	*tmpf;

long
getsize()
{
	register c;
	long j;

	c = getc(tmpf);
	if(c == EOF)
		return((long)-1);
	if(c <= 253)
		return((long)c);
	if(c == 255)
		return((long)-1);
	j = 0;
	for(c=0;c<3;c++)
		j = (j<<8) + (getc(tmpf)&0377);
	return(j);
}

putsize(s)
long	s;
{
	if(s <= 253) {
		putc((char)s, tmpf);
		return;
	}
	putc(254, tmpf);
	putc((char)(s>>16), tmpf);
	putc((char)(s>>8), tmpf);
	putc((char)s, tmpf);
}
