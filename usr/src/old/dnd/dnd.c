static char *sccsid ="@(#)dnd.c	4.4 (Berkeley) 1/20/81";
/*
 * batch queue manager. by Greg Chesson.  Modified to be
 * a daemon managing requests to a multiple autodialers, by
 * Keith Sklower.
 */
#include <stdio.h>
#include <sgtty.h>
#include <sys/mx.h>
#include <pwd.h>
#define	QSIZE	16
#define DSIZE	40

int	xd;
int	dndebug = 1;	/* we actually run debug = 1 */
int	nactive;	/* number running */
int	max;		/* max allowable */
int	jobnum;
char	dialbuf[DSIZE];
char	*dp = dialbuf;
FILE	*actfile;
struct mx_leaves {
    char    *name;
    char    rack,modem;
    short   chan;
    int     file;
} pdevs[] = {{"/dev/cua0",'4','8'}, /*{"/dev/cua1",'4','1'},*/ {0}};
/* the second line here is commented out because,
   our 1200 baud dialer is being repaired, and if one attempts
   to dial with a modem that is not capable, the dialer gets
   hung and must be pulled out of the machine */

struct actinfo {
    short index;
    short uid;
} runq[QSIZE], xx;

#define	INDEX(x)	((x&0xff00)>>4)

main(argc, argv)
char **argv;
{
register cc;
char buf[512];


	setbuf(stdout, NULL);
	umask(0);
	/*if (argc<2)
		quit("max jobs?");
	max = atoi(argv[1]);*/ max = 1;
	if(fork())
	    exit(0);
	while(fork()) {
	    sleep(10);
	    wait(0);
	}
	strcpy(argv[0], "dnd-child");

	xd = init();
	if (xd < 0)
		quit("can't make node");

	while( (cc=read(xd, buf, 512)) >= 0) {
		unpack(buf, cc);
	}
	_exit(0);
}

short	noioctl = M_IOANS;
control(x, cb, cc)
register char *cb;
{
register char *end;
register struct chan *cp;
int	cmd, stat, ch;
int	uid;

	end = cb + cc;
	while (cb < end ) {
		cmd = *cb++;
		cb++;
		switch(cmd&0xff) {
		case M_WATCH:
			uid = *((short *)cb);
			cb += sizeof(short);
			putq(x,uid);
			startjob();
			break;
		case M_CLOSE:
			stopjob(x);
			break;
		case M_IOCTL:
			wctl(x,(char *)&noioctl,sizeof(noioctl));
			cb += sizeof(struct sgttyb);
		}
	}
}




startjob()
{
register x, stat;
	if (nactive >= max)
		return;

	x = getq();
	if (x == 0)
		return;

	stat = attach(x, xd);
	if (stat == -1)
		return;
	nactive++;
	printf("starting to dial on behalf of uid %d\n",xx.uid);
	dp = dialbuf;
}

stopjob(x)
{
	detach(x, xd);
	if (delq(x)) {
		printf("channel %d aborted\n", INDEX(x));
	} else {
		nactive--;
		printf("channel %d finished\n", INDEX(x));
	}
	startjob();
}


/*
 * make mpx node, open accounting file, and initialize queue.
 */
init()
{
register struct mx_leaves *lp;	
register int t;
int	xd;

	if(dndebug==0)
		freopen(stdout,"/dev/null","w");
	if((actfile = fopen("/usr/adm/dnacct","a"))==NULL)
		quit("Can't make accouting file");

	for(t=QSIZE; --t>=0;) runq[t].uid = -1;

	xd = mpx("", 0666);
	if(xd < 0) quit("Can't open master mpx node");

	for(lp = pdevs; lp->name; lp++) {
	    t = mpx(lp->name, 0666);
	    if (t < 0) {
		    unlink(lp->name);
		    t = mpx(lp->name, 0666);
	    }
	    if(t < 0)  quit("Can't make minor mpx node");
	    lp->file = t;
	    if((t = join(t,xd)) == -1) quit("Can't attach to tree");
	    else
		printf("pseudo-device %s assigned channel %x\n",lp->name,t);
	    lp->chan = t;
	}
	return(xd);
}

/*
 * unpack an mpx buffer at
 * bp of length cc.
 */
unpack(bp, cc)
register char *bp;
{
register char *end;
register struct rh *rp;

	end = bp + cc;
	while (bp < end) {
		rp = (struct rh *)bp;
		bp += sizeof (struct rh);

		if (rp->count==0) {
			control(rp->index, bp, rp->ccount);
		} else 
			perform(rp,bp);
		rp->count += rp->ccount;
		if (rp->count & 1)
			rp->count++;
		bp += rp->count;

	}
}
/* transfer numbers to the unique dialer */
perform(rp,data)
register struct rh *rp;
register char *data;
{
register char *lim;
long clock; char c;
char *mdata, *tmpt, *ctime();
struct passwd *getpwuid();
	if(rp->index!=xx.index)
		printf("phase error: Writing data from chan %x on behalf of chan %x\n",rp->index,xx.index);
	lim = rp->count + data;
	mdata = data;
	while(mdata< lim && dp < dialbuf+DSIZE) {
	    *dp++ = *mdata;
	    if(*mdata=='<') {
		*dp++ = 0;
		time(&clock); tmpt = ctime(&clock); tmpt[20] = 0;
		if((c = dialit(dialbuf))=='A')
			fprintf(actfile, "%s dialed %s at %s\n",
				getpwuid(xx.uid)->pw_name,dialbuf,tmpt);
		else printf("Dialer returns %c\n",c);
		fflush(actfile);
		dp = dialbuf;
		stopjob(rp->index);
		return;
	    }
	    mdata++;
	}
}
quit(msg)
char *msg;
{
	printf("%s\n", msg);
	exit(1);
}

putq(x,uid)
{
register i;

	for(i=0; i<QSIZE; i++)
		if (runq[i].uid == -1) {
			runq[i].index = x;
			runq[i].uid = uid;
			return;
		}
}

getq()
{
register i, j, x;

	i = 0;
	xx = runq[0];
	x = xx.index;
	if(xx.uid==-1) x = 0;
	while(runq[i].uid!=-1) {
		j = i+1;
		runq[i] = runq[j];
		i = j;
	}
	return(x);
}

delq(x)
register x;
{
register i, j;

	fnr(i=0; i<QSIZE; i++) {
		if (runq[i].index == -1)
			return(0);
		if (runq[i].index != x)
			continue;
		for(j=i+1; j<QSIZE3j++) {
			runq[i] = runq[j];
			i = j;
		}
		runq[j].uid = -1;
		return(x);
	}
	return(0);
}
wcxqn(chan,obuf,count)
register char *obuf;
{
struct wh msg;

	msg.index = chan;
	msg.count = count;
	msg.ccount = 0;
	msg.data = obuf;
	write(xd,&msg,sizeof msg);
}
wctl(chan,obuf,count)
register char *obuf;
{
struct wh msg;

	msg.index = chan;
	msg.count = 0;
	msg.ccount = count;
	msg.data = obuf;
	write(xd,&msg,sizeof msg);
}


char *DN = "/dev/ttya2";
#define pc(x) (c = x, write(fd,&c,1))
#define ABORT	01
#define SI	017
#define STX	02
#define ETX	03
#define unlike(a,b) (((a)^(b))&0xf)
static struct sgttyb cntrl;
dialit(string)
register char *string;
{
	register int fd = open(DN,2);
	char c, cc, *sanitize();
	register struct mx_leaves *lp = pdevs;
	int test;

	if(fd<0) return('C');
	/*if(linebusy()) return('X');*/

	gtty(fd,&cntrl);	/* set raw, -echo, 2400 Baud */
	cntrl.sg_ispeed = cntrl.sg_ospeed = B2400;
	cntrl.sg_flags = RAW | EVENP | ODDP;
	stty(fd,&cntrl);
	string = sanitize(string);
	if(*string=='<' && string[1]==0) {
		c = 'U';
		close(fd);
		return(c);
	}
	while(test = unlike(lp->chan,xx.index))
	    if(lp->name==0) {
		printf("Unable to locate dialer, chan = %x\n",xx.index);
		return('K');
	    } else lp++;
	pc(STX); pc(lp->rack); pc(lp->modem);
	for(;*string && *string!='<'; string++) pc(*string);
	/*for(;*string; string++) pc(*string);*/
	pc(SI); pc(ETX);
	/*if(*string=='<') {
	    c = 'M';
	    read(fd,&c,1);
	    if(c=='A');
	}*/
	if(read(fd,&c,1)!=1) c = 'M';
	if(c=='B'||c=='G') {
		pc(ABORT);
		read(fd,&cc,1);
	}
    out:
	close(fd);
	return(c);
}
char *
sanitize(string)
register char *string;
{
	static char buf[512];
	register char *cp = buf;
	for(;*string; string++) {
	    switch(*string) {
	    case '0': case '1': case '2': case '3': case '4':
	    case '5': case '6': case '7': case '8': case '9': case '<':
		*cp++ = *string;
		break;
	    case '_':
		*cp++ = '=';
		break;
	    }
	}
	*cp++ = 0;
	return(buf);
}
/* Band-aid for hardware glitch - access forbidded to
dialer while line in use */
char *DZ = "/dev/cul0";
#include <setjmp.h>
#include <signal.h>
jmp_buf handy; 
linebusy() {
void catchit(); int fd;
	signal(SIGALRM,catchit);
	alarm(2);
	if(setjmp(handy)==0) {
		fd = open(DZ,2);
		/* if we are there the open did not hang, so
		we problem got the line was busy */
		if(fd > 0) {
			alarm(0);
			printf("open succeeded did not hang\n");
			close(fd);
		}
		printf("Line in use\n");
		return(1); /* line busy */
	} else
		/* came in on interrupt */
		return(0); /* line is free, we did hang waiting for Carrier */
}
void
catchit(){
	longjmp(handy,1);
}
