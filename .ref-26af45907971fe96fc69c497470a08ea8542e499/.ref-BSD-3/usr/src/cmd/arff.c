#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <signal.h>
#include <stdio.h>
#define dbprintf printf
struct rt_dat {
unsigned short	int	rt_yr:5;	/*Year - 1972			*/
unsigned short	int	rt_dy:5;	/*day				*/
unsigned short	int	rt_mo:5;	/*month				*/
};
struct	rt_axent {
	char	rt_sent[14];
};

struct rt_ent {
	char  rt_pad;			/*unusued		     */
	char  rt_stat;			/*Type of entry, or end of seg*/
	unsigned short rt_name[3];	/*Name, 3 words in rad50 form */
	short rt_len;			/*Length of file	      */
	char  rt_chan;			/*Only used in temporary files*/
	char  rt_job;			/*Only used in temporary files*/
	struct rt_dat rt_date;		/*Creation Date			*/
};
#define RT_TEMP 1
#define RT_NULL 2
#define RT_FILE 4
#define RT_ESEG 8
#define RT_BLOCK 512
struct rt_head {
	short	rt_numseg;		/*number of segments available*/
	short	rt_nxtseg;		/*segment no of next log. seg */
	short	rt_lstseg;		/*highest seg currenltly open */
	unsigned short	rt_entpad;	/*extra words/dir. entry      */
	short	rt_stfile;		/*block no where files begin  */
};
struct	rt_dir {
	struct rt_head	rt_axhead;
	struct rt_ent	rt_ents[72];
	char	_dirpad[6];
};
extern struct rt_dir	rt_dir;
extern int		rt_entsiz;
extern int		floppydes;
extern char		*rt_last;
typedef struct fldope {
	int	startad;
	int	count;
struct	rt_ent	*rtdope;
} FLDOPE;
FLDOPE *lookup();
#define rt(p) ((struct rt_ent *) p )
#define Ain1 03100
#define Ain2 050
#define flag(c) (flg[(c) - 'a'])

char	*man	=	{ "rxtd" };

char zeroes[512];
extern char *val;
extern char table[256];
struct rt_dir	rt_dir = {{4,0,1,0,14},{0,RT_NULL,{0,0,0},494,0}, {0,RT_ESEG}};
int		rt_entsiz;
int		rt_nleft;
struct rt_ent	*rt_curend;
int		floppydes;
int		dirdirty;
char		*rt_last;
char		*defdev = "/dev/floppy";

char	*opt	=	{ "vf" };

int	signum[] = {SIGHUP, SIGINT, SIGQUIT, 0};
long	lseek();
int	rcmd();
int	dcmd();
int	xcmd();
int	tcmd();
int	(*comfun)();
char	flg[26];
char	**namv;
int	namc;
int	file;


main(argc, argv)
char *argv[];
{
	register char *cp;

	/*register i;
	for(i=0; signum[i]; i++)
		if(signal(signum[i], SIG_IGN) != SIG_IGN)
			signal(signum[i], sigdone);*/
	if(argc < 2)
		usage();
	cp = argv[1];
	for(cp = argv[1]; *cp; cp++)
	switch(*cp) {
	case 'm':
	case 'v':
	case 'u':
	case 'w':
		flg[*cp - 'a']++;
		continue;
	case 'c':
		{
#define SURE	"Are you sure you want to clobber the floppy?\n"
			int tty;
			char response[2];
			tty = open("/dev/tty",2);
			write(tty,SURE,sizeof(SURE));
			read(tty,response,2);
			if(*response!='y')
				exit(50);
			flag('c')++;
			close(tty);
		}
		dirdirty++;
		continue;

	case 'r':
		setcom(rcmd);
		flag('r')++;
		continue;

	case 'd':
		setcom(dcmd);
		flag('d')++;
		continue;

	case 'x':
		setcom(xcmd);
		continue;

	case 't':
		setcom(tcmd);
		continue;

	case 'f':
		defdev = argv[2];
		argv++;
		argc--;
		continue;


	default:
		fprintf(stderr, "arff: bad option `%c'\n", *cp);
		exit(1);
	}
	namv = argv+2;
	namc = argc-2;
	if(comfun == 0) {
		if(flg['u'-'a'] == 0) {
			fprintf(stderr, "arff: one of [%s] must be specified\n", man);
			exit(1);
		}
		setcom(rcmd);
	}
	(*comfun)();
	exit(notfound());
}

setcom(fun)
int (*fun)();
{

	if(comfun != 0) {
		fprintf(stderr, "arff: only one of [%s] allowed\n", man);
		exit(1);
	}
	comfun = fun;
}








usage()
{
	printf("usage: ar [%s][%s] archive files ...\n", opt, man);
	exit(1);
}



notfound()
{
	register i, n;

	n = 0;
	for(i=0; i<namc; i++)
		if(namv[i]) {
			fprintf(stderr, "arff: %s not found\n", namv[i]);
			n++;
		}
	return(n);
}



phserr()
{

	fprintf(stderr, "arff: phase error on %s\n", file);
}

mesg(c)
{

	if(flg['v'-'a'])
		if(c != 'c' || flg['v'-'a'] > 1)
			printf("%c - %s\n", c, file);
}

tcmd()
{
	register char *de;
	FLDOPE *lookup(), *dope;
	int nleft; register i;
	register struct rt_ent *rde;

	rt_init();
	if(namc==0)
		for(de=((char *)&rt_dir)+10; de <= rt_last; de += rt_entsiz) {
			if(rtls(rt(de))) {
				nleft = (rt_last - de) / rt_entsiz;
				printf("\n\n%d entries remaining.\n",nleft);
				break;
			}
		}
	else
		for(i = 0; i < namc; i++) {
			if(dope = lookup(namv[i])) {
				rde = dope->rtdope;
				rtls(rde);
				namv[i] = 0;
			}
		}
}
rtls(de)
register struct rt_ent *de;
{
	int month,day,year;
	char name[12], ext[4];

	if(flg['v'-'a'])
		switch(de->rt_stat) {
		case RT_TEMP:
			printf("Tempfile:\n");
		case RT_FILE:
			unrad50(2,de->rt_name,name);
			unrad50(1,&(de->rt_name[2]),ext);
			day = de->rt_date.rt_dy;
			year = de->rt_date.rt_yr + 72;
			month = de->rt_date.rt_mo;
			printf("%6.6s  %3.3s	%02d/%02d/%02d	%d\n",name,
				ext,month,day,year,de->rt_len);
			break;

		case RT_NULL:
			printf("%-25.9s	%d\n","<UNUSED>",de->rt_len);
			break;

		case RT_ESEG:
			return(1);
		}
	else {
		switch(de->rt_stat) {
		case RT_TEMP:
		case RT_FILE:
			sunrad50(name,de->rt_name);
			printf(name);putchar('\n');
			break;

		case RT_ESEG:
			return(1);

		case RT_NULL:
			;
		}
	}
	return(0);
}
xcmd()
{
	register char *de;
	char name[12];
	register int i;

	rt_init();
	if(namc==0)
		for(de=((char *)&rt_dir)+10; de <= rt_last; de += rt_entsiz) {
			sunrad50(name,rt(de)->rt_name);
			rtx(name);
		}

	else
		for(i = 0; i < namc; i++)
		if(rtx(namv[i])==0) namv[i] = 0;
}
rtx(name)
char *name;
{
	register FLDOPE *dope;
	FLDOPE *lookup();
	register startad, count;
	int file; char buff[512];


	if(dope = lookup(name)) {
		if(flg['v' - 'a'])
			rtls(dope->rtdope);
		else
			printf("x - %s\n",name);

		file = creat(name, 0666);
		if(file < 0) return(1);
		count = dope->count;
		startad = dope->startad;
		for( ; count > 0 ; count -= 512) {
			lread(startad,512,buff);
			write(file,buff,512);
			startad += 512;
		}
		close(file);
		return(0);
	}
	return(1);
}
rt_init()
{
	static initized = 0;
	register char *de;
	int mode;

	if(initized) return;
	initized = 1;
	if(flag('c') || flag('d') || flag('r'))
		mode = 2;
	else
		mode = 0;
	if((floppydes = open(defdev,mode)) < 0)
		dbprintf("Floppy open failed\n");
	if(flag('c')==0)
		lread(6*RT_BLOCK,2*RT_BLOCK,(char *)&rt_dir);

	rt_entsiz = 2*rt_dir.rt_axhead.rt_entpad + 14;
	rt_entsiz = 14;
	rt_last = ((char *) &rt_dir) + 10 + 1014/rt_entsiz*rt_entsiz;
	for(de=((char *)&rt_dir)+10; de <= rt_last; de += rt_entsiz) {
		if(rt(de)->rt_stat==RT_ESEG) break;
	}
	rt_curend = rt(de);
	rt_nleft = (rt_last - de) / rt_entsiz;
}

static FLDOPE result;
FLDOPE *
lookup(name)
char * name;
{
	unsigned short rname[3];
	register char *de;
	register index;

	srad50(name,rname);

	/*
	 *  Search for name, accumulate blocks in index
	 */
	rt_init();
	index = 0;
	for(de = ((char *) &rt_dir) + 10; de <= rt_last; de += rt_entsiz) {
		switch(rt(de)->rt_stat) {
		case RT_ESEG:
			return((FLDOPE *) 0);
		case RT_FILE:
		case RT_TEMP:
		if(samename(rname,rt(de)->rt_name))
			goto found;
		case RT_NULL:
			index += rt(de)->rt_len;
		}
	}
	return((FLDOPE *) 0);
found:	result.count = rt(de)->rt_len * 512;
	result.startad = 512 * (rt_dir.rt_axhead.rt_stfile + index);
	result.rtdope = (struct rt_ent *) de;
	return(&result);
}
static
samename(a,b)
unsigned short a[3],b[3];
{
	return( a[0]==b[0] && a[1]==b[1] && a[2]==b[2] );
}


rad50(cp,out)
register unsigned char *cp;
unsigned short *out;
{
	register index;
	register temp;

	for(index = 0;*cp; index++) {

		temp = Ain1 * table[*cp++];
		if(*cp!=0) {
			temp += Ain2 * table[*cp++];

			if(*cp!=0) 
				temp += table[*cp++];
		}

		out[index] = temp;
	}
}
#define reduce(x,p,q) \
	(x = v[p/q], p %= q);

unrad50(count,in,cp)
unsigned short *in;
register char *cp;
{
	register i, temp; register unsigned char *v = (unsigned char *) val;
	
	for(i = 0; i < count; i++) {
		temp = in[i];

		reduce (*cp++,temp,Ain1);
		reduce (*cp++,temp,Ain2);
		reduce (*cp++,temp,1);
	}
	*cp=0;
}

srad50(name,rname)
register char * name;
register unsigned short *rname;
{
	register index; register char *cp;
	char file[7],ext[4];
	/*
	 * Find end of pathname
	 */
	for(cp = name; *cp++; );
	while(cp >= name && *--cp != '/');
	cp++;
	/*
	 * Change to rad50
	 *
	 */
	for(index = 0; *cp; ){
		file[index++] = *cp++;
		if(*cp=='.') {
			cp++;
			break;
		}
		if(index>=6) {
			break;
		}
	}
	file[index] = 0;
	for(index = 0; *cp; ){
		ext[index++] = *cp++;
		if(*cp=='.' || index>=3) {
			break;
		}
	}
	ext[index]=0;
	rname[0] = 0;
	rname[1] = 0;
	rname[2] = 0;
	rad50((unsigned char *)file,rname);
	rad50((unsigned char *)ext,rname+2);
}
sunrad50(name,rname)
unsigned short rname[3];
register char *name;
{
	register char *cp, *cp2;
	char ext[4];

	unrad50(2,rname,name);
	unrad50(1,rname + 2,ext);
	/* Jam name and extension together with a dot
	   deleting white space */
	for(cp = name; *cp++;);--cp;  while(*--cp==' ' && cp>=name);
	*++cp = '.';cp++;
	for(cp2=ext; *cp2!=' ' && cp2 < ext + 3;) {
		*cp++ = *cp2++;
	}
	*cp=0;
	if(cp[-1]=='.') cp[-1] = 0;
}

static char *oval = " ABCDEFGHIJKLMNOPQRSTUVWXYZ$.@0123456789";
static char *val = " abcdefghijklmnopqrstuvwxyz$.@0123456789";
static char table[256] = {
29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 
29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 
0, 29, 29, 29, 27, 29, 29, 29, 29, 29, 29, 29, 29, 29, 28, 29, 
30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 29, 29, 29, 29, 29, 29, 
29, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 
16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 29, 29, 29, 29, 29, 
29, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 
16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 29, 29, 29, 29, 29, 
29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 
29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 
0, 29, 29, 29, 27, 29, 29, 29, 29, 29, 29, 29, 29, 29, 28, 29, 
30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 29, 29, 29, 29, 29, 29, 
29, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 
16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 29, 29, 29, 29, 29, 
29, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 
16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 29, 29, 29, 29 };
		
long trans(logical)
register int logical;
{
	/*  Logical to physical adress translation */
	register int sector, bytes, track;

	logical += 26 * 128;
	bytes = (logical & 127);
	logical >>= 7;
	sector = logical % 26;
	if(sector >= 13)
		sector = sector *2 +1;
	else
		sector *= 2;
	sector += 26 + ((track = (logical / 26)) - 1) * 6;
	sector %= 26;
	return( (((track *26) + sector) << 7) + bytes);
}
lread(startad,count,obuff)
register startad, count;
register char * obuff;
{
	long trans();
	extern floppydes;
	rt_init();
	if(flg['m'-'a']==0)
		while( (count -= 128) >= 0) {
			lseek(floppydes, trans(startad), 0);
			read(floppydes,obuff,128);
			obuff += 128;
			startad += 128;
		}
	else
		while( (count -= 512) >= 0) {
			lseek(floppydes,(long) (startad), 0);
			read(floppydes,obuff,512);
			obuff += 512;
			startad += 512;
		}
}
lwrite(startad,count,obuff)
register startad, count;
register char * obuff;
{
	long trans();
	extern floppydes;
	rt_init();
	if(flg['m'-'a']==0)
		while( (count -= 128) >= 0) {
			lseek(floppydes, trans(startad), 0);
			write(floppydes,obuff,128);
			obuff += 128;
			startad += 128;
		}
	else
		while( (count -= 512) >= 0) {
			lseek(floppydes,(long) (startad), 0);
			write(floppydes,obuff,512);
			obuff += 512;
			startad += 512;
		}
}

rcmd()
{
	register int i;

	rt_init();
	if(namc>0)
		for(i = 0; i < namc; i++)
			if(rtr(namv[i])==0) namv[i]=0;
	
	
}

rtr(name)
char *name;
{
	register FLDOPE *dope; register struct rt_ent *de;
	struct stat buf; register struct stat *bufp = &buf;

	if(stat(name,bufp)<0) return(1);
	if(dope = lookup(name)) {
		/* can replace, no problem */
		de = dope->rtdope;
		if(bufp->st_size <= (de->rt_len * 512))
			printf("r - %s\n",name),
			toflop(name,bufp->st_size,dope);
		else {
			printf("%s will not fit in currently used file on floppy\n",name);
			return(1);
		}
	} else {
		/* Search for vacant spot */
		for(de = rt_dir.rt_ents; (char *) de <= rt_last; de++) {
			switch((de)->rt_stat) {
			case RT_NULL:
				if(bufp->st_size <= (de->rt_len * 512)) {
					printf("a - %s\n",name),
					mkent(de,bufp,name);
					goto found;
				}
				continue;
			case RT_ESEG:
				return(3);
			}
		}
		return(5);
	}
found:	if(dope=lookup(name)) {
		toflop(name,bufp->st_size,dope);
		return(0);
	}
	return(7);

}
mkent(de,bufp,name)
register struct rt_ent *de;
register struct stat *bufp;
char *name;
{
	struct tm *localtime(); register struct tm *timp;
	register struct rt_ent *workp; int count;
	
	count = (((bufp->st_size -1) >>9) + 1);
						/* Make sure there is room */
	if(de->rt_len==count)
		goto overwrite;
	if(rt_nleft==0) {
		if(flg['o'-'a'])
			goto overwrite;
		fprintf(stderr,"Directory full on  %s\n",defdev);
		exit(1);
	}	
					/* copy directory entries up */
	for(workp = rt_curend+1; workp > de; workp--)
		*workp = workp[-1];
	de[1].rt_len -= count;
	de->rt_len = count;
	rt_curend++;
	rt_nleft--;
overwrite:
	srad50(name,de->rt_name);
	timp = localtime(&bufp->st_mtime);
	de->rt_date.rt_dy = timp->tm_mday + 1;
	de->rt_date.rt_mo = timp->tm_mon + 1;
	de->rt_date.rt_yr = timp->tm_year - 72;
	de->rt_stat = RT_FILE;
	de->rt_pad = 0;
	de->rt_chan = 0;
	de->rt_job = 0;
	lwrite(6*RT_BLOCK,2*RT_BLOCK,(char *)&rt_dir);

}

toflop(name,ocount,dope)
char *name;
register FLDOPE *dope;
long ocount;
{
	register file, n, startad = dope->startad, count = ocount;
	char buff[512];
	
	file = open(name,0);
	if(file < 0) {
		printf("arff: couldn't open %s\n",name);exit(1);}
	for( ; count >= 512; count -= 512) {
		read(file,buff,512);
		lwrite(startad,512,buff);
		startad += 512;
	}
	read(file,buff,count);
	close(file);
	if(count <= 0) return;
	for(n = count; n < 512; n ++) buff[n] = 0;
	lwrite(startad,512,buff);
	count = (dope->rtdope->rt_len * 512 - ocount) / 512 ;
	if(count <= 0) return;
	for( ; count > 0 ; count--) {
		startad += 512;
		lwrite(startad,512,zeroes);
	}

}
dcmd()
{
	register int i;

	rt_init();
	if(namc)
		for(i = 0; i < namc; i++)
			if(rtk(namv[i])==0) namv[i]=0;
	if(dirdirty)
		scrunch();
	
}
rtk(name)
char *name;
{
	register FLDOPE *dope;
	register struct rt_ent *de;
	FLDOPE *lookup();

	if(dope = lookup(name)) {
		printf("d - %s\n",name);
		de = dope->rtdope;
		de->rt_stat = RT_NULL;
		de->rt_name[0] = 0;
		de->rt_name[1] = 0;
		de->rt_name[2] = 0;
		* ((unsigned short *) & (de->rt_date)) = 0;
		dirdirty = 1;
		return(0);
	}
	return(1);
}
scrunch() {
	register struct rt_ent *de = rt_dir.rt_ents, *workp;
	for(de = rt_dir.rt_ents; de <= rt_curend; de++) {
		if(de->rt_stat==RT_NULL && de[1].rt_stat==RT_NULL) {
			(de+1)->rt_len += de->rt_len;
			for(workp = de; workp < rt_curend; workp++)
				*workp = workp[1];
			de--;
			rt_curend--;
			rt_nleft++;
		}
	}
	lwrite(6*RT_BLOCK,2*RT_BLOCK,(char *)&rt_dir);
}
