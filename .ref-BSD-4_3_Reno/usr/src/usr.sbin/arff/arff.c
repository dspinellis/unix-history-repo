/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)arff.c	5.7 (Berkeley) 5/11/89";
#endif not lint

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/signal.h>
#include <sys/file.h>
#include <stdio.h>
#include "pathnames.h"

#define dbprintf printf

struct rt_dat {
	u_short	rt_yr:5;	/* year-1972 */
	u_short	rt_dy:5;	/* day */
	u_short	rt_mo:5;	/* month */
};

struct	rt_axent {
	char	rt_sent[14];
};

struct rt_ent {
	char	rt_pad;		/* unusued */
	u_char	rt_stat;	/* type of entry, or end of seg */
	u_short	rt_name[3];	/* name, 3 words in rad50 form */
	u_short	rt_len;		/* length of file */
	u_char	rt_chan;	/* only used in temporary files */
	char	rt_job;		/* only used in temporary files */
	struct	rt_dat rt_date;	/* creation date */
};

#define RT_TEMP		1
#define RT_NULL		2
#define RT_FILE		4
#define RT_PFILE	(0200|RT_FILE)	/* protected file */
#define RT_ESEG		8

#define RT_BLOCK	512	/* block size */
#define RT_DIRSIZE	31	/* max # of directory segments */

struct rt_head {
	short	rt_numseg;	/* # of segments available */
	short	rt_nxtseg;	/* # of next logical segment */
	short	rt_lstseg;	/* highest seg currently open */
	u_short	rt_entpad;	/* extra words/directory entry */
	short	rt_stfile;	/* block # where files begin */
};

struct	rt_dir {
	struct rt_head	rt_axhead;
	struct rt_ent	rt_ents[72];
	char		_dirpad[6];
};

#define rd_numseg rt_axhead.rt_numseg
#define rd_nxtseg rt_axhead.rt_nxtseg
#define rd_lstseg rt_axhead.rt_lstseg
#define rd_entpad rt_axhead.rt_entpad
#define rd_stfile rt_axhead.rt_stfile

typedef struct fldope {
	int	startad;
	int	count;
struct	rt_ent	*rtdope;
} FLDOPE;

FLDOPE *lookup();

#define	rt(p)	((struct rt_ent *) p )
#define	Ain1	03100
#define	Ain2	050
#define	flag(c)	(flg[('c') - 'a'])

char	*man = "rxtd";
char	zeroes[512];

extern char *val;
extern char table[256];
struct rt_dir rt_dir[RT_DIRSIZE] = {
	{
	{ 4, 0, 1, 0, 14 },
	{ { 0, RT_NULL, { 0, 0, 0 }, 486, 0 },
	  { 0, RT_ESEG } }
	}
};

struct rt_dir rt_nulldir = {
	{ 0, 0, 0, 0, 0 },
	{ { 0, RT_NULL, { 0, 0, 0 }, 0, 0 },
	  { 0, RT_ESEG } }
};

int	rt_entsiz;
int	rt_nleft;
struct rt_ent *rt_curend[RT_DIRSIZE];
int	floppydes;
int	dirdirty;
char	*rt_last;
char	*defdev = _PATH_FLOPPY;

char *opt = "vfbcm";

extern long lseek();
int	rcmd(), dcmd(), xcmd(), tcmd();

int	(*comfun)();
char	flg[26];
char	**namv;
int	namc;

main(argc, argv)
	char *argv[];
{
	register char *cp;

	if (argc < 2)
		usage();
	for (cp = argv[1]; *cp; cp++)
		switch (*cp) {

		case 'm':
		case 'v':
		case 'u':
		case 'w':
		case 'b':
			flg[*cp-'a']++;
			continue;
		case 'c':
			flag(c)++;
			dirdirty++;
			continue;

		case 'r':
			setcom(rcmd);
			flag(r)++;
			continue;

		case 'd':
			setcom(dcmd);
			flag(d)++;
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
	if (comfun == 0) {
		if (flag(u) == 0) {
			fprintf(stderr, "arff: one of [%s] must be specified\n",
				man);
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
	if (comfun != 0) {
		fprintf(stderr, "arff: only one of [%s] allowed\n", man);
		exit(1);
	}
	comfun = fun;
}

usage()
{
	fprintf(stderr, "usage: ar [%s][%s] archive files ...\n", opt, man);
	exit(1);
}

notfound()
{
	register i, n = 0;

	for (i = 0; i < namc; i++)
		if (namv[i]) {
			fprintf(stderr, "arff: %s not found\n", namv[i]);
			n++;
		}
	return (n);
}

tcmd()
{
	register char *de, *last;
	FLDOPE *lookup(), *dope;
	int segnum, nleft;
	register i;
	register struct rt_ent *rde;

	rt_init();
	if (namc != 0) {
		for (i = 0; i < namc; i++)
			if (dope = lookup(namv[i])) {
				rde = dope->rtdope;
				(void) rtls(rde);
				namv[i] = 0;
			}
		return;
	}
	for (segnum = 0; segnum != -1;
	  segnum = rt_dir[segnum].rd_nxtseg - 1) {
		last = rt_last + segnum*2*RT_BLOCK;
		for (de = ((char *)&rt_dir[segnum])+10; de <= last; 
		    de += rt_entsiz)
			if (rtls(rt(de))) {
				nleft = (last-de)/rt_entsiz;
#define ENTRIES "\n%d entries remaining in directory segment %d.\n"
				printf(ENTRIES, nleft, segnum+1);
				break;
			}
	}
}

rtls(de)
	register struct rt_ent *de;
{
	int month, day, year;
	char name[12], ext[4];

	switch (de->rt_stat) {

	case RT_TEMP:
		if (flag(v))
			printf("Tempfile:\n");
		/* fall thru...*/

	case RT_FILE:
	case RT_PFILE:
		if (!flag(v)) {
			sunrad50(name, de->rt_name);
			printf("%s\n", name);
			break;
		}
		unrad50(2, de->rt_name, name);
		unrad50(1, &(de->rt_name[2]), ext);
		day = de->rt_date.rt_dy;
		year = de->rt_date.rt_yr+72;
		month = de->rt_date.rt_mo;
		printf("%6.6s  %3.3s	%02d/%02d/%02d	%d\n",name,
			ext, month, day, year, de->rt_len);
		break;

	case RT_NULL:
		printf("%-25.9s	%d\n","<UNUSED>", de->rt_len);
		break;

	case RT_ESEG:
		return (1);
	}
	return (0);
}

xcmd()
{
	register char *de, *last;
	int segnum;
	char name[12];
	register int i;

	rt_init();
	if (namc != 0) {
		for (i = 0; i < namc; i++)
			if (rtx(namv[i]) == 0)
				namv[i] = 0;
		return;
	}
	for (segnum = 0; segnum != -1;
	     segnum = rt_dir[segnum].rd_nxtseg-1)
		for (last = rt_last+(segnum*2*RT_BLOCK),
		     de = ((char *)&rt_dir[segnum])+10; de <= last; 
		     de += rt_entsiz) {
			switch (rt(de)->rt_stat) {

			case RT_ESEG:
				break;	/* exit loop and try next segment */

			case RT_TEMP:
			case RT_FILE:
			case RT_PFILE:
				sunrad50(name,rt(de)->rt_name);
				(void) rtx(name);

			case RT_NULL:
			default:
				continue;
			}
			break;
		}
}

rtx(name)
	char *name;
{
	register FLDOPE *dope;
	FLDOPE *lookup();
	register startad, count;
	int file;
	char buff[512];


	if (dope = lookup(name)) {
		if (flag(v))
			(void) rtls(dope->rtdope);
		else
			printf("x - %s\n",name);

		if ((file = creat(name, 0666)) < 0)
			return (1);
		count = dope->count;
		startad = dope->startad;
		for( ; count > 0 ; count -= 512) {
			(void) lread(startad, 512, buff);
			(void) write(file, buff, 512);
			startad += 512;
		}
		(void) close(file);
		return (0);
	}
	return (1);
}

rt_init()
{
	static initized = 0;
	register char *de, *last;
	register i;
	int dirnum;
	char *mode;
	FILE *temp_floppydes;

	if (initized)
		return;
	initized = 1;
	if (flag(c)) {
		struct stat sb;
		char response[128];
		int tty;

		if (stat(defdev, &sb) >= 0 && (sb.st_mode & S_IFMT) == S_IFREG)
			goto ignore;
		tty = open(_PATH_TTY, O_RDWR);
#define SURE	"Are you sure you want to clobber the floppy? "
		(void) write(tty, SURE, sizeof (SURE));
		(void) read(tty, response, sizeof (response));
		if (*response != 'y')
			exit(50);
		(void) close(tty);
ignore:
		;
	}
	if (flag(c) || flag(d) || flag(r))
		mode = "r+";
	else
		mode = "r";
	if ((temp_floppydes = fopen(defdev, mode)) == NULL) {
		perror(defdev);
		exit(1);
	} else
		floppydes = fileno(temp_floppydes);
	if (!flag(c)) {
		if (lread(6*RT_BLOCK, 2*RT_BLOCK, (char *)&rt_dir[0]))
			exit(2);
		dirnum = rt_dir[0].rd_numseg;
		/* check for blank/uninitialized diskette */
		if (dirnum <= 0) {
			fprintf(stderr,"arff: bad directory format\n");
			exit(1);
		}
		if (dirnum > RT_DIRSIZE) {
			fprintf(stderr,"arff: too many directory segments\n");
			exit(1);
		}
		for (i = 1; i < dirnum; i++)
		    if (lread((6+2*i)*RT_BLOCK, 2*RT_BLOCK, (char *)&rt_dir[i]))
			exit(1);
	} else {
		dirnum = 1;
		if (flag(b)) {
			rt_dir[0].rd_numseg = 31;
			rt_dir[0].rd_stfile = 68;
			rt_dir[0].rt_ents[0].rt_len = 20480 - 68;
		}
	}

	rt_entsiz = 2*rt_dir[0].rd_entpad + 14;
	/*
	 * We assume that the directory entries have no padding.  This
	 * may not be a valid assumption, but there are numerous point
	 * in the code where it assumes it is an rt_ent structure and
	 * not an rt_entsiz sized structure.
	 */
	rt_entsiz = 14;
	rt_last = ((char *) &rt_dir[0]) + 10 + 1014/rt_entsiz*rt_entsiz; 
	rt_nleft = 0;
	
	for (i = 0; i < dirnum; i++) {
		last = rt_last + i*2*RT_BLOCK;
		for (de = ((char *)&rt_dir[i])+10; de <= last; de += rt_entsiz)
			if (rt(de)->rt_stat == RT_ESEG)
				break;
		rt_curend[i] = rt(de);
		rt_nleft += (last-de)/rt_entsiz;
	}
}

static FLDOPE result;

FLDOPE *
lookup(name)
	char *name;
{
	unsigned short rname[3];
	register char *de;
	int segnum;
	register index;

	srad50(name,rname);

	/* 
	 *  Search for name, accumulate blocks in index
	 */
	rt_init();
	for (segnum = 0; segnum != -1;
	     segnum = rt_dir[segnum].rd_nxtseg - 1)
	{
		index = 0;
		for (de=((char *)&rt_dir[segnum])+10; 
		     rt(de)->rt_stat != RT_ESEG; de += rt_entsiz)
			switch(rt(de)->rt_stat) {

			case RT_FILE:
			case RT_PFILE:
			case RT_TEMP:
				if(samename(rname,rt(de)->rt_name)) {
					result.count = rt(de)->rt_len * 512;
					result.startad = 512*
					    (rt_dir[segnum].rd_stfile + index);
					result.rtdope = (struct rt_ent *) de;
					return (&result);
				}

			case RT_NULL:
				index += rt(de)->rt_len;
			}
        }
	return ((FLDOPE *) 0);

}

static
samename(a, b)
	u_short a[], b[];
{
	return (*a == *b && a[1] == b[1] && a[2] == b[2] );
}

rad50(cp, out)
	register u_char *cp;
	u_short *out;
{
	register index, temp;

	for (index = 0; *cp; index++) {
		temp = Ain1 * table[*cp++];
		if (*cp!=0) {
			temp += Ain2 * table[*cp++];
			if(*cp!=0) 
				temp += table[*cp++];
		}
		out[index] = temp;
	}
}

#define reduce(x, p, q) (x = v[p/q], p %= q);

unrad50(count, in, cp)
	u_short *in;
	register char *cp;
{
	register i, temp;
	register u_char *v = (u_char *) val;
	
	for (i = 0; i < count; i++) {
		temp = in[i];
		reduce(*cp++, temp, Ain1);
		reduce(*cp++, temp, Ain2);
		reduce(*cp++, temp, 1);
	}
	*cp=0;
}

srad50(name, rname)
	register char *name;
	register u_short *rname;
{
	register index;
	register char *cp;
	char file[7], ext[4];

	/* 
	 * Find end of pathname
	 */
	for (cp = name; *cp++; )
		;
	while (cp >= name && *--cp != '/')
		;
	cp++;
	/* 
	 * Change to rad50
	 */
	for (index = 0; *cp; ) {
		file[index++] = *cp++;
		if (*cp == '.') {
			cp++;
			break;
		}
		if (index >= 6) {
			break;
		}
	}
	file[index] = 0;
	for (index = 0; *cp; ) {
		ext[index++] = *cp++;
		if (*cp == '.' || index >= 3)
			break;
	}
	ext[index]=0;
	rname[0] = rname[1] = rname[2] = 0;
	rad50((u_char *)file, rname);
	rad50((u_char *)ext, rname+2);
}

sunrad50(name, rname)
	u_short rname[];
	register char *name;
{
	register char *cp, *cp2;
	char ext[4];

	unrad50(2, rname, name);
	unrad50(1, rname + 2, ext);
	/*
	 * Jam name and extension together with a dot
	 * deleting white space
	 */
	for (cp = name; *cp++;)
		;
	--cp;
	while (*--cp == ' ' && cp >= name)
		;
	*++cp = '.';
	cp++;
	for (cp2 = ext; *cp2 != ' ' && cp2 < ext+3;)
		*cp++ = *cp2++;
	*cp=0;
	if (cp[-1] == '.')
		cp[-1] = 0;
}

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
		
/*
 * Logical to physical adress translation
 */
long
trans(logical)
	register int logical;
{
	register int sector, bytes, track;

	logical += 26*128;
	bytes = (logical&127);
	logical >>= 7;
	sector = logical%26;
	if(sector >= 13)
		sector = sector*2+1;
	else
		sector *= 2;
	sector += 26 + ((track = (logical/26))-1)*6;
	sector %= 26;
	return ((((track*26)+sector) << 7) + bytes);
}

lread(startad, count, obuff)
	register startad, count;
	register char *obuff;
{
	long trans();
	extern floppydes;
	register int size = flag(m) ? 512 : 128;
	int error = 0;
	extern int errno;

	rt_init();
	while ((count -= size) >= 0) {
		(void) lseek(floppydes, flag(m) ?
			(long)startad : trans(startad), 0);
		if (read(floppydes, obuff, size) != size) {
			error = errno;
			fprintf(stderr, "arff: read error block %d: ",
				startad/size);
			errno = error;
			perror("");
		}
		obuff += size;
		startad += size;
	}
	return (error);
}

lwrite(startad, count, obuff)
	register startad, count;
	register char *obuff;
{
	long trans();
	extern floppydes;
	register int size = flag(m) ? 512 : 128;

	rt_init();
	while ((count -= size) >= 0) {
		(void) lseek(floppydes, flag(m) ?
			(long)startad : trans(startad), 0);
		if (write(floppydes, obuff, size) != size)
			fprintf(stderr, "arff: write error block %d\n",
				startad/size);
		obuff += size;
		startad += size;
	}
}

rcmd()
{
	register int i;

	rt_init();
	if (namc > 0)
		for (i = 0; i < namc; i++)
			if (rtr(namv[i]) == 0)
				namv[i] = 0;
}

rtr(name)
	char *name;
{
	register FLDOPE *dope;
	register struct rt_ent *de;
	struct stat buf;
	register struct stat *bufp = &buf;
	int segnum;
	char type;

	if (stat(name, bufp) < 0) {
		perror(name);
		return (-1);
	}
	type = 'a';
	if (dope = lookup(name)) {
		/* can replace, no problem */
		de = dope->rtdope;
		if (bufp->st_size <= (de->rt_len * 512)) {
			printf("r - %s\n",name);
			toflop(name, bufp->st_size, dope);
			goto found;
		} else {
			de = dope->rtdope;
			type = 'r';
			de->rt_stat = RT_NULL;
			de->rt_name[0] = 0;
			de->rt_name[1] = 0;
			de->rt_name[2] = 0;
			*((u_short *)&(de->rt_date)) = 0;
			scrunch();
		}
	}
	/*
	 * Search for vacant spot
	 */
	for (segnum = 0; segnum != -1;
	     segnum = rt_dir[segnum].rd_nxtseg - 1)
	{
		for (de = rt_dir[segnum].rt_ents;
		    rt(de)->rt_stat != RT_ESEG; de++)
			if ((de)->rt_stat == RT_NULL) {
				if (bufp->st_size <= (de->rt_len*512)) {
					printf("%c - %s\n", type, name),
					mkent(de, segnum, bufp,name);
					goto found;
				}
				continue;
			}
	}
	if (type == 'r')
		printf("%s: no slot for file, file deleted\n",name);
	else
		printf("%s: no slot for file\n", name);
	return (-1);

found:
	if (dope = lookup(name)) {
		toflop(name, bufp->st_size, dope);
		return (0);
	}
	printf("%s: internal error, added then not found\n", name);
	return (-1);
}

mkent(de, segnum, bufp, name)
	register struct rt_ent *de;
	int segnum;
	register struct stat *bufp;
	char *name;
{
	struct tm *localtime();
	register struct tm *timp;
	register struct rt_ent *workp;
	int count;
	
	count = (((bufp->st_size -1) >>9) + 1);
	/* make sure there is room */
	if (de->rt_len == count)
		goto overwrite;
	if ((char *)rt_curend[segnum] == (rt_last + (segnum*2*RT_BLOCK))) {
		/* no entries left on segment, trying adding new segment */
		if (rt_dir[0].rd_numseg > rt_dir[0].rd_lstseg) {
			short newseg;
			register int i;
			int maxseg;
			short size;

			newseg = rt_dir[0].rd_lstseg++;
			rt_dir[newseg] = rt_nulldir;
			rt_dir[newseg].rd_nxtseg = rt_dir[segnum].rd_nxtseg;
			rt_dir[segnum].rd_nxtseg = newseg + 1;
			rt_dir[newseg].rd_entpad = rt_dir[0].rd_entpad;
			rt_dir[newseg].rd_numseg = rt_dir[0].rd_numseg;
			size = 0;
			maxseg = 0;
			for(i = newseg - 1; i >= 0; i--) {
				workp = rt_curend[i] - 1;
				if (workp->rt_stat != RT_NULL)
					continue;
				if (workp->rt_len < size)
					continue;
				size = workp->rt_len;
				maxseg = i;
			}
			size = 0;
			for (workp = &rt_dir[maxseg].rt_ents[0]; 
			    workp->rt_stat != RT_ESEG; workp++) {
				size += workp->rt_len;
			}
			workp--;
			rt_dir[newseg].rt_ents[0].rt_len = workp->rt_len;
			rt_dir[newseg].rd_stfile = 
			    rt_dir[maxseg].rd_stfile + size - workp->rt_len;
			workp->rt_len = 0;
			rt_curend[newseg] = &rt_dir[newseg].rt_ents[1];
			lwrite(6*RT_BLOCK, 2*RT_BLOCK, (char *)&rt_dir[0]);
			if (segnum != 0)
				lwrite((6+segnum*2)*RT_BLOCK, 2*RT_BLOCK,
				    (char *)&rt_dir[segnum]);
			lwrite((6+newseg*2)*RT_BLOCK, 2*RT_BLOCK,
			    (char *)&rt_dir[newseg]);
			segnum = newseg;
			de = &rt_dir[newseg].rt_ents[0];
		} else {
			fprintf(stderr, "All directory segments full on  %s\n",
				defdev);
			exit(1);
		}
	}	
	/* copy directory entries up */
	for (workp = rt_curend[segnum]+1; workp > de; workp--)
		*workp = workp[-1];
	de[1].rt_len -= count;
	de->rt_len = count;
	rt_curend[segnum]++;
	rt_nleft--;

overwrite:
	srad50(name,de->rt_name);
	timp = localtime(&bufp->st_mtime);
	de->rt_date.rt_dy = timp->tm_mday;
	de->rt_date.rt_mo = timp->tm_mon + 1;
	de->rt_date.rt_yr = timp->tm_year - 72;
	de->rt_stat = RT_FILE;
	de->rt_pad = 0;
	de->rt_chan = 0;
	de->rt_job = 0;
	lwrite((6+segnum*2)*RT_BLOCK, 2*RT_BLOCK, (char *)&rt_dir[segnum]);
}

toflop(name, ocount, dope)
	char *name;
	register FLDOPE *dope;
	long ocount;
{
	register file, n, startad = dope->startad, count = ocount;
	char buff[512];
	
	file = open(name, 0);
	if (file < 0) {
		fprintf(stderr, "arff: couldn't open %s\n",name);
		exit(1);
	}
	for( ; count >= 512; count -= 512) {
		(void) read(file, buff, 512);
		lwrite(startad, 512, buff);
		startad += 512;
	}
	(void) read(file, buff, count);
	(void) close(file);
	if (count <= 0)
		return;
	for (n = count; n < 512; n ++)
		buff[n] = 0;
	lwrite(startad, 512, buff);
	count = (dope->rtdope->rt_len*512-ocount)/512 ;
	if (count <= 0)
		return;
	for ( ; count > 0 ; count--) {
		startad += 512;
		lwrite(startad, 512, zeroes);
	}
}

dcmd()
{
	register int i;

	rt_init();
	if (namc)
		for (i = 0; i < namc; i++)
			if (rtk(namv[i])==0)
				namv[i]=0;
	if (dirdirty)
		scrunch();
}

rtk(name)
	char *name;
{
	register FLDOPE *dope;
	register struct rt_ent *de;
	FLDOPE *lookup();

	if (dope = lookup(name)) {
		printf("d - %s\n",name);
		de = dope->rtdope;
		de->rt_stat = RT_NULL;
		de->rt_name[0] = 0;
		de->rt_name[1] = 0;
		de->rt_name[2] = 0;
		*((u_short *)&(de->rt_date)) = 0;
		dirdirty = 1;
		return (0);
	}
	return (1);
}

scrunch()
{
	register struct rt_ent *de , *workp;
	register segnum;

	for (segnum = 0; segnum != -1;
	     segnum = rt_dir[segnum].rd_nxtseg - 1) {
		for (de = rt_dir[segnum].rt_ents; de <= rt_curend[segnum]; de++)
			if (de->rt_stat == RT_NULL &&
			    (de+1)->rt_stat == RT_NULL) {
				(de+1)->rt_len += de->rt_len;
				for (workp=de; workp<rt_curend[segnum]; workp++)
					*workp = workp[1];
				de--;
				rt_curend[segnum]--;
				rt_nleft++;
			}
		lwrite((6+segnum*2)*RT_BLOCK, 2*RT_BLOCK,
			(char *)&rt_dir[segnum]);
	}
	dirdirty = 0;
}
