#
/*
 * fix - fix up data
 *
 * Author: Kurt Shoens UCB July, 1977
 *
 * Fix reads the concatenation of the files presented to
 * it and transcribes them to the standard output with unprintable
 * characters removed.  A report of the bad characters found is put
 * on the diagnostic output stream if desired.  The options of this
 * program are as follows:
 *
 * fix [ -crnhfqblo ] file ...
 *	c	carriage returns (015) are illegal
 *	r	a report is put on diagnostic output
 *	n	no output is put on standard output (report only)
 *	h	erase processing is done on control-h's
 *	f	form feeds (014) are illegal
 *	q	given multiple files, this suppresses printing of file name
 *		on report lines
 *	l	line numbers are printed on report lines.
 *	o	bad characters are printed in octal instead of as ?'s
 *
 * The n, l, and o flags turn on the r flag.
 */

#define CLEAN 1
#define REPORT 2
#define CTLH 4
#define CRILL 8
#define FFILL 16
#define VERBOSE 32
#define BELLILL 64
#define LNO 128
#define OCTAL 256

char buf[512],wbuf[512],*nxtchar;
int wcount,avail,eof,errs,linenum;
int FLAGS;

extern int fout;

main(argc,argv)
	char **argv;
{
	register fd;
	register char *cp;
	FLAGS = CLEAN | VERBOSE;
	if (*argv[1] == '-')
	{
		cp = *++argv;
		++cp;
		--argc;
		while (*cp)
		{
			switch (*cp)
			{
			case 'c':
				FLAGS =| CRILL;
				break;

			case 'r':
				FLAGS =| REPORT;
				break;

			case 'n':
				FLAGS =& ~CLEAN;
				FLAGS =| REPORT;
				break;

			case 'h':
				FLAGS =| CTLH;
				break;

			case 'f':
				FLAGS =| FFILL;
				break;

			case 'q':
				FLAGS =& ~VERBOSE;
				break;

			case 'b':
				FLAGS =| BELLILL;
				break;

			case 'l':
				FLAGS =| LNO | REPORT;
				break;

			case 'o':
				FLAGS =| OCTAL|REPORT;
				break;

			default:
				prs("Usage: fix [ -crnhfqblo ] [ file ... ]\n");
				errs++;
				exit(errs);
			}
			++cp;
		}
	}
	if (errs) exit(errs);
	if (argc == 1)
	{
		FLAGS =& ~VERBOSE;
		linenum = 0;
		eof = 0;
		fix(0,"standard input");
		flushout();
		exit(errs);
	}
	if (argc == 2) FLAGS =& ~VERBOSE;
	while (--argc)
	{
		fd = open(*++argv,0);
		if (fd < 0)
		{
			perror(*argv);
			errs++;
			continue;
		}
		nxtchar = buf + 512;
		eof = 0;
		avail = 0;
		linenum = 0;
		fix(fd,*argv);
		close(fd);
	}
	flushout();
}

fix(f,file)
	char *file;
{
	char line[512],*cp,ch;
	int count;
	while (!eof)
	{
		ch = bread(f);
		count = 0;
		cp = line;
		while (ch != 012 && ch != 015 && !eof && count < 512)
		{
			*cp++ = ch;
			count++;
			ch = bread(f);
		}
		++linenum;
		if (count >= 512)
		{
			prs("Line too long.\n");
			while (ch != 012 && ch != 015 && !eof) ch = bread(f);
			++errs;
		}
		*cp = 0;
		if (FLAGS & REPORT) printbad(line,file);
		if (FLAGS & CTLH) fixctlh(line);
		if (FLAGS & CLEAN)
		{
			cp = line;
			while (*cp) if (legalchar(*cp)) bwrite(*cp++);
			else ++cp;
			if (ch == 012) bwrite(012);
		}
	}
}

printbad(cp,file)
	char *cp,*file;
{
	char mine[512];
	register char *cp2;
	register bad,x;
	cp2 = mine;
	bad = 0;
	while (*cp)
		if (!legalchar(*cp))
		{
			if (FLAGS & OCTAL)
			{
				x = *cp;
				*cp2++ = '\\';
				*cp2++ = ((x & 0300) >> 6) | '0';
				*cp2++ = ((x & 0070) >> 3) | '0';
				*cp2++ = ((x & 0007)     ) | '0';
			}
			else *cp2++ = '?';
			++bad;
			++cp;
		} else *cp2++ = *cp++;
	*cp2++ = 012;
	if (bad)
	{
		if (FLAGS & VERBOSE)
		{
			prs(file);
			if (FLAGS & LNO) prs("/");
			else prs(":");
		}
		if (FLAGS & LNO)
		{
			prd(linenum);
			prs(":");
		}
		write(2,mine,cp2-mine);
	}
}

fixctlh(cp)
	char *cp;
{
	register char *np,*bp;
	bp = np = cp;
	while (*cp)
	{
		if (*cp == '\b')
		{
			++cp;
			if (bp != np) --np;
		}
		else *np++ = *cp++;
	}
	*np = 0;
}

bread(f)
{
	for (;;)
	{
		if (eof) return(-1);
		if (!avail)
		{
			avail = read(f,buf,512);
			nxtchar = buf;
		}
		if (!avail)
		{
			eof = 1;
			return(-1);
		}
		--avail;
		if (*nxtchar) return(*nxtchar++);
		++nxtchar;
		return(0177);
	}
}

bwrite(ch)
{
	wbuf[wcount++] = ch;
	if (wcount >= 512)
	{
		wcount = 0;
		if (write(1,wbuf,512) != 512)
		{
			prs("fatal write error.\n");
			exit(1);
		}
	}
}

flushout()
{
	if (write(1,wbuf,wcount) != wcount)
	{
		prs("fatal write error\n");
		exit(1);
	}
}

prs(str)
	char *str;
{
	register char *dart;
	dart=str;
	while (*dart) dart++;
	write(2,str,dart-str);
}

legalchar(ch)
	char ch;
{
	if (FLAGS & FFILL && ch == 014) return(0);
	if (FLAGS & CRILL && ch == 015) return(0);
	if (FLAGS & BELLILL && ch == 007) return(0);
	if (any(ch,"\t\014\015\007")) return(1);
	if (ch < 040 || ch > 0176) return(0);
	return(1);
}

any(c,cp)
	char c,*cp;
{
	while (*cp) if (c == *cp++) return(-1);
	return(0);
}

prd(x)
{
	char nbuf[10];
	int i,j,negf;
	if (!x)
	{
		prs("0");
		return;
	}
	i=8;
	nbuf[9] = 0;
	if (x < 0)
	{
		negf = 1;
		x = -x;
	}
	else negf = 0;
	while (x)
	{
		j = x % 10;
		nbuf[i--] = j + 060;
		x =/ 10;
	}
	if (negf) prs("-");
	prs(nbuf+ ++i);
}
