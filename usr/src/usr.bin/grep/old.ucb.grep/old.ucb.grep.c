#ifndef lint
static char sccsid[] = "@(#)old.ucb.grep.c	4.5 (Berkeley) %G%";
#endif

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
/*
 * grep -- print lines matching (or not matching) a pattern
 */

#define BLKSIZE 8192
#define	CCHR	2
#define	CDOT	4
#define	CCL	6
#define	NCCL	8
#define	CDOL	10
#define	CEOF	11

#define	CBRC	14
#define	CLET	15
#define	STAR	01

#define	ESIZE	256

char	expbuf[ESIZE];
long	lnum;
char	linebuf[BUFSIZ+1];
int	bflag;
int	nflag;
int	cflag;
int	vflag;
int	nfile;
int	iflag;
int	lflag;
int	wflag;
int	sflag;
int	nsucc;
int	circf;
int	blkno;
long	tln;

main(argc, argv)
char **argv;
{

	while (--argc > 0 && (++argv)[0][0]=='-') {
		char *cp = argv[0] + 1;
		while (*cp) switch (*cp++) {

		case 'v':
			vflag++;
			continue;

		case 'b':
			bflag++;
			continue;

		case 'i':
		case 'y':	/* -y for compatibility with btl grep */
			iflag++;
			continue;

		case 'l':
			lflag++;
		case 'c':
			cflag++;
			continue;

		case 'w':
			wflag++;
			continue;

		case 's':
			sflag++;
			continue;

		case 'n':
			nflag++;
			continue;

		case 'e':
			--argc;
			++argv;
			goto out;

		default:
			fprintf(stderr, "Unknown flag\n");
			continue;
		}
	}
out:
	if (argc<=0)
		exit(2);
	compile(*argv);
	nfile = --argc;
	if (argc<=0) {
		if (lflag)
			exit(1);
		execute(0);
	}
	else while (--argc >= 0) {
		argv++;
		execute(*argv);
	}
	exit(nsucc == 0);
}

compile(astr)
char *astr;
{
	register c;
	register char *ep, *sp;
	char *lastep;
	int cclcnt;

	ep = expbuf;
	sp = astr;
	if (*sp == '^') {
		circf++;
		sp++;
	}
	if (wflag)
		*ep++ = CBRC;
	for (;;) {
		if (ep >= &expbuf[ESIZE])
			goto cerror;
		if ((c = *sp++) != '*')
			lastep = ep;
		switch (c) {

		case '\0':
			if (wflag)
				*ep++ = CLET;
			*ep++ = CEOF;
			return;

		case '.':
			*ep++ = CDOT;
			continue;

		case '*':
			if (lastep==0)
				goto defchar;
			*lastep |= STAR;
			continue;

		case '$':
			if (*sp != '\0')
				goto defchar;
			*ep++ = CDOL;
			continue;

		case '[':
			*ep++ = CCL;
			*ep++ = 0;
			cclcnt = 1;
			if ((c = *sp++) == '^') {
				c = *sp++;
				ep[-2] = NCCL;
			}
			do {
				*ep++ = c;
				cclcnt++;
				if (c=='\0' || ep >= &expbuf[ESIZE])
					goto cerror;
			} while ((c = *sp++) != ']');
			lastep[1] = cclcnt;
			continue;

		case '\\':
			if ((c = *sp++) == '\0')
				goto cerror;
			if (c == '<') {
				*ep++ = CBRC;
				continue;
			}
			if (c == '>') {
				*ep++ = CLET;
				continue;
			}
		defchar:
		default:
			*ep++ = CCHR;
			*ep++ = c;
		}
	}
    cerror:
	fprintf(stderr, "RE error\n");
}

same(a, b)
	register int a, b;
{

	return (a == b || iflag && (a ^ b) == ' ' && letter(a) == letter(b));
}

letter(c)
	register int c;
{

	if (c >= 'a' && c <= 'z')
		return (c);
	if (c >= 'A' && c <= 'Z')
		return (c + 'a' - 'A');
	return (0);
}

execute(file)
{
	register char *p1, *p2;
	register c;
	int f;
	char *ebp, *cbp;
	static char *buf;
	static int blksize;
	struct stat stb;

	if (file) {
		if ((f = open(file, 0)) < 0) {
			perror(file);
		}
	} else
		f = 0;
	if (buf == NULL) {
		if (fstat(f, &stb) > 0 && stb.st_blksize > 0)
			blksize = stb.st_blksize;
		else
			blksize = BLKSIZE;
		buf = (char *)malloc(blksize);
		if (buf == NULL) {
			fprintf(stderr, "egrep: no memory for %s\n", file);
			return;
		}
	}
	ebp = buf;
	cbp = buf;
	lnum = 0;
	tln = 0;
	blkno = -1;
	for (;;) {
		lnum++;
		if((lnum&0377) == 0)
			fflush(stdout);
		p1 = linebuf;
		p2 = cbp;
		for (;;) {
			if (p2 >= ebp) {
				if ((c = read(f, buf, blksize)) <= 0) {
					close(f);
					if (cflag) {
						if (lflag) {
							if (tln)
							printf("%s\n", file);
						} else {
							if (nfile > 1)
								printf("%s:", file);
							printf("%ld\n", tln);
						}
					}
					return;
				}
				blkno++;
				p2 = buf;
				ebp = buf+c;
			}
			if ((c = *p2++) == '\n')
				break;
			if(c)
			if (p1 < &linebuf[BUFSIZ-1])
				*p1++ = c;
		}
		*p1++ = 0;
		cbp = p2;
		p1 = linebuf;
		p2 = expbuf;
		if (circf) {
			if (advance(p1, p2))
				goto found;
			goto nfound;
		}
		/* fast check for first character */
		if (*p2==CCHR) {
			c = p2[1];
			do {
				if (*p1!=c && (!iflag || (c ^ *p1) != ' '
					|| letter(c) != letter(*p1)))
					continue;
				if (advance(p1, p2))
					goto found;
			} while (*p1++);
			goto nfound;
		}
		/* regular algorithm */
		do {
			if (advance(p1, p2))
				goto found;
		} while (*p1++);
	nfound:
		if (vflag)
			succeed(file);
		continue;
	found:
		if (vflag==0)
			succeed(file);
	}
}

advance(alp, aep)
	char *alp, *aep;
{
	register char *lp, *ep, *curlp;
	char *nextep;

	lp = alp;
	ep = aep;
	for (;;) switch (*ep++) {

	case CCHR:
		if (!same(*ep, *lp))
			return (0);
		ep++, lp++;
		continue;

	case CDOT:
		if (*lp++)
			continue;
		return(0);

	case CDOL:
		if (*lp==0)
			continue;
		return(0);

	case CEOF:
		return(1);

	case CCL:
		if (cclass(ep, *lp++, 1)) {
			ep += *ep;
			continue;
		}
		return(0);

	case NCCL:
		if (cclass(ep, *lp++, 0)) {
			ep += *ep;
			continue;
		}
		return(0);

	case CDOT|STAR:
		curlp = lp;
		while (*lp++);
		goto star;

	case CCHR|STAR:
		curlp = lp;
		while (same(*lp, *ep))
			lp++;
		lp++;
		ep++;
		goto star;

	case CCL|STAR:
	case NCCL|STAR:
		curlp = lp;
		while (cclass(ep, *lp++, ep[-1]==(CCL|STAR)));
		ep += *ep;
		goto star;

	star:
		do {
			lp--;
			if (advance(lp, ep))
				return(1);
		} while (lp > curlp);
		return(0);

	case CBRC:
		if (lp == expbuf)
			continue;
#define	uletter(c)	(letter(c) || c == '_')
		if ( ( uletter(*lp) || digit ( * lp ) )  && !uletter(lp[-1]) && !digit(lp[-1]))
			continue;
		return (0);

	case CLET:
		if (!uletter(*lp) && !digit(*lp))
			continue;
		return (0);

	default:
		fprintf(stderr, "RE botch\n");
	}
}

cclass(aset, ac, af)
	char *aset;
{
	register char *set, c;
	register n;

	set = aset;
	if ((c = ac) == 0)
		return(0);
	n = *set++;
	while (--n)
		if (n > 2 && set[1] == '-') {
			if (c >= (set[0] & 0177) && c <= (set[2] & 0177))
				return (af);
			set += 3;
			n -= 2;
		} else
			if ((*set++ & 0177) == c)
				return(af);
	return(!af);
}

succeed(f)
{
	nsucc = 1;
	if (sflag)
		return;
	if (cflag) {
		tln++;
		return;
	}
	if (nfile > 1)
		printf("%s:", f);
	if (bflag)
		printf("%d:", blkno);
	if (nflag)
		printf("%ld:", lnum);
	printf("%s\n", linebuf);
}

digit(c)
	char c;
{
	return (c>='0' && c<='9');
}
