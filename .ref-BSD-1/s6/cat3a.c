#
/*
 * cat3a [ - ] [ file ... ]
 *
 * Cat3a - cat to an adm3a quickly
 * Bill Joy UCB June 1977
 *
 * Option - causes screen clear first
 */
char line[66];
char *linp line;

struct buf {
	int fildes;
	int nleft;
	char *nextp;
	char buff[512];
} ibuf;

#define	EOF	-1

main(argc, argv)
	int argc;
	char *argv[];
{
	register c;

	argv++;
	argc--;
	if (argc > 0 && argv[0][0] == '-' && argv[0][1] == 0) {
		putchar('\032');
		argc--;
		argv++;
	}
	pstart();
	do {
		if (argc > 0) {
			close(0);
			if (fopen(argv[0], &ibuf) < 0) {
				printf("%s: Cannot open\n", argv[0]);
				putchar(EOF);
				pstop();
				exit(1);
			}
			argc--;
			argv++;
		}
		while ((c = getc(&ibuf)) != -1)
			putchar(c);
	} while (argc > 0);
	putchar(EOF);
	pstop();
}

putchar(c)
	register c;
{

	if (c == EOF) {
		flush(1);
		return;
	}
	*linp++ = c;
	if (c == '\n' || linp >= &line[64])
		flush(0);
}

int outcol -20;
int outline -20;
int destcol;
int destline 23;
int obuf[259] 1;

flush(f)
	int f;
{
	register char *lp;
	register c;

	*linp = 0;
	linp = line;
	lp = line;
	while (*lp)
		switch (c = *lp++) {
			case '\r':
				destline =+ destcol / 80;
				destcol = 0;
				continue;
			case '\b':
				destline =+ destcol / 80;
				destcol =% 80;
				if (destcol)
					destcol--;
				continue;
			case '\032':
				putc(c, obuf);
				outcol = 0;
				outline = 0;
				destcol = 0;
				destline = 0;
				continue;
			case ' ':
				destcol++;
				continue;
			case '\t':
				destcol =+ 8;
				destcol =& ~7;
				continue;
			case '\n':
				destline =+ destcol / 80;
				destline++;
				destcol = 0;
				continue;
			default:
				fgoto();
				do {
					putc(c, obuf);
					outcol++;
					destcol++;
					c = *lp++;
				} while (c > ' ');
				--lp;
				continue;
		}
	if (f)
		fgoto();
	fflush(obuf);
}

fgoto()
{
	register int i, j;

	if (outcol > 79) {
		outline =+ outcol / 80;
		outcol =% 80;
	}
	if (outline > 23) {
		destline =- outline - 23;
		outline = 23;
	}
	if (destcol > 79) {
		destline =+ destcol / 80;
		destcol =% 80;
	}
	if (destline > 23) {
		i = destline;
		destline = 23;
		fgoto();
		while (i > destline) {
			putc('\n', obuf);
			i--;
		}
		return;
	}
	j = destline - outline;
	i = destcol - outcol;
	if (i < 0)
		i = -i;
	i =+ j;
	if (outcol)
		j++;
	j =+ destcol;
	if (i < 4 || j < 4) {
		if (j < i) {
			putc('\r', obuf);
			outcol = 0;
		}
		while (outcol < destcol) {
			outcol++;
			putc('\014', obuf);
		}
		while (outcol > destcol) {
			outcol--;
			putc('\b', obuf);
		}
		while (outline < destline) {
			outline++;
			putc('\n', obuf);
		}
		return;
	}
	putc('\033', obuf);
	putc('=', obuf);
	putc(' ' + destline, obuf);
	putc(' ' + destcol, obuf);
	outline = destline;
	outcol = destcol;
}

int ptty[3], pintr(), psvtty, psvint;

#define	ECHO	010
#define	CRLF	020

pstart()
{

	gtty(1, ptty);
	psvtty = ptty[2];
	psvint = signal(2, pintr);
	ptty[2] =& ~(ECHO|CRLF);
	stty(1, ptty);
}

pintr()
{

	pstop();
	exit(1);
}

pstop()
{

	ptty[2] = psvtty;
	stty(1, ptty);
	signal(2, psvint);
}

getc(ip)
	register struct buf *ip;
{

	if (ip->nleft > 0) {
		ip->nleft--;
		return (*ip->nextp++);
	}
	flush(1);
	ip->nleft = read(ip->fildes, &ip->buff, 512);
	if (ip->nleft <= 0)
		return (-1);
	ip->nextp = ip->buff;
	return (getc(ip));
}

fopen(cp, ip)
	register char *cp;
	register struct buf *ip;
{

	ip->fildes = open(cp, 0);
	if (ip->fildes < 0)
		return (-1);
	ip->nleft = 0;
}
