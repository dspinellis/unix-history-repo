#
/*
 * lpd -- line printer daemon dispatcher
 *
 */

#include	<ctype.h>

#define	SPIDER	0
#define	PHONE	0
#define	LPD	1

char	dpd[]	= "/usr/spool/lpd";
char	dfname[30] = "/usr/spool/lpd/";
char	lock[]	= "/usr/spool/lpd/lock";

#include	"daemon.c"

/*
 * The remaining part is the line printer interface.
 */

char	lp[]	= "/dev/lp";
char	chrtab[][16];
FILE	*lpf = NULL;

dem_con()
{
	if((lpf = fopen(lp, "w")) == NULL)
		return(-1);
	return(0);
}

dem_dis()
{

	FCLOSE(lpf);
	FCLOSE(dfb);
}

dem_open(file)
char	*file;
{
}

dem_close()
{
}

get_snumb()
{
}

lwrite()
{
	banner(&line[1]);
}


FILE	*ibuf;

sascii(fff)
{
	if((ibuf = fopen(&line[1], "r")) == NULL)
		return(0);
	if(fff)
		putc(ff, lpf);
	bsopt();
	fflush(lpf);
	fclose(ibuf);
	return(0);
}

/*
 *	Copy file ibuf to lpf arranging that no
 *	backspaces will appear in the output.  Courtesy ARK
 */

#define BSIZE 256

char b1[BSIZE], b2[BSIZE];
int size1, size2;

bsopt()
{
	register int cp, c;

	size1 = size2 = cp = 0;
	while ((c = getc(ibuf)) != EOF) {
		if (isprint (c)) {
			if (cp < BSIZE) {
				if (cp >= size1 || b1[cp] == ' ') {
					while (cp >= size1)
						b1[size1++] = ' ';
				} else {
					if (cp < size2 && b2[cp] != ' ')
						pr2();
					while (cp >= size2)
						b2[size2++] = ' ';
					b2[cp] = b1[cp];
				}
				b1[cp++] = c;
			}
		} else {
			switch (c) {

			case ' ':
				cp++;
				break;
			
			case '\b':
				if (cp)
					cp--;
				break;

			case '\t':
				cp = (cp + 8) & -8;
				break;

			case '\r':
				cp = 0;
				break;

			case '\n':
			case '\f':
				pr2();
				pr1();
				putc(c, lpf);
				size1 = cp = 0;
				break;

			}
		}
	}
	pr2();
	pr1();
}
pr1()
{
	register char *p, *lim;

	p = b1;
	lim = p + size1;
	while (p < lim)
		putc(*p++, lpf);
	size1 = 0;
}

pr2()
{
	register char *p, *lim;

	p = b2;
	lim = p + size2;
	if (p < lim) {
		do
			putc(*p++, lpf);
		while (p < lim);
		putc('\r', lpf);
	}
	size2 = 0;
}

/* VARARGS */
trouble(s, a1, a2, a3, a4)
char	*s;
{
	if(retcode != 0){
		dem_dis();
	}
	longjmp(env, 1);
}

/* VARARGS */
logerr()
{
}

getowner()
{
}

maildname()
{
	fprintf(pmail, "Your lpr printer job is done.\n");
}

banner(s)
char *s;
{
	long timeb;
	register char *sp;
	int i, j, t, lsw;

	for(lsw=0; s[lsw] && lsw<5; lsw++);
	putc(ff, lpf);
	fprintf(lpf, "\n\n\n\n\n\n\n\n");
	for (i=0; i<16; i++) {
		if(lsw < 5)
			fprintf(lpf, "                ");
		for (sp=s; *sp; sp++) {
			if (*sp<=' '|| *sp >'}')
				continue;
			fprintf(lpf, "  ");
			t = chrtab[*sp - ' '][i];
			for (j=7; j>=0; j--)
				if ((t>>j) & 01)
					putc('X', lpf);
				else
					putc(' ', lpf);
		}
		putc('\n', lpf);
	}
	fprintf(lpf, "\n\n\n\n\n\n\n\n");
	time(&timeb);
	fprintf(lpf, "                ");
	fprintf(lpf, ctime(&timeb));
	putc(ff, lpf);
}
