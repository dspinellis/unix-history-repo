# include	"../hdr/defines.h"

static char Sccsid[] = "@(#)dohist.c	1.4	%G%";

char	Cstr[RESPSIZE];
char	Mstr[RESPSIZE];

dohist(file)
char *file;
{
	char line[BUFSIZ];
	int tty[3];
	int doprmt;
	register char *p;
	FILE *in;
	extern char *Mrs;
	extern char *Comments;
	extern int Domrs;

	in = xfopen(file,0);
	while ((p = fgets(line,sizeof(line),in)) != NULL)
		if (line[0] == CTLCHAR && line[1] == EUSERNAM)
			break;
	if (p != NULL) {
		while ((p = fgets(line,sizeof(line),in)) != NULL)
			if (line[3] == VALFLAG && line[1] == FLAG && line[0] == CTLCHAR)
				break;
			else if (line[1] == BUSERTXT && line[0] == CTLCHAR)
				break;
		if (p != NULL && line[1] == FLAG) {
			Domrs++;
		}
	}
	fclose(in);
	doprmt = 0;
	if (gtty(0,tty) >= 0)
		doprmt++;
	if (Domrs && !Mrs) {
		if (doprmt)
			printf("MRs? ");
		Mrs = getresp(" ",Mstr);
	}
	if (Domrs)
		mrfixup();
	if (!Comments) {
		if (doprmt)
			printf("comments? ");
		sprintf(line,"\n%c%c ",CTLCHAR,COMMENTS);
		Comments = getresp(line,Cstr);
	}
}


getresp(repstr,result)
char *repstr;
char *result;
{
	char line[BUFSIZ], *index();
	register int done, sz;
	register char *p;

	result[0] = 0;
	done = 0;
	setbuf(stdin,NULL);
	sz = sizeof(line) - size(repstr);
	while (!done && fgets(line,sz,stdin) != NULL) {
		p = index(line, '\0');
		if (*--p == '\n') {
			if (*--p == '\\') {
				copy(repstr,p);
			}
			else {
				*++p = 0;
				++done;
			}
		}
		else
			fatal("line too long (co18)");
		if ((size(line) + size(result)) > RESPSIZE)
			fatal("response too long (co19)");
		strcat(result,line);
	}
	return(result);
}


char	*Varg[NVARGS];

valmrs(pkt,pgm)
struct packet *pkt;
char *pgm;
{
	extern char *Sflags[];
	register int i;
	int st;
	register char *p;

	Varg[0] = pgm;
	Varg[1] = auxf(pkt->p_file,'g');
	if (p = Sflags[TYPEFLAG - 'a'])
		Varg[2] = p;
	else
		Varg[2] = Null;
	if ((i = fork()) < 0) {
		fatal("cannot fork; try again (co20)");
	}
	else if (i == 0) {
		for (i = 4; i < 15; i++)
			close(i);
		pexec(pgm,Varg);
		exit(1);
	}
	else {
		wait(&st);
		return(st);
	}
}


mrfixup()
{
	register char **argv, *p, c;
	char *ap;

	argv = &Varg[VSTART];
	p = Mrs;
	NONBLANK(p);
	for (ap = p; *p; p++) {
		if (*p == ' ' || *p == '\t') {
			if (argv >= &Varg[(NVARGS - 1)])
				fatal("too many MRs (co21)");
			*argv = stalloc(size(ap));
			c = *p;
			*p = 0;
			copy(ap,*argv);
			*p = c;
			argv++;
			NONBLANK(p);
			ap = p;
		}
	}
	--p;
	if (*p != ' ' && *p != '\t')
		copy(ap,*argv++ = stalloc(size(ap)));
	*argv = 0;
}


# define STBUFSZ	500

stalloc(n)
register int n;
{
	static char stbuf[STBUFSZ];
	static int stind;
	register char *p;

	p = &stbuf[stind];
	if (&p[n] >= &stbuf[STBUFSZ])
		fatal("out of space (co22)");
	stind += n;
	return(p);
}
