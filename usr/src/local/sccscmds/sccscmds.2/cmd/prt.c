# include "../hdr/defines.h"
# include "../hdr/had.h"

static char Sccsid[] = "@(#)prt.c	4.3	%G%";

/*
	Program to print parts or all of an SCCS file.
	Arguments to the program may appear in any order
	and consist of keyletters, which begin with '-',
	and named files.

	If a direcory is given as an argument, each
	SCCS file within the directory is processed as if
	it had been specifically named. If a name of '-'
	is given, the standard input is read for a list
	of names of SCCS files to be processed.
	Non-SCCS files are ignored.
*/

# define NOEOF	0
# define BLANK(p)	while (!(*p == ' ' || *p == '\t')) p++;

char had[26];
FILE *iptr;
char line[512];
char statistics[25];
struct delent {
	char type;
	char *osid;
	char *datetime;
	char *pgmr;
	char *serial;
	char *pred;
} del;
int num_files;
int prefix;
long cutoff;
long revcut;
int linenum;
char *ysid;
char *flagdesc[26] = {	"",
			"branch",
			"ceiling",
			"default SID",
			"",
			"floor",
			"",
			"",
			"id keywd err/warn",
			"",
			"",
			"",
			"module",
			"null delta",
			"",
			"",
			"",
			"",
			"",
			"type",
			"",
			"validate MRs",
			"",
			"",
			"",
			""
};

main(argc,argv)
int argc;
char *argv[];
{
	register int j;
	register char *p;
	char c;
	int testklt;
	extern prt();
	extern int Fcnt;

	/*
	Set flags for 'fatal' to issue message, call clean-up
	routine, and terminate processing.
	*/
	Fflags = FTLMSG | FTLCLN | FTLEXIT;

	testklt = 1;

	/*
	The following loop processes keyletters and arguments.
	Note that these are processed only once for each
	invocation of 'main'.
	*/
	for (j = 1; j < argc; j++)
		if (argv[j][0] == '-' && (c = argv[j][1])) {
			p = &argv[j][2];
			switch (c) {
			case 'e':	/* print everything but body */
			case 's':	/* print only delta desc. and stats */
			case 'd':	/* print whole delta table */
			case 'a':	/* print all deltas */
			case 'i':	/* print inc, exc, and ignore info */
			case 'u':	/* print users allowed to do deltas */
			case 'f':	/* print flags */
			case 't':	/* print descriptive user-text */
			case 'b':	/* print body */
				break;

			case 'y':	/* delta cutoff */
				ysid = p;
				prefix++;
				break;

			case 'c':	/* time cutoff */
				if (*p && date_ab(p,&cutoff))
					fatal("bad date/time (cm5)");
				prefix++;
				break;

			case 'r':	/* reverse time cutoff */
				if (*p && date_ab(p,&revcut))
					fatal ("bad date/time (cm5)");
				prefix++;
				break;

			default:
				fatal("unknown key letter (cm1)");
			}

			if (had[c - 'a']++ && testklt++)
				fatal("key letter twice (cm2)");
			argv[j] = 0;
		}
		else
			num_files++;

	if (num_files == 0)
		fatal("missing file arg (cm3)");

	if (HADC && HADR)
		fatal("both 'c' and 'r' keyletters specified (pr2)");

	setsig();

	/*
	Change flags for 'fatal' so that it will return to this
	routine (main) instead of terminating processing.
	*/
	Fflags &= ~FTLEXIT;
	Fflags |= FTLJMP;

	/*
	Call 'prt' routine for each file argument.
	*/
	for (j = 1; j < argc; j++)
		if (p = argv[j])
			do_file(p,prt);

	exit(Fcnt ? 1 : 0);
}


/*
	Routine that actually performs the 'prt' functions.
*/

prt(file)
char *file;
{
	int stopdel;
	int user, flag, text;
	char *p;
	long bindate;

	if (setjmp(Fjmp))	/* set up to return here from 'fatal' */
		return;		/* and return to caller of prt */

	if (HADE)
		HADD = HADI = HADU = HADF = HADT = 1;

	if (!HADU && !HADF && !HADT && !HADB)
		HADD = 1;

	if (!HADD)
		HADR = HADS = HADA = HADI = HADY = HADC = 0;

	if (HADS && HADI)
		fatal("s and i conflict (pr1)");

	iptr = xfopen(file,0);

	p = lineread(NOEOF);
	if (*p++ != CTLCHAR || *p != HEAD)
		fatal("not an sccs file (co2)");

	stopdel = 0;

	if (!prefix)
		printf("\n%s:\n",file);

	if (HADD) {
		while ((p = lineread(NOEOF)) && *p++ == CTLCHAR &&
				*p++ == STATS && !stopdel) {
			NONBLANK(p);
			copy(p,statistics);

			p = lineread(NOEOF);
			getdel(&del,p);

			if (!HADA && del.type != 'D') {
				read_to(EDELTAB);
				continue;
			}
			if (HADC) {
				date_ab(del.datetime,&bindate);
				if (bindate < cutoff) {
					stopdel = 1;
					break;
				}
			}
			if (HADR) {
				date_ab(del.datetime,&bindate);
				if (bindate >= revcut) {
					read_to(EDELTAB);
					continue;
				}
			}
			if (HADY && (equal(del.osid,ysid) || !(*ysid)))
				stopdel = 1;

			printdel(file,&del);

			while ((p = lineread(NOEOF)) && *p++ == CTLCHAR) {
				if (*p == EDELTAB)
					break;
				switch (*p) {
				case INCLUDE:
					if (HADI)
						printit(file,"Included:\t",p);
					break;

				case EXCLUDE:
					if (HADI)
						printit(file,"Excluded:\t",p);
					break;

				case IGNORE:
					if (HADI)
						printit(file,"Ignored:\t",p);
					break;

				case MRNUM:
					if (!HADS)
						printit(file,"MRs:\t",p);
					break;

				case COMMENTS:
					if (!HADS)
						printit(file,"",p);
					break;

				default:
					sprintf(Error, "format error at line %d (co4)",linenum);
					fatal(Error);
				}
			}
		}
		if (prefix)
			printf("\n");

		if (stopdel && !(line[0] == CTLCHAR && line[1] == BUSERNAM))
			read_to(BUSERNAM);
	}
	else
		read_to(BUSERNAM);

	if (HADU) {
		user = 0;
		printf("\nUsers allowed to make deltas --\n");
		while ((p = lineread(NOEOF)) && *p != CTLCHAR) {
			user = 1;
			printf("\t%s",p);
		}
		if (!user)
			printf("\teveryone\n");
	}
	else
		read_to(EUSERNAM);

	if (HADF) {
		flag = 0;
		printf("\nFlags --\n");
		while ((p = lineread(NOEOF)) && *p++ == CTLCHAR &&
				*p++ == FLAG) {
			flag = 1;
			NONBLANK(p);
			printf("\t%s",flagdesc[*p - 'a']);

			if (*++p) {
				NONBLANK(p);
				printf("\t%s",p);
			}
		}
		if (!flag)
			printf("\tnone\n");
	}
	else
		read_to(BUSERTXT);

	if (HADT) {
		text = 0;
		printf("\nDescription --\n");
		while ((p = lineread(NOEOF)) && *p != CTLCHAR) {
			text = 1;
			printf("\t%s",p);
		}
		if (!text)
			printf("\tnone\n");
	}
	else
		read_to(EUSERTXT);

	if (HADB) {
		printf("\n");
		while (p = lineread(EOF))
			if (*p == CTLCHAR)
				printf("*** %s", ++p);
			else
				printf("\t%s", p);
	}

	fclose(iptr);
}


getdel(delp,lp)
register struct delent *delp;
register char *lp;
{
	lp += 2;
	NONBLANK(lp);
	delp->type = *lp++;
	NONBLANK(lp);
	delp->osid = lp;
	BLANK(lp);
	*lp++ = '\0';
	NONBLANK(lp);
	delp->datetime = lp;
	BLANK(lp);
	NONBLANK(lp);
	BLANK(lp);
	*lp++ = '\0';
	NONBLANK(lp);
	delp->pgmr = lp;
	BLANK(lp);
	*lp++ = '\0';
	NONBLANK(lp);
	delp->serial = lp;
	BLANK(lp);
	*lp++ = '\0';
	NONBLANK(lp);
	delp->pred = lp;
	repl(lp,'\n','\0');
}


read_to(ch)
register char ch;
{
	char *n;

	while ((n = lineread(NOEOF)) &&
			!(*n++ == CTLCHAR && *n == ch))
		;

	return(n);
}


lineread(eof)
register int eof;
{
	char *k;

	k = fgets(line,512,iptr);

	if (k == NULL && !eof)
		fatal("premature eof (co5)");

	linenum++;

	return(k);
}


printdel(file,delp)
register char *file;
register struct delent *delp;
{
	printf("\n");

	if (prefix) {
		statistics[length(statistics) - 1] = '\0';
		printf("%s:\t",file);
	}

	printf("%c %s\t%s\t%s\t%s\t%s\t%s",delp->type,delp->osid,
		delp->datetime,delp->pgmr,delp->serial,delp->pred,statistics);
}


printit(file,str,cp)
register char *file;
register char *str, *cp;
{
	cp++;
	NONBLANK(cp);

	if (prefix) {
		cp[length(cp) - 1] = '\0';
		printf(" ");
	}

	printf("%s%s",str,cp);
}
