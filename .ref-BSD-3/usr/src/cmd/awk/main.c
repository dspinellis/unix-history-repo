#include "stdio.h"
#include "awk.def"
#include "awk.h"

int	dbg	= 0;
int	svargc;
char	**svargv, **xargv;
extern FILE	*yyin;	/* lex input file */
char	*lexprog;	/* points to program argument if it exists */
extern	errorflag;	/* non-zero if any syntax errors; set by yyerror */

int filefd, iflag, symnum, ansfd;
char *filelist;
extern int maxsym, errno;
main(argc, argv) int argc; char *argv[]; {
	if (argc == 1)
		error(FATAL, "Usage: awk [-f source | 'cmds'] [files]");
	if (strcmp(argv[0], "a.out"))
		logit(argc, argv);
	syminit();
	while (argc > 1) {
		argc--;
		argv++;
		if (argv[0][0] == '-' && argv[0][1] == 'f') {
			yyin = fopen(argv[1], "r");
			if (yyin == NULL)
				error(FATAL, "can't open %s", argv[1]);
			argc--;
			argv++;
			break;
		} else if (argv[0][0] == '-' && argv[0][1] == 'F') {	/* set field sep */
			if (argv[0][2] == 't')	/* special case for tab */
				**FS = '\t';
			else
				**FS = argv[0][2];
			continue;
		} else if (argv[0][0] != '-') {
			dprintf("cmds=|%s|\n", argv[0], NULL, NULL);
			yyin = NULL;
			lexprog = argv[0];
			argv[0] = argv[-1];	/* need this space */
			break;
		} else if (strcmp("-d", argv[0])==0)
			dbg = 1;
		else if (argv[0][0]=='-' && argv[0][1]=='i') {
			iflag=1;
			sscanf(argv[0], "-i%d, %d", &filefd, &ansfd);
		}
	}
	if (argc <= 1 && !iflag) {
		argv[0][0] = '-';
		argv[0][1] = '\0';
		argc++;
		argv--;
	}
	if (!iflag) {
		svargc = --argc;
		svargv = ++argv;
		dprintf("svargc=%d svargv[0]=%s\n", svargc, svargv[0], NULL);
	}
	*FILENAME = *svargv;	/* initial file name */
iloop:
	if (iflag)
		msgfiles();
	yyparse();
	dprintf("errorflag=%d\n", errorflag, NULL, NULL);
	if (errorflag)
		exit(0);
	run();
	if (iflag)
		write(ansfd, &errorflag, sizeof(errorflag));
	else exit(errorflag);
/*sym cleanup should go here , followed by another syminit*/
	goto iloop;
}

logit(n, s) char *s[];
{	int i, tvec[2];
	FILE *f, *g;
	char buf[BUFSIZ];
	if ((f=fopen("/usr/pjw/awk/awkhist", "a"))==NULL)
		return;
	time(tvec);
	fprintf(f, "%-8s %s", getlogin(), ctime(tvec));
	for (i=0; i<n; i++)
		fprintf(f, "'%s'", s[i]);
	putc('\n', f);
	if (strcmp(s[1], "-f")) {
		fclose(f);
		return;
	}
	if ((g=fopen(s[2], "r"))==NULL) {
		fclose(f);
		return;
	}
	while ((i=fread(buf, 1, BUFSIZ, g))>0)
		fwrite(buf, 1, i, f);
	fclose(f);
	fclose(g);
}

yywrap()
{
	return(1);
}

msgfiles()
{	char buf[BUFSIZ], *p, *q, **s;
	int n;
	n=read(filefd, buf, BUFSIZ);
	if (n<=0)	/*no one at other end?*/ {
		perror("no files");
		exit(errno);
	}
	xfree(filelist);
	q=filelist=malloc(n);
	for (p=buf; *p==' ' || *p=='\t' || *p=='\n'; p++); 
	for (n=0; *p!=';'; )
	{
		if (*p==' ' || *p=='\t' || *p=='\n') {
			n++;
			*q++=0;
			while (*p==' ' || *p=='\t' || *p=='\n')
				p++;
		}
		else	*q++ = *p++;
	}
	if (q!=filelist && *(q-1)!=0) {
		n++;
		*q++ = 0;
	}
	svargc=n;
	xfree(xargv);
	xargv=s=svargv=malloc(n*sizeof(char *));
	for (p=filelist; n>0; n--)
	{
		*s++=p;
		while (*p++ != 0);
	}
}
