#ifndef lint
static char sccsid[] = "@(#)main.c	4.5 (Berkeley) %G%";
#endif

#include "stdio.h"
#include "ctype.h"
#include "awk.def"
#include "awk.h"
#define TOLOWER(c)	(isupper(c) ? tolower(c) : c) /* ugh!!! */

int	dbg	= 0;
int	ldbg	= 0;
int	svflg	= 0;
int	rstflg	= 0;
int	svargc;
char	**svargv, **xargv;
extern FILE	*yyin;	/* lex input file */
char	*lexprog;	/* points to program argument if it exists */
extern	errorflag;	/* non-zero if any syntax errors; set by yyerror */

int filefd, symnum, ansfd;
char *filelist;
extern int maxsym, errno;
main(argc, argv) int argc; char *argv[]; {
	if (argc == 1)
		error(FATAL, "Usage: awk [-f source | 'cmds'] [files]");
	syminit();
	while (argc > 1) {
		argc--;
		argv++;
		/* this nonsense is because gcos argument handling */
		/* folds -F into -f.  accordingly, one checks the next
		/* character after f to see if it's -f file or -Fx.
		*/
		if (argv[0][0] == '-' && TOLOWER(argv[0][1]) == 'f' && argv[0][2] == '\0') {
			if (argv[1][0] == '-' && argv[1][1] == '\0')
				yyin = stdin;
			else {
				yyin = fopen(argv[1], "r");
				if (yyin == NULL)
					error(FATAL, "can't open %s", argv[1]);
			}
			argc--;
			argv++;
			break;
		} else if (argv[0][0] == '-' && TOLOWER(argv[0][1]) == 'f') {	/* set field sep */
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
		} else if (strcmp("-d", argv[0])==0) {
			dbg = 1;
		}
		else if (strcmp("-l", argv[0])==0) {
			ldbg = 1;
		}
		else if(strcmp("-S", argv[0]) == 0) {
			svflg = 1;
		}
		else if(strncmp("-R", argv[0], 2) == 0) {
			if(thaw(argv[0] + 2) == 0)
				rstflg = 1;
			else {
				fprintf(stderr, "not restored\n");
				exit(1);
			}
		}
	}
	if (argc <= 1) {
		argv[0][0] = '-';
		argv[0][1] = '\0';
		argc++;
		argv--;
	}
	svargc = --argc;
	svargv = ++argv;
	dprintf("svargc=%d svargv[0]=%s\n", svargc, svargv[0], NULL);
	*FILENAME = *svargv;	/* initial file name */
	if(rstflg == 0)
		yyparse();
	dprintf("errorflag=%d\n", errorflag, NULL, NULL);
	if (errorflag)
		exit(errorflag);
	if(svflg) {
		svflg = 0;
		if(freeze("awk.out") != 0)
			fprintf(stderr, "not saved\n");
		exit(0);
	}
	run();
	exit(errorflag);
}

yywrap()
{
	return(1);
}
