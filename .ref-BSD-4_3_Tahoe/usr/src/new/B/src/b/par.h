/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: par.h,v 1.1 84/07/04 17:57:16 timo Exp $ */

#include	<stdio.h>
#define	EQ	==
#define	NE	!=
#define	EOS	'\0'
#define	ERR	(-1)

#ifdef	MFILE
#define	MDOTS	"... "
#else
#define	MDOTS	""
#endif

#define	ERM	{fprintf(stderr, "%s: ", pname); fprintf(stderr,
#define	MRE	); exit(ERR); }

#ifdef	lint
int __void__;	/* to tell `lint' not to care */
#define	VOID(x)	(__void__ = (int)(x))
#else	lint
#define	VOID(x)	(x)
#endif	lint

FILE	*ifile;		/* input file */
char	*iname;		/* input name */
char	*pname;		/* program name */

main(pargc, pargv) char *pargv[];	{
	register int argc;
	register char **argv;

	pname = pargv[0];
	argc = pargc-1;
	argv = pargv+1;
	while (argc > 0 && argv[0][0] EQ '-' && argv[0][1] NE EOS)	{
#ifdef	OPTIONS
		register int nop = options(argc, argv);
		if (nop EQ ERR)
			ERM "options are [%s] [ file %s]\n",
				OPTIONS, MDOTS
			MRE
		argc -= nop;
		argv += nop;
#else
		ERM "parameters are [ file %s]\n", MDOTS MRE
#endif
	}
#ifndef	MFILE
	if (argc > 1)
		ERM "arg count\n" MRE
#endif
	do	{
		if (argc EQ 0 || (argv[0][0] EQ '-' && argv[0][1] EQ EOS))	{
			iname = "standard input";
			ifile = stdin;
		}
		else	{
			iname = *argv;
			ifile = fopen(iname, "r");
#ifndef	MFILE
			if (ifile EQ NULL)
				ERM "cannot open %s\n", iname MRE
#endif
		}
		process();
		if (ifile NE NULL && ifile NE stdin)
			VOID(fclose(ifile));
	} while (++argv, --argc > 0);
	exit(0);
}

