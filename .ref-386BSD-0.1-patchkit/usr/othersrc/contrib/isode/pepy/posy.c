/* posy.c - PEPY optional structure-generator (yacc-based) */

/* OPEN QUESTIONS:

	How to do smarter DEFAULT determination for the other types and NULLs

	Perhaps pull-up primitive IDentifiers

	Abort a CHOICE encoding if the structure is empty


				  HEURISTICS

   1. LANGUAGE SIMPLIFICATIONS:


	Pull-up uni-member SEQUENCEs/SETs/CHOICEs


   2. LANGUAGE ASSUMPTIONS:

	Unique tags to avoid conflicts for internal structures (-h1 option)


   3. STYLE ISSUES:

	SEQUENCE/SET OF Type should have Type be an ID for nicer naming
 */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/pepy/RCS/posy.c,v 7.6 91/02/22 09:35:13 mrose Interim $";
#endif

/*
 * $Header: /f/osi/pepy/RCS/posy.c,v 7.6 91/02/22 09:35:13 mrose Interim $
 *
 *
 * $Log:	posy.c,v $
 * Revision 7.6  91/02/22  09:35:13  mrose
 * Interim 6.8
 * 
 * Revision 7.5  90/10/17  11:51:24  mrose
 * sync
 * 
 * Revision 7.4  90/09/07  17:35:09  mrose
 * touch-up
 * 
 * Revision 7.3  90/02/23  17:50:09  mrose
 * update
 * 
 * Revision 7.2  90/02/19  13:09:35  mrose
 * update
 * 
 * Revision 7.1  90/01/11  18:37:05  mrose
 * real-sync
 * 
 * Revision 7.0  89/11/23  22:11:59  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#include <ctype.h>
#include <stdio.h>
#include <varargs.h>
#include "pepy.h"


#define	SVAL(s)		((s) ? (s) : "")
#define PARVAL(s)	((s) ? (s) : "parm")

/*    DATA */

static int aflag = 0;
int	Cflag = 0;		/* posy */
int	dflag = 0;
int	Pflag = 0;		/* pepy compat ... */
char    *bflag = NULL;		/*  .. */
char    *module_actions = NULL;
int	pepydebug = 0;
int	doexternals = 1;
static int fflag = 0;
static int linepos = 0;
static int mflag = 0;
static int sflag = 0;

#define	hflag	(options[0])
#define	Hflag	(options[1])
#define h2flag	(options[2])
#define	NOPTIONS	3

static	int options[NOPTIONS];

static  char *eval = NULLCP;

char   *mymodule = "";
OID	mymoduleid = NULLOID;
static char modulename[BUFSIZ];

int yysection = YP_DECODER;
char *yyencpref = "encode";
char *yydecpref = "decode";
char *yyprfpref = "print";
char *yyencdflt = "encode";
char *yydecdflt = "decode";
char *yyprfdflt = "print";

static char *classes[] = {
    "UNIVERSAL ",
    "APPLICATION ",
    "",
    "PRIVATE "
};

static char *tags[] = {
    "", "BOOLEAN", "INTEGER", "INTEGER", "BIT STRING", "BIT STRING",
    "OCTET STRING", "NULL", "SEQUENCE", "SEQUENCE OF", "SEQUENCE",  "SET",
    "SET OF", "SET", "CHOICE", "ANY", "OBJECT IDENTIFIER", "", "ENUMERATED",
    "REAL",

    NULL
};

static char autogen[BUFSIZ];

char   *sysin = NULLCP;
static char sysout[BUFSIZ];
static char sysdef[BUFSIZ];
static char sysact[BUFSIZ];

static FILE *fact;
static FILE *fdef;

typedef struct modlist {
    char   *md_module;

    struct modlist *md_next;
}		modlist, *MD;
#define	NULLMD	((MD) 0)

static  MD	mymodules = NULLMD;

typedef struct symlist {
    char   *sy_encpref;
    char   *sy_decpref;
    char   *sy_prfpref;
    char   *sy_module;
    char   *sy_name;

    YP	    sy_type;

    struct symlist *sy_next;
}		symlist, *SY;
#define	NULLSY	((SY) 0)

static	SY	mysymbols = NULLSY;


char   *gensym (), *modsym (), *array ();
MD	lookup_module ();
SY	new_symbol (), add_symbol ();
static double val2real ();
static void prime_default ();
YP	lookup_type ();

/*    MAIN */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    int	    i;
    register char  *cp,
		   *dp;

    dp = pepyversion + strlen ("pepy ");
    fprintf (stderr, "posy %s\n", dp);

    sysout[0] = sysdef[0] = sysact[0] = NULL;
    for (argc--, argv++; argc > 0; argc--, argv++) {
	cp = *argv;

	if (strcmp (cp, "-a") == 0) {
	    aflag++;
	    continue;
	}
	if (strcmp (cp, "-d") == 0) {
	    dflag++;
	    continue;
	}
	if (strcmp (cp, "-f") == 0) {
	    dflag++, fflag++;
	    continue;
	}
	if (strncmp (cp, "-h", 2) == 0) {
	    if (cp[2] == NULL) {
		hflag++;
		continue;
	    }
	    if (sscanf (cp + 2, "%d", &i) != 1 || i >= NOPTIONS)
		goto usage;
	    hflag++, options[i]++;
	    continue;
	}
	if (strcmp (cp, "-m") == 0) {
	    mflag++;
	    continue;
	}
	if (strcmp (cp, "-o") == 0) {
	    if (sysout[0]) {
		fprintf (stderr, "too many output files\n");
		exit (1);
	    }
	    argc--, argv++;
	    if ((cp = *argv) == NULL || (*cp == '-' && cp[1] != NULL))
		goto usage;
	    (void) strcpy (sysout, cp);

	    continue;
	}
	if (strcmp (cp, "-s") == 0) {
	    sflag++;
	    continue;
	}

	if (sysin) {
    usage:  ;
	    fprintf (stderr,
		    "usage: posy [-a] [-d] [-f] [-Hh] [-o newmodule.py] [-s] module.py\n");
	    exit (1);
	}

	if (*cp == '-') {
	    if (*++cp != NULL)
		goto usage;
	    sysin = "";
	}
	sysin = cp;
    }

    switch (pepydebug = (cp = getenv ("POSYTEST")) && *cp ? atoi (cp) : 0) {
	case 2: 
	    yydebug++;		/* fall */
	case 1: 
	    sflag++;		/*   .. */
	case 0: 
	    break;
    }

    if (sysin == NULLCP)
	sysin = "";

    if (*sysin && freopen (sysin, "r", stdin) == NULL) {
	fprintf (stderr, "unable to read "), perror (sysin);
	exit (1);
    }

    if (strcmp (sysout, "-") == 0)
	sysout[0] = NULL;
    if (*sysout && freopen (sysout, "w", stdout) == NULL) {
	fprintf (stderr, "unable to write "), perror (sysout);
	exit (1);
    }

    if (cp = index (dp, ')')) {
	for (cp++; *cp != ' '; cp++)
	    if (*cp == NULL)
		break;
	if (*cp == NULL)
	    cp = NULL;
    }
    if (cp == NULL)
	cp = dp + strlen (dp);
    (void) sprintf (autogen, "posy %*.*s", cp - dp, cp - dp, dp);
    printf ("-- automatically generated by %s, do not edit!\n\n", autogen);

    initoidtbl ();

    exit (yyparse ());		/* NOTREACHED */
}

/*    ERRORS */

yyerror (s)
register char   *s;
{
    yyerror_aux (s);

    if (*sysout)
	(void) unlink (sysout);
    if (*sysdef)
	(void) unlink (sysdef);
    if (*sysact)
	(void) unlink (sysact);

    exit (1);
}

#ifndef lint
warning (va_alist)
va_dcl
{
    char	buffer[BUFSIZ];
    char	buffer2[BUFSIZ];
    char	*cp;
    va_list	ap;

    va_start (ap);

    _asprintf (buffer, NULLCP, ap);

    va_end (ap);

    (void) sprintf (buffer2, "Warning: %s", buffer);
    yyerror_aux (buffer2);
}

#else

/* VARARGS1 */
warning (fmt)
char	*fmt;
{
    warning (fmt);
}
#endif

static	yyerror_aux (s)
register char   *s;
{
    if (linepos)
	fprintf (stderr, "\n"), linepos = 0;

    if (eval)
	fprintf (stderr, "type %s: ", eval);
    else
	fprintf (stderr, "line %d: ", yylineno);
    fprintf (stderr, "%s\n", s);
    if (!eval)
	fprintf (stderr, "last token read was \"%s\"\n", yytext);
}

/*  */

#ifndef	lint
myyerror (va_alist)
va_dcl
{
    char    buffer[BUFSIZ];
    va_list ap;

    va_start (ap);

    _asprintf (buffer, NULLCP, ap);

    va_end (ap);

    yyerror (buffer);
}
#else
/* VARARGS */

myyerror (fmt)
char   *fmt;
{
    myyerror (fmt);
}
#endif


#ifndef	lint
static	pyyerror (va_alist)
va_dcl
{
    char    buffer[BUFSIZ];
    register YP	yp;
    va_list	ap;

    va_start (ap);

    yp = va_arg (ap, YP);

    _asprintf (buffer, NULLCP, ap);

    va_end (ap);

    yyerror_aux (buffer);
    print_type (yp, 0);


    if (*sysout)
	(void) unlink (sysout);
    if (*sysdef)
	(void) unlink (sysdef);
    if (*sysact)
	(void) unlink (sysact);

    exit (1);
}
#else
/* VARARGS */

static	pyyerror (yp, fmt)
YP	yp;
char   *fmt;
{
    pyyerror (yp, fmt);
}
#endif

/*  */

yywrap () {
    if (linepos)
	fprintf (stderr, "\n"), linepos = 0;

    return 1;
}

/*  */

/* ARGSUSED */

yyprint (s, f, top)
char    *s;
int	f;
int	top;
{
    int     len;
    static int  nameoutput = 0;
    static int  outputlinelen = 79;

    if (sflag || !s)
	return;

    if (!nameoutput) {
	if (linepos)
	    fprintf (stderr, "\n\n");

	fprintf (stderr, "%s", mymodule);
	nameoutput = (linepos = strlen (mymodule)) + 1;

	fprintf (stderr, " types:");
	linepos += 7;

	if (top)
	    return;
    }

    len = strlen (s);
    if (linepos != nameoutput)
	if (len + linepos + 1 > outputlinelen)
	    fprintf (stderr, "\n%*s", linepos = nameoutput, "");
	else
	    fprintf (stderr, " "), linepos++;
    fprintf (stderr, "%s", s);
    linepos += len;
}

/*    PASS1 */

pass1 ()
{
    printf ("%s ", mymodule);
    if (mymoduleid) {
	printf ("%s ", oidprint(mymoduleid));
    }
    printf ("DEFINITIONS ::=\n\n");
}

/*  */

pass1_type (encpref, decpref, prfpref, mod, id, yp)
register char  *encpref,
	       *decpref,
	       *prfpref,
	       *mod,
	       *id;
register YP	yp;
{
    register SY	    sy;

    if (lookup_type (mod, id))	/* no duplicate entries, please... */
	return;

    if (pepydebug) {
	if (linepos)
	    fprintf (stderr, "\n"), linepos = 0;

	fprintf (stderr, "%s.%s\n", mod ? mod : mymodule, id);
	print_type (yp, 0);
	fprintf (stderr, "--------\n");
    }
    else
	if (!(yp -> yp_flags & YP_IMPORTED))
	    yyprint (id, 0, 0);

    sy = new_symbol (encpref, decpref, prfpref, mod, id, yp);
    mysymbols = add_symbol (mysymbols, sy);
}

/*    PASS2 */

pass2 () {
    int	    first;
    register SY	    sy;
    register YP	    yp,
		    y;

    if (!sflag)
	(void) fflush (stderr);

    modsym_aux (mymodule, modulename);

    (void) sprintf (sysdef, "%s-types.h", mymodule);
    if ((fdef = fopen (sysdef, "w")) == NULL)
	myyerror ("unable to write %s", sysdef);
    fprintf (fdef, "/* automatically generated by %s, do not edit! */\n\n",
	    autogen);
    fprintf (fdef, "#ifndef\t_module_%s_defined_\n", modulename);
    fprintf (fdef, "#define\t_module_%s_defined_\n\n", modulename);

    fprintf (fdef, "#ifndef	PEPYPATH\n");
    fprintf (fdef, "#include <isode/psap.h>\n");
    if (strcmp (mymodule, "UNIV"))
	fprintf (fdef, "#include <isode/pepy/UNIV-types.h>\n");
    fprintf (fdef, "#else\n");
    fprintf (fdef, "#include \"psap.h\"\n");
    if (strcmp (mymodule, "UNIV"))
	fprintf (fdef, "#include \"../pepy/UNIV-types.h\"\n");
    fprintf (fdef, "#endif\n");

    fprintf (fdef, "\n\n");

    if (fflag) {
	(void) sprintf (sysact, "%s-types.tmp", mymodule);
	if ((fact = fopen (sysact, "w+")) == NULL)
	    myyerror ("unable to write %s", sysact);
    }

    for (sy = mysymbols; sy; sy = sy -> sy_next) {
	eval = sy -> sy_name;
	yp = sy -> sy_type;
	if (sy -> sy_module == NULLCP)
	    yyerror ("no module name associated with symbol");
	if (yp -> yp_flags & YP_IMPORTED)
	    continue;

	do_struct0 (yp, eval);
	if (ferror (stdout) || ferror (fdef) || (fflag && ferror (fact)))
	    myyerror ("write error - %s", sys_errname (errno));
    }

    for (sy = mysymbols; sy; sy = sy -> sy_next) {
	eval = sy -> sy_name;
	yp = sy -> sy_type;
	if (yp -> yp_flags & YP_IMPORTED)
	    continue;

	do_struct1 (yp, eval, NULLCP);
	if (ferror (stdout) || ferror (fdef) || (fflag && ferror (fact)))
	    myyerror ("write error - %s", sys_errname (errno));
    }

    for (sy = mysymbols; sy; sy = sy -> sy_next) {
	eval = sy -> sy_name;
	yp = sy -> sy_type;
	if (yp -> yp_flags & YP_IMPORTED)
	    continue;

	do_struct2 (yp, eval, NULLCP);
	if (ferror (stdout) || ferror (fdef) || (fflag && ferror (fact)))
	    myyerror ("write error - %s", sys_errname (errno));
    }

    if (Cflag == 0)
	printf ("%%{\n#include <stdio.h>\n#include \"%s\"\n%%}\n", sysdef);

    printf ("\nPREFIXES %s %s %s\n", yyencdflt, yydecdflt, yyprfdflt);

    printf ("\nBEGIN\n");

    print_expimp ();

    first = 1;
    for (sy = mysymbols; sy; sy = sy -> sy_next) {
	eval = sy -> sy_name;
	yp = sy -> sy_type;
	if (sy -> sy_module == NULLCP)
	    yyerror ("no module name associated with symbol");
	if (yp -> yp_flags & YP_IMPORTED)
	    continue;

	if (first) {
	    printf ("\nENCODER %s\n", yyencpref);
	    first = 0;
	}

	printf ("\n%s", sy -> sy_name);
	printf (" [[P struct %s *]]",
		modsym (mymodule, sy -> sy_name, "type"));
	switch (yp -> yp_code) {
	    case YP_SEQTYPE:
	    case YP_SEQLIST:
	    case YP_SETTYPE:
	    case YP_SETLIST:
	    case YP_CHOICE:
		if (yp -> yp_declexp || yp -> yp_type)
		    do_type0 (yp, YP_ENCODER);
		break;

	    case YP_BIT:
	    case YP_BITLIST:
		do_type0 (yp, YP_ENCODER);
		break;

	    default:
	        if (!yp -> yp_declexp)
		    break;
		do_type0 (yp, YP_ENCODER);
		break;
	}
	printf (" ::=\n");
	do_type1 (yp, 1, (yp -> yp_flags & YP_TAG) ? 1 : 2, eval, "parm",
		NULLCP, YP_ENCODER);
	printf ("\n");
	if (ferror (stdout) || ferror (fdef) || (fflag && ferror (fact)))
	    myyerror ("write error - %s", sys_errname (errno));
    }

    first = 1;
    for (sy = mysymbols; sy; sy = sy -> sy_next) {
	eval = sy -> sy_name;
	yp = sy -> sy_type;
	if (sy -> sy_module == NULLCP)
	    yyerror ("no module name associated with symbol");
	if (yp -> yp_flags & YP_IMPORTED)
	    continue;

	if (first) {
	    printf ("\nDECODER %s\n", yydecpref);
	    first = 0;
	}

	printf ("\n%s", sy -> sy_name);
	printf (" [[P struct %s **]]",
		modsym (mymodule, sy -> sy_name, "type"));
	switch (yp -> yp_code) {
	    case YP_SEQTYPE:
	    case YP_SEQLIST:
	    case YP_SETTYPE:
	    case YP_SETLIST:
	    case YP_CHOICE:
		if (yp -> yp_declexp || yp -> yp_type)
		    do_type0 (yp, YP_DECODER);
		break;

	    default:
	        if (!yp -> yp_declexp)
		    break;
		do_type0 (yp, YP_DECODER);
		break;
	}
	printf (" ::=\n");

	y = yp;
again: 	;
	switch (y -> yp_code) {
	    case YP_SEQTYPE:
	    case YP_SETTYPE:
	        if (h2flag)
		    xalloc (y, 0, 1, "parm",
			    modsym (mymodule, sy -> sy_name, "type"), 1);
		break;

	    case YP_BIT: 
	    case YP_BITLIST: 
	    case YP_SEQ: 
	    case YP_SET: 
	    case YP_ANY: 
	    case YP_OCT: 
	    case YP_OID: 
	    case YP_IDEFINED: 
	    case YP_REAL:
		break;

	    case YP_SEQLIST: 
	    case YP_SETLIST: 
	    case YP_CHOICE: 
		if (hflag && y -> yp_type && !y -> yp_type -> yp_next) {
		    y = y -> yp_type;
		    goto again;
		}
		/* else fall */

	    default: 
		xalloc (y, 1, 1, "parm",
			modsym (mymodule, sy -> sy_name, "type"), 1);
		break;
	}
	do_type1 (yp, 1, (yp -> yp_flags & YP_TAG) ? 1 : 2, eval, "(*parm)",
		NULLCP, YP_DECODER);
	printf ("\n");
	if (ferror (stdout) || ferror (fdef) || (fflag && ferror (fact)))
	    myyerror ("write error - %s", sys_errname (errno));
    }

    printf ("\nEND\n");

    if (fflag) {
	register int	c;

	(void) fflush (fact);
	(void) fseek (fact, 0L, 0);

	printf ("\n%%{\n");
	while ((c = getc (fact)) != EOF)
		putchar (c);
	printf ("\n%%}\n");

	(void) fclose (fact);
	if (*sysact)
	    (void) unlink (sysact);
    }

    fprintf (fdef, "#endif\n");
    (void) fflush (fdef);
    (void) fflush (stdout);
    if (ferror (stdout) || ferror (fdef))
	myyerror ("write error - %s", sys_errname (errno));
    (void) fclose (fdef);
}

/*  */

/* ARGSUSED */

static	do_struct0 (yp, id)
register YP	yp;
char   *id;
{
    register YP	    y;

    switch (yp -> yp_code) {
	case YP_SEQLIST:
	case YP_SETLIST:
	    components_pullup (yp);
	    break;

	default:
	    break;
    }

    switch (yp -> yp_code) {
	case YP_SEQTYPE: 
	case YP_SETTYPE: 
	    do_struct0 (yp -> yp_type, id);
	    break;

	case YP_CHOICE: 
	case YP_SETLIST: 
	    choice_pullup (yp, yp -> yp_code == YP_CHOICE ? CH_FULLY
							  : CH_PARTIAL);
	    /* and fall */
	case YP_SEQLIST: 
	    for (y = yp -> yp_type; y; y = y -> yp_next)
		do_struct0 (y, id);
	    break;

	case YP_IDEFINED: 
	    if (yp -> yp_module
		    && strcmp (yp -> yp_module, mymodule)
		    && !lookup_module (yp -> yp_module))
		fprintf (fdef, "#include \"%s-types.h\"\n", yp -> yp_module);
	    break;

	default: 
	    break;
    }
}

/*  */

static	do_struct1 (yp, id, pullup)
register YP	yp;
char   *id,
       *pullup;
{
    register int    i,
                    j;
    char    buf1[BUFSIZ];
    register YP	    y;
    register YV	    yv;

    switch (yp -> yp_code) {
	case YP_BIT: 
	case YP_BITLIST: 
	case YP_SEQ: 
	case YP_SET: 
	case YP_ANY: 
	    fprintf (fdef, "\n");
	    if (aflag)
		printag (yp, 4, pullup);
	    fprintf (fdef, "#define\t%s\tPElement\n",
		    modsym (mymodule, id, "type"));
	    if (yp -> yp_code == YP_BITLIST) {
		i = -1;
		for (yv = yp -> yp_value; yv; yv = yv -> yv_next)
		    if ((j = val2int (yv)) < 0)
			pyyerror (yp, "invalid bit number in BIT STRING");
		    else
			if (j > i)
			    i = j;
		if (i < sizeof (int) * 8) {	/* NBBY */
		    fprintf (fdef, "#define\t%s\t\"\\020",
			    modsym (mymodule, eval, "bits"));
		    for (yv = yp -> yp_value; yv; yv = yv -> yv_next)
			if (yv -> yv_flags & YV_NAMED)
			    fprintf (fdef, "\\0%o%s",
				    val2int (yv) + 1, yv -> yv_named);
			else
			    fprintf (fdef, "\\0%oBIT%d",
				    val2int (yv) + 1, val2int (yv));
		    fprintf (fdef, "\"\n");
		}
		for (yv = yp -> yp_value; yv; yv = yv -> yv_next) {
		    modsym_aux (yv -> yv_named, buf1);
		    fprintf (fdef, "#define\t%s_%s\t%d\n",
			    modsym (mymodule, eval, "bit"),
			    buf1, val2int (yv));
		}
	    }
	    if (fflag)
		fprintf (fdef, "#define\t%s\tpe_free\n",
			modsym (mymodule, id, "free"));
	    break;

	case YP_OCT: 
	    fprintf (fdef, "\n");
	    if (aflag)
		printag (yp, 4, pullup);
	    fprintf (fdef, "#define\t%s\tqbuf\n",
		    modsym (mymodule, id, "type"));
	    if (fflag)
		fprintf (fdef, "#define\t%s\tqb_free\n",
			modsym (mymodule, id, "free"));
	    break;

	case YP_OID: 
	    fprintf (fdef, "\n");
	    if (aflag)
		printag (yp, 4, pullup);
	    fprintf (fdef, "#define\t%s\tOIDentifier\n",
		    modsym (mymodule, id, "type"));
	    if (fflag)
		fprintf (fdef, "#define\t%s\toid_free\n",
			modsym (mymodule, id, "free"));
	    break;

	case YP_IDEFINED: 
	    fprintf (fdef, "\n");
	    if (aflag)
		printag (yp, 4, pullup);
	    fprintf (fdef, "#define\t%s\t",
		    modsym (mymodule, id, "type"));
	    fprintf (fdef, "%s\n",
		    modsym (yp -> yp_module, yp -> yp_identifier, "type"));
	    if (fflag) {
		fprintf (fdef, "#define\t%s\t",
			modsym (mymodule, id, "free"));
		fprintf (fdef, "%s\n",
			modsym (yp -> yp_module, yp -> yp_identifier, "free"));
	    }
	    break;

	case YP_SEQLIST: 
	case YP_SETLIST: 
	case YP_CHOICE: 
	    if (hflag && (y = yp -> yp_type) && !y -> yp_next) {
		do_struct1 (y, id, tags[yp -> yp_code]);
		break;
	    }
	/* else fall */

	default: 
	    break;
    }
}

/*  */

static	do_struct2 (yp, id, pullup)
register YP	yp;
char   *id,
       *pullup;
{
    register YP	    y;
    int	flg = (yp -> yp_code == YP_SEQTYPE || yp -> yp_code == YP_SETTYPE);

    switch (yp -> yp_code) {
	case YP_BIT: 
	case YP_BITLIST: 
	case YP_SEQ: 
	case YP_SET: 
	case YP_ANY: 
	case YP_OCT: 
	case YP_OID: 
	case YP_IDEFINED: 
	    break;

	case YP_SEQLIST: 
	case YP_SETLIST: 
	case YP_CHOICE: 
	    if (hflag && (y = yp -> yp_type) && !y -> yp_next) {
		do_struct2 (y, id, tags[yp -> yp_code]);
		break;
	    }
	    /* else fall */

	default: 
	    fprintf (fdef, "\n");
	    if (aflag)
		printag (yp, 4, pullup);
	    fprintf (fdef, "struct %s {\n", modsym (mymodule, id, "type"));
	    if (fflag) {	
		fprintf (fact, "\n%s (arg)\n", modsym (mymodule, id, "free"));
		fprintf (fact, "struct %s *arg;\n{\n",
			modsym (mymodule, id, "type"));
		fprintf (fact, "    struct %s *parm = arg;\n",
			modsym (mymodule, id, "type"));
		if (h2flag && flg)
		    fprintf (fact, "    int\tn_parm;\n");
		fprintf (fact, "\n    if (parm == NULL)\n\treturn;\n\n");
	    }
	    posy (yp, 1, 1, "parm", id, "parm", flg && h2flag);
	    fprintf (fdef, "};\n");
	    fprintf (fdef, "int\t%s ();\n", modsym (mymodule, id, "free"));
	    if (fflag) {
		if (yp -> yp_code != YP_SEQTYPE &&
		    yp -> yp_code != YP_SETTYPE)
		    fprintf (fact, "\n    free ((char *) arg);");
		fprintf (fact, "\n}\n");
	    }
		
	    break;
    }
}

/*  */

static int type0_brackets;
static int type0_bit;

static	do_type0 (yp, direction)
register YP	yp;
int	direction;
{
    type0_brackets = type0_bit = 0;
    do_type0_aux (yp, direction);
    if (type0_brackets)
	printf ("    %%}\n   ");
}

static do_type0_aux (yp, direction)
register YP yp;
int 	direction;
{
    register YP	    y;

    if (yp -> yp_declexp) {
	if (type0_brackets++ == 0)
	    printf ("\n    %%{\n");
	printf ("\tstruct %s *%s%s;\n", yp -> yp_declexp,
		direction == YP_DECODER ? "*" : "", yp -> yp_declexp);
    }

    switch (yp -> yp_code) {
	case YP_SEQTYPE: 
	case YP_SETTYPE: 
	    if (h2flag) {
		if (type0_brackets++ == 0)
		    printf ("\n    %%{\n");
		printf ("\tint     n_%s = 0;\n", yp -> yp_declexp ?
			yp -> yp_declexp : "parm");
	    }
	    do_type0_aux (yp -> yp_type, direction);
	    break;

	case YP_SEQLIST: 
	case YP_SETLIST: 
	case YP_CHOICE: 
	    for (y = yp -> yp_type; y; y = y -> yp_next)
		do_type0_aux (y, direction);
	    break;

	case YP_BIT:
	case YP_BITLIST:
	    if (direction == YP_ENCODER) {
		if (type0_brackets++ == 0)
		    printf ("\n    %%{\n");
		if (type0_bit++ == 0)
		    printf ("\tchar *bit_parm;\n");
	    }
	    break;

	default: 
	    break;
    }
}

/*  */

/* ARGSUSED */

static	do_type1 (yp, top, level, id, var, action2, direction)
register YP yp;
int	top,
	level;
char   *id,
       *var,
       *action2;
int	direction;
{
    int	    i;
    char   *cp,
	   *ep,
	    buffer[BUFSIZ],
    	    varbuf[BUFSIZ];
    register YP	    y;
    register YV	    yv;
    register YT	    yt;

    printf ("%*s", level * 4, "");

    if (yp -> yp_flags & YP_ID) {
	printf ("%s", yp -> yp_id);
	if (!(yp -> yp_flags & YP_TAG))
	    printf ("\n%*s", ++level * 4, "");
    }

    if (yp -> yp_flags & YP_TAG) {
	yt = yp -> yp_tag;
	printf ("[%s%d]\n", classes[yt -> yt_class], val2int (yt -> yt_value));
	level++;
	printf ("%*s", level * 4, "");
    }
    if (yp -> yp_flags & YP_OPTIONAL && yp -> yp_varexp) {
	if ((ep = index (yp -> yp_varexp, ' ')) == NULL)
	    yyerror ("Bug in varexp!");

	(void) sprintf (varbuf, "%*.*s", ep - yp -> yp_varexp,
			ep - yp -> yp_varexp, yp -> yp_varexp);
    }

    switch (yp -> yp_code) {
	case YP_BOOL: 
	    if ((yp -> yp_flags & (YP_OPTIONAL|YP_DEFAULT)) &&
		direction == YP_DECODER) {
		if (!top && !(yp -> yp_flags & (YP_ID | YP_TAG)))
		    printf ("dummy-for-default\n%*s", ++level * 4, "");
		if (yp -> yp_flags & YP_OPTIONAL)
		    printf ("%%{ %s -> optionals |= %s; %%}\n%*s",
			    varbuf, yp -> yp_optcontrol, level * 4, "");
		else
		    printf ("%%{ %s%s = %d; %%}\n%*s",
			    var, SVAL (yp -> yp_varexp),
			    val2int (yp -> yp_default) ? 1 : 0, level * 4, "");
	    }
	    break;

	case YP_INT: 
	    if ((yp -> yp_flags & (YP_OPTIONAL|YP_DEFAULT)) &&
		direction == YP_DECODER) {
		if (!top && !(yp -> yp_flags & (YP_ID | YP_TAG)))
		    printf ("dummy-for-default\n%*s", ++level * 4, "");
		if (yp -> yp_flags & YP_OPTIONAL)
		    printf ("%%{ %s -> optionals |= %s; %%}\n%*s",
			    varbuf, yp -> yp_optcontrol, level * 4, "");
		else
		    printf ("%%{ %s%s = %d; %%}\n%*s",
			    var, SVAL (yp -> yp_varexp),
			    val2int (yp -> yp_default), level * 4, "");
	    }
	    break;

	case YP_INTLIST:
	case YP_ENUMLIST:
	    if ((yp -> yp_flags & (YP_OPTIONAL|YP_DEFAULT)) &&
		direction == YP_DECODER) {
		if (!top && !(yp -> yp_flags & (YP_ID | YP_TAG)))
		    printf ("dummy-for-default\n%*s", ++level * 4, "");
		if (yp -> yp_flags & YP_OPTIONAL)
		    printf ("%%{ %s -> optionals |= %s; %%}\n%*s",
			    varbuf, yp -> yp_optcontrol, level * 4, "");
		else
		    printf ("%%{ %s%s = %d; %%}\n%*s",
			    var, SVAL (yp -> yp_varexp), dfl2int (yp),
			    level * 4, "");
	    }
	    break;

	case YP_REAL: 
	    if ((yp -> yp_flags & (YP_OPTIONAL|YP_DEFAULT)) &&
		direction == YP_DECODER) {
		if (!top && !(yp -> yp_flags & (YP_ID | YP_TAG)))
		    printf ("dummy-for-default\n%*s", ++level * 4, "");
		if (yp -> yp_flags & YP_OPTIONAL)
		    printf ("%%{ %s -> optionals |= %s; %%}\n%*s",
			    varbuf, yp -> yp_optcontrol, level * 4, "");
		else
		    printf ("%%{ %s%s = %g; %%}\n%*s",
			    var, SVAL (yp -> yp_varexp),
			    val2real (yp -> yp_default), level * 4, "");
	    }
	    break;

	case YP_NULL:
	    if ((yp -> yp_flags & YP_OPTIONAL) && direction == YP_DECODER) {
		if (!top && !(yp -> yp_flags & (YP_ID | YP_TAG)))
		    printf ("dummy-for-default\n%*s", ++level * 4, "");
		printf ("%%{ %s -> optionals |= %s; %%}\n%*s",
			varbuf, yp -> yp_optcontrol, level * 4, "");
	    }
	    break;
    }

    if ((yp -> yp_flags & (YP_TAG | YP_IMPLICIT)) == (YP_TAG | YP_IMPLICIT))
	printf ("IMPLICIT ");
    if (yp -> yp_flags & YP_BOUND)
	printf ("%s < ", yp -> yp_bound);
    if (yp -> yp_flags & YP_COMPONENTS)
	printf ("COMPONENTS OF ");
    if (yp -> yp_flags & YP_ENCRYPTED)
	printf ("ENCRYPTED ");

    switch (yp -> yp_code) {
	case YP_BOOL: 
	    printf ("BOOLEAN");
	    switch (direction) {
		case YP_ENCODER: 
		case YP_DECODER: 
		    printf (top ? "\n%*s[[b %s -> %s ]]" : "\n%*s[[b %s%s ]]",
			    level * 4, "", var, SVAL (yp -> yp_varexp));
		    break;
	    }
	    break;

	case YP_INT: 
	    printf ("INTEGER");
	    switch (direction) {
		case YP_ENCODER: 
		case YP_DECODER: 
		    printf (top ? "\n%*s[[i %s -> %s ]]" : "\n%*s[[i %s%s ]]",
			    level * 4, "", var, SVAL (yp -> yp_varexp));
		    break;
	    }
	    break;

	case YP_INTLIST:
	case YP_ENUMLIST:
	    if (yp -> yp_code == YP_ENUMLIST)
		printf ("ENUMERATED");
	    else
		printf ("INTEGER");
	    switch (direction) {
		case YP_ENCODER: 
		case YP_DECODER: 
		    printf (top ? "\n%*s[[i %s -> %s ]]\n%*s{\n"
			    : "\n%*s[[i %s%s ]]\n%*s{\n",
			    level * 4, "", var, SVAL (yp -> yp_varexp),
			    level * 4, "");
		    break;

		default: 
		    printf (" {\n");
		    break;
	    }
	    level++;
	    for (yv = yp -> yp_value; yv; yv = yv -> yv_next)
		printf ("%*s%s(%d)%s\n", level * 4, "", yv -> yv_named,
			val2int (yv), yv -> yv_next ? "," : "");
	    level--;
	    printf ("%*s}", level * 4, "");
	    break;

	case YP_BIT: 
	    printf ("BIT STRING");
	    switch (direction) {
		case YP_ENCODER: 
		    printf ("\n%*s[[x bit_parm = bitstr2strb (%s%s, &len) $ len]]",
			    level * 4, "", var, SVAL (yp -> yp_varexp));
		    printf ("\n%*s%%{\n%*sfree (bit_parm);\n", level * 4, "",
			    (level + 1) * 4, "");
		    if (action2)
			printf ("%*s%s\n", (level + 1) * 4, "", action2);
		    printf ("%*s%%}\n", level * 4, "");
		    break;

		case YP_DECODER: 
		    balloc (yp, var, action2, level);
		    break;
	    }
	    break;

	case YP_BITLIST: 
	    printf ("BIT STRING");
	    switch (direction) {
		case YP_ENCODER: 
		    printf ("\n%*s[[x bit_parm = bitstr2strb (%s%s, &len) $ len]]\n%*s{\n",
			    level * 4, "", var, SVAL (yp -> yp_varexp),
			    level * 4, "");
		    break;

		case YP_DECODER: 
		default: 
		    printf (" {\n");
		    break;
	    }
	    level++;
	    for (yv = yp -> yp_value; yv; yv = yv -> yv_next)
		printf ("%*s%s(%d)%s\n", level * 4, "", yv -> yv_named,
			val2int (yv), yv -> yv_next ? "," : "");
	    level--;
	    printf ("%*s}", level * 4, "");
	    switch (direction) {
		case YP_DECODER: 
		    balloc (yp, var, action2, level);
		    break;

		case YP_ENCODER:
		    printf ("\n%*s%%{\n%*sfree (bit_parm);\n", level * 4, "",
			    (level + 1) * 4, "");
		    if (action2)
			printf ("%*s%s\n", (level + 1) * 4, "", action2);
		    printf ("%*s%%}\n", level * 4, "");
		    break;
	    }
	    break;

	case YP_OCT: 
	    printf ("OCTET STRING");
	    switch (direction) {
		case YP_ENCODER: 
		    printf ("\n%*s[[q %s%s ]]", level * 4, "",
			    var, SVAL (yp -> yp_varexp));
		    break;

		case YP_DECODER: 
		    printf ("\n%*s[[q %s%s ]]", level * 4, "",
			    var, SVAL (yp -> yp_varexp));
		    break;
	    }
	    break;

	case YP_REAL:
	    printf ("REAL");
	    printf (top ? "\n%*s[[r %s -> %s ]]" : "\n%*s[[r %s%s ]]",
		    level * 4, "", var, SVAL (yp -> yp_varexp));
	    break;

	case YP_NULL: 
	    printf ("NULL");
	    break;

	case YP_SEQ: 
	case YP_SET:
	case YP_ANY:
	    printf ("%s", tags[yp -> yp_code]);
	    switch (direction) {
		case YP_ENCODER:
		case YP_DECODER:
			printf ("\n%*s[[a %s%s ]]",
				level * 4, "", var, SVAL (yp -> yp_varexp));
		    break;
	    }
	    break;

	case YP_SEQTYPE: 
	case YP_SETTYPE: 
	    ep = yp -> yp_code != YP_SETTYPE ? "element" : "member";
	    printf ("%s\n", tags [yp -> yp_code]);
	    switch (direction) {
		case YP_ENCODER: 
		    if ((y = yp -> yp_type) -> yp_declexp) {
			printf ("%*s%%{ %s = %s; %%}\n",
				(level + 1) * 4, "", y -> yp_declexp,
				SVAL (y -> yp_varexp));
		    }
		    if (h2flag) {
			if (top) {
			    printf ("%*s<<n_parm = 0; ", (level + 1) * 4, "");
			    printf ("n_parm < parm -> nelem; n_parm++>>\n");
			}
			else {
			    printf ("%*s<<n_%s = 0;\n%*sn_%s < %s -> nelem;\n",
				    (level + 1) * 4, "", yp -> yp_declexp,
				    (level + 3) * 4, "", yp -> yp_declexp,
				    yp -> yp_declexp);
			    printf ("%*sn_%s++>>\n",
				    (level + 3) * 4, "", yp -> yp_declexp);
			}
		    }
		    else {
			if (top)
			    printf ("%*s<<; parm; parm = parm -> next>>\n",
				    (level + 1) * 4, "");
			else
			    printf ("%*s<<%s = %s%s;\n%*s%s;\n%*s%s = %s -> next>>\n",
				    (level + 1) * 4, "", yp -> yp_declexp,
				    var, SVAL (yp -> yp_varexp),
				    (level + 3) * 4, "", yp -> yp_declexp,
				    (level + 3) * 4, "", yp -> yp_declexp,
				    yp -> yp_declexp);
		    }
		    break;

		case YP_DECODER: 
		    if (h2flag) {
			y = yp -> yp_type;
			xalloc (y, 0, level + 2, y -> yp_declexp,
				y -> yp_declexp, 1);
		    }
		    else
			xalloc (yp, 0, level + 1,
				top ? "parm" : yp -> yp_declexp,
				top ? modsym (mymodule, eval, "type")
				: yp -> yp_declexp, 1);
		    break;
	    }
	    do_type1 (yp -> yp_type, 0, level + 1, ep, "", NULLCP, direction);
	    switch (direction) {
		case YP_DECODER:
		    printf ("\n%*s%%{ ", (level + 1) * 4, "");
		    if (h2flag) {
			
			printf ("n_%s++;", top ? "parm" : yp -> yp_declexp);

			if ((yp -> yp_type) && (yp -> yp_type -> yp_declexp))
			    printf(" %s ++;", yp -> yp_type -> yp_declexp);
			
		    }
		    else
			if (top)
			    printf ("parm = &((*parm) -> next);");
			else
			    printf ("%s = &((*%s) -> next);",
				    yp -> yp_declexp, yp -> yp_declexp);
		    if (action2)
			printf (" %s", action2);
		    printf (" %%}");
		    break;
	    }
	    break;

	case YP_SEQLIST: 
	case YP_SETLIST: 
	    ep = yp -> yp_code != YP_SETLIST ? "element" : "member";
	    printf ("%s", tags [yp -> yp_code]);
	    printf ("\n%*s%%{\n", (level + 1) * 4, "");
	    if (direction == YP_DECODER)
		xalloc (yp, 1, level + 2, yp -> yp_declexp,
			yp -> yp_declexp, 0);
	    for (y = yp -> yp_type; y; y = y -> yp_next) {
		if (y -> yp_declexp)
		    switch (direction) {
		    case YP_ENCODER: 
			printf ("%*s%s = %s;\n",
				(level + 2) * 4, "",
				y -> yp_declexp, y -> yp_varexp);
			break;

		    case YP_DECODER:
			printf ("%*s%s = &(%s);\n",
				(level + 2) * 4, "",
				y -> yp_declexp, y -> yp_varexp);
			break;
		    }
		if (direction == YP_DECODER &&
			 y -> yp_flags & YP_DEFAULT) {
		    prime_default (y, level + 2);
		}
	    }
	    printf ("%*s%%}\n%*s{\n", (level + 1) * 4, "",
		    level * 4, "");
	    
	    if (!hflag || !(y = yp -> yp_type) || y -> yp_next) {
		var = "";
		top = 0;
	    }
	    for (y = yp -> yp_type; y; y = y -> yp_next) {
		do_type1 (y, top,
			level + ((y -> yp_flags & (YP_ID | YP_TAG)) ? 1 : 2),
			ep, var, NULLCP, direction);
		printf ("%s\n", y -> yp_next ? ",\n" : "");
	    }
	    printf ("%*s}", level * 4, "");
	    break;

	case YP_CHOICE: 
	    printf ("CHOICE");
	    if (!hflag || !(y = yp -> yp_type) || y -> yp_next)
		var = "";
	    i = 0;
	    for (y = yp -> yp_type; y; y = y -> yp_next)
		if (y -> yp_declexp)
		    i++;
	    switch (direction) {
		case YP_ENCODER: 
		    if (i) {
			printf ("\n%*s%%{\n", (level + 1) * 4, "");
			for (y = yp -> yp_type; y; y = y -> yp_next)
			    if (y -> yp_declexp)
				printf ("%*s%s = %s;\n", (level + 2) * 4, "",
					y -> yp_declexp, y -> yp_varexp);
			printf ("%*s%%}\n%*s", (level + 1) * 4, "",
				(level + 1) * 4 - 1, "" );
		    }
		    if (*var)
			printf (" <<1>>");
		    else
			if (top)
			    printf (" <<parm -> offset>>");
			else
			    printf (" <<%s -> offset>>",
				    yp -> yp_declexp);
		    printf (i ? "\n%*s{\n" : " {\n", level * 4, "");
		    break;

		case YP_DECODER: 
		    printf ("\n");
		    xalloc (yp, 0, level + 1, yp -> yp_declexp,
			    yp -> yp_declexp, 1);
		    printf ("%*s{\n", level * 4, "");
		    break;

		default: 
		    printf (" {\n");
		    break;
	    }
	    if (direction == YP_DECODER) {
		(void) sprintf (cp = buffer, "(*(%s)) -> offset = ",
			    top ? "parm" : yp -> yp_declexp);
		cp += strlen (cp);
	    }
	    else
		cp = NULL;
	    if (!hflag || !(y = yp -> yp_type) || y -> yp_next)
		top = 0;
	    else
		if (top)
		    cp = NULL;
	    for (y = yp -> yp_type; y; y = y -> yp_next) {
		if (cp)
		    (void) sprintf (cp, "%s;", y -> yp_offset);
		do_type1 (y, top, level + 1, "choice", var,
			cp ? buffer : NULLCP, direction);
		printf ("%s\n", y -> yp_next ? ",\n" : "");
	    }
	    printf ("%*s}", level * 4, "");
	    break;
	
	case YP_OID: 
	    printf ("OBJECT IDENTIFIER");
	    switch (direction) {
		case YP_ENCODER: 
		case YP_DECODER: 
		    printf ("\n%*s[[O %s%s ]]",
			    level * 4, "", var, SVAL (yp -> yp_varexp));
		    break;
	    }
	    break;

	case YP_IDEFINED: 
	    if (yp -> yp_module && strcmp (yp -> yp_module, mymodule))
		printf ("%s.", yp -> yp_module);
	    printf ("%s", yp -> yp_identifier);
	    switch (direction) {
		case YP_ENCODER: 
		    printf ("\n%*s[[p %s%s ]]",
			    level * 4, "", var, SVAL (yp -> yp_varexp));
		    break;

		case YP_DECODER: 
		    printf ("\n%*s[[p &(%s%s)]]",
			    level * 4, "", var, SVAL (yp -> yp_varexp));
		    break;
	    }
	    break;

	default: 
	    myyerror ("unknown type: %d", yp -> yp_code);
    }

    if (action2)
	switch (yp -> yp_code) {
	    case YP_BIT:
	    case YP_BITLIST:
	    	if (direction == YP_ENCODER)
		    break;
	    case YP_SEQTYPE:
	    case YP_SETTYPE:
		if (direction == YP_DECODER)
		    break;
		/* else fall */

	    default:		
		printf ("\n%*s%%{ %s %%}", level * 4, "", action2);
		break;
	}

    if (yp -> yp_flags & YP_OPTIONAL) {
	printf ("\n%*sOPTIONAL", level * 4, "");

	if (direction == YP_ENCODER)
	    switch (yp -> yp_code) {
		case YP_BOOL: 
		case YP_INT: 
		case YP_INTLIST:
	    	case YP_ENUMLIST:
		case YP_NULL:
		case YP_REAL:
		    printf (" <<%s -> optionals & %s >>",
			    varbuf, yp -> yp_optcontrol);
		default: 
		    break;

		case YP_BIT: 
		case YP_BITLIST: 
		case YP_OCT: 
		case YP_SEQ: 
		case YP_SEQTYPE: 
		case YP_SEQLIST: 
		case YP_SET: 
		case YP_SETTYPE: 
		case YP_SETLIST: 
		case YP_CHOICE: 
		case YP_ANY: 
		case YP_OID: 
		case YP_IDEFINED: 
		    printf (" <<%s%s>>", var, SVAL (yp -> yp_varexp));
		    break;
	    }
    }
    else
	if (yp -> yp_flags & YP_DEFAULT) {
	    printf ("\n%*sDEFAULT ", level * 4, "");
	    val2prf (yp -> yp_default, level + 2);

	    if (direction == YP_ENCODER)
		switch (yp -> yp_code) {
		    case YP_BOOL: 
			printf (" <<%s%s%s>>",
				val2int (yp -> yp_default) ? "!" : "",
				var, SVAL (yp -> yp_varexp));
			break;

		    case YP_INT: 
		    case YP_INTLIST:
		    case YP_ENUMLIST:
			printf (" <<%s%s != %d>>", var, SVAL (yp -> yp_varexp),
				dfl2int (yp));
			break;

		    case YP_REAL:
			printf (" << %s%s != %g >>",
				var, SVAL (yp -> yp_varexp),
				val2real (yp -> yp_default));
			break;

		    case YP_NULL: 
		    default: 
			break;

		    case YP_BIT: 
		    case YP_BITLIST: 
		    case YP_OCT: 
		    case YP_SEQ: 
		    case YP_SEQTYPE: 
		    case YP_SEQLIST: 
		    case YP_SET: 
		    case YP_SETTYPE: 
		    case YP_SETLIST: 
		    case YP_CHOICE: 
		    case YP_ANY: 
		    case YP_OID: 
		    case YP_IDEFINED: 
			printf (" <<%s%s>>", var, SVAL (yp -> yp_varexp));
			break;
		}
	}

    if (direction == YP_ENCODER
	    && yp -> yp_varexp
	    && (cp = index (yp -> yp_varexp, ' '))
	    && strncmp (cp + 1, "-> ", 3) == 0) {
	*cp = NULL;
	(void) sprintf (buffer, "(*%s) -> %s", yp -> yp_varexp, cp + 4);
	yp -> yp_varexp = new_string (buffer);
    }
}

/*    TYPE HANDLING */

static YP  lookup_type (mod, id)
register char *mod,
	      *id;
{
    register SY	    sy;

    for (sy = mysymbols; sy; sy = sy -> sy_next) {
	if (mod) {
	    if (strcmp (sy -> sy_module, mod))
		continue;
	}
	else
	    if (strcmp (sy -> sy_module, mymodule)
		    && strcmp (sy -> sy_module, "UNIV"))
		continue;

	if (strcmp (sy -> sy_name, id) == 0)
	    return sy -> sy_type;
    }

    return NULLYP;
}

/*  */

static	posy (yp, top, level, id, val, var, arrayflg)
register YP	yp;
int	top,
	level,
        arrayflg;
char   *id,
       *val,
       *var;
{
    register int    i,
		    j;
    register char  *bp;
    char   *cp,
	   *dp,
           *ep,
    	   *newid,
            buf1[BUFSIZ],
	    buf2[BUFSIZ],
	    buf3[BUFSIZ];
    register YP	    y;
    register YV	    yv;

    (void) strcpy (bp = buf2, var);
    bp += strlen (bp);

    switch (yp -> yp_code) {
	case YP_BOOL: 
	    if (aflag)
		printag (yp, level + 4, NULLCP);
	    fprintf (fdef, "%*schar    %s;\n", level * 4, "",
		     array(id, arrayflg));
	    yp -> yp_varexp = new_string (buf2);
	    break;

	case YP_INT: 
	case YP_INTLIST:
	case YP_ENUMLIST:
	    if (aflag)
		printag (yp, level + 4, NULLCP);
	    fprintf (fdef, "%*sinteger    %s;\n", level * 4, "",
		     array(id, arrayflg));
	    yp -> yp_varexp = new_string (buf2);
	    if (yp -> yp_code == YP_INT)
		break;
	    for (yv = yp -> yp_value; yv; yv = yv -> yv_next) {
		modsym_aux (yv -> yv_named, buf1);
		fprintf (fdef, "#define\t%s_%s\t%d\n",
			modsym (mymodule, top ? eval : id, "int"),
			buf1, val2int (yv));
	    }
	    break;

	case YP_BIT: 
	case YP_BITLIST: 
	    if (!top) {
		if (aflag)
		    printag (yp, level + 4, NULLCP);
		fprintf (fdef, "%*sPE      %s;\n", level * 4, "",
			 array(id, arrayflg));
	    }
	    if (fflag) {
		fprintf (fact, "%*sif (%s)\n", level * 4, "", buf2);
		fprintf (fact,
			"%*spe_free (%s),\n%*s%s = NULLPE;\n",
			(level + 1) * 4, "", buf2,
			(level + 2) * 4, "", buf2);
	    }
	    yp -> yp_varexp = new_string (buf2);
	    if (yp -> yp_code != YP_BITLIST)
		break;
	    i = -1;
	    for (yv = yp -> yp_value; yv; yv = yv -> yv_next)
		if ((j = val2int (yv)) < 0)
		    pyyerror (yp, "invalid bit number in BIT STRING");
		else
		    if (j > i)
			i = j;
	    if (i < sizeof (int) * 8) {		/* NBBY */
		fprintf (fdef, "#define\t%s\t\"\\020",
			modsym (mymodule, top ? eval : id, "bits"));
		for (yv = yp -> yp_value; yv; yv = yv -> yv_next)
		    if (yv -> yv_flags & YV_NAMED)
			fprintf (fdef, "\\0%o%s",
				val2int (yv) + 1, yv -> yv_named);
		    else
			fprintf (fdef, "\\0%oBIT%d",
				val2int (yv) + 1, val2int (yv));
		fprintf (fdef, "\"\n");
	    }
	    for (yv = yp -> yp_value; yv; yv = yv -> yv_next) {
		modsym_aux (yv -> yv_named, buf1);
		fprintf (fdef, "#define\t%s_%s\t%d\n",
			modsym (mymodule, top ? eval : id, "bit"),
			buf1, val2int (yv));
	    }
	    break;

	case YP_REAL: 
	    if (aflag)
		printag (yp, level + 4, NULLCP);
	    fprintf (fdef, "%*sdouble    %s;\n", level * 4, "",
		     array(id, arrayflg));
	    yp -> yp_varexp = new_string (buf2);
	    break;

	case YP_OCT: 
	    if (!top) {
		if (aflag)
		    printag (yp, level + 4, NULLCP);
		fprintf (fdef, "%*sstruct qbuf *%s;\n", level * 4, "",
			 array(id, arrayflg));
	    }
	    if (fflag) {
		fprintf (fact, "%*sif (%s)\n", level * 4, "", buf2);
		fprintf (fact,
			"%*sqb_free (%s),\n%*s%s = NULL;\n",
			(level + 1) * 4, "", buf2,
			(level + 2) * 4, "", buf2);
	    }
	    yp -> yp_varexp = new_string (buf2);
	    break;

	case YP_NULL: 
	    if (aflag)
		printag (yp, level + 4, NULLCP);
	    fprintf (fdef, "%*schar    %s;\n", level * 4, "",
		     array(id, arrayflg));
	    if (yp -> yp_flags & YP_OPTIONAL)
		yp -> yp_varexp = new_string (buf2);
	    break;

	case YP_SEQ: 
	case YP_SET: 
	case YP_ANY: 
	    if (!top) {
		if (aflag)
		    printag (yp, level + 4, NULLCP);
		fprintf (fdef, "%*sPE      %s;\n", level * 4, "",
			 array(id, arrayflg));
	    }
	    if (fflag) {
		fprintf (fact, "%*sif (%s)\n", level * 4, "", buf2);
		fprintf (fact,
			"%*spe_free (%s),\n%*s%s = NULLPE;\n",
			(level + 1) * 4, "", buf2,
			(level + 2) * 4, "", buf2);
	    }
	    yp -> yp_varexp = new_string (buf2);
	    break;

	case YP_SEQTYPE: 
	case YP_SETTYPE: 
	    ep = yp -> yp_code != YP_SETTYPE ? "element" : "member";
	    if ((cp = rindex (buf2, ' ')) && *++cp) {
		if ((dp = rindex (cp, '.')) && *++dp)
		    cp = dp;
		(void) sprintf (dp = buf1, "%*.*s",
			cp - buf2, cp - buf2, buf2);
		dp += strlen (dp);
	    }
	    else {
		(void) strcpy (buf1, buf2);
		dp = NULL;
	    }
	    newid = yp -> yp_ptrname ? yp -> yp_ptrname : id;
	    if (h2flag && top)
		fprintf (fdef, "%*sint\tnelem;\n", level * 4, "");
	    if (!top) {
		if (yp -> yp_structname)
		    id = yp -> yp_structname;
		else if (!Hflag)
		    id = gensym (ep, NULLCP);
		if (aflag)
		    printag (yp, level + 4, NULLCP);
		fprintf (fdef, "%*sstruct %s {\n", level * 4, "", id);
		if (h2flag)
		    fprintf (fdef, "%*sint\tnelem;\n", (level + 1) * 4, "");
	    }
	    if (dp)
		(void) strcpy (dp, newid);
	    (void) strcpy (bp = buf2, id);
	    bp += strlen (bp);

	    if (!top)
		yp -> yp_declexp = new_string (id);

	    if (dp)
		(void) strcpy (dp, newid);
	    yp -> yp_varexp = new_string (buf1);
	    if ((y = yp -> yp_type) -> yp_code == YP_IDEFINED && hflag) {
		modsym_aux (y -> yp_identifier, cp = buf3);
		if (h2flag) {
		    cp += strlen(cp);
		    (void) sprintf (cp, "[n_%s]", PARVAL (yp->yp_declexp));
		    cp = buf3;
		}
	    }
	    else {
		switch (y -> yp_code) {
		case YP_SEQLIST:
		case YP_SETLIST:
		case YP_SETTYPE:
		case YP_SEQTYPE:
		case YP_CHOICE:
		case YP_IDEFINED:
		    cp = gensym (ep, h2flag ? PARVAL(yp->yp_declexp) : NULLCP);
		    break;
		default:
		    cp = gensym (ep, NULLCP);
		    break;
		}
	    }
	    (void) sprintf (bp, " -> %s", cp);
	    if (fflag) {
		if (!top) {
		    fprintf (fact, "%*s{\n", level * 4, "");
		    level++;
		    if (h2flag) {
			fprintf (fact, "%*sint     n_%s;\n",
				 level * 4, "", PARVAL (yp->yp_declexp));
			fprintf (fact, "%*sstruct %s *%s = %s;\n\n",
				 level * 4, "", id, id, var);
		    }
		    else
			fprintf (fact, "%*sstruct %s *%s;\n\n",
				 level * 4, "", id, id);
		}
		if (h2flag) {
		    fprintf (fact, "%*sfor (n_%s = 0;\n",
			     level * 4, "", PARVAL(yp -> yp_declexp));
		    fprintf (fact, "%*sn_%s < %s -> nelem;\n",
			     (level + 2) * 4, "", PARVAL(yp->yp_declexp), id);
		    fprintf (fact, "%*sn_%s++) {\n",
			     (level + 2) * 4, "", PARVAL(yp->yp_declexp));
		}
		else {
		    fprintf (fact,
			     "%*sfor (%s = %s; %s;) {\n",
			     level * 4, "", id, buf1, id);
		    fprintf (fact, "%*sstruct %s *f_%s = %s -> next;\n\n",
			     (level + 1) * 4, "",
			     top ? modsym (mymodule, val, "type") : id,
			     id, id);
		}
	    }
	    level++;
	    posy (y, 0, level, cp, ep, buf2, h2flag);
	    *bp = NULL;
	    if (y -> yp_code != YP_IDEFINED)
		free (cp);
	    if (!h2flag)
		fprintf (fdef, "\n%*sstruct %s *next;\n", level * 4, "",
			 top ? modsym (mymodule, val, "type") : id);

	    level--;

	    (void) strcpy (bp = buf2, var);
	    bp += strlen (bp);
	    if (fflag) {
		if (!h2flag) {
		    fprintf (fact, "\n%*sif (%s)\n%*sfree ((char *) %s);",
			     (level + 1) * 4, "", id,
			     (level + 2) * 4, "", id);
		    fprintf (fact, "\n%*s%s = f_%s;", (level + 1) * 4, "",
			     id, id);
		}
		fprintf (fact, "\n%*s}\n", level * 4, "");

		if (!top) {
		    if (h2flag)
			fprintf (fact, "\n%*s%s = NULL;\n",
				 level * 4, "", var);
		    else
			fprintf (fact, "\n%*s%s = NULL;\n",
				 level * 4, "", yp -> yp_varexp);

		    level--;
		    fprintf (fact, "%*s}\n", level * 4, "");
		}
	    }

	    if (!top) {
		fprintf (fdef, "%*s} *%s;\n", level * 4, "",
			 array(newid, arrayflg));
		if (!Hflag)
		    free (id);
	    }
	    break;

	case YP_SEQLIST: 
	case YP_SETLIST: 
	    ep = yp -> yp_code != YP_SETLIST ? "element" : "member";
	    if ((cp = rindex (buf2, ' ')) && *++cp) {
		if ((dp = rindex (cp, '.')) && *++dp)
		    cp = dp;
		(void) sprintf (dp = buf1, "%*.*s",
			cp - buf2, cp - buf2, buf2);
		dp += strlen (dp);
	    }
	    else {
		(void) strcpy (buf1, buf2);
		dp = NULL;
	    }
	    newid = yp -> yp_ptrname ? yp -> yp_ptrname : id;
	    if (!top) {
		if (yp -> yp_structname)
		    id = yp -> yp_structname;
		else if (!Hflag)
		    id = gensym (ep, NULLCP);
		if (aflag)
		    printag (yp, level + 4, NULLCP);
		fprintf (fdef, "%*sstruct %s {\n", level * 4, "", id);

		if (dp)
		    (void) strcpy (dp, newid);

		if (fflag) {
		    fprintf (fact, "%*sif (%s) {\n",
			    level * 4, "", buf1);
		    i = 0;
		    for (y = yp -> yp_type; y; y = y -> yp_next) {
			switch (y -> yp_code) {
			    case YP_BOOL:
			    case YP_INT:
			    case YP_INTLIST:
			    case YP_ENUMLIST:
			    case YP_REAL:
			    case YP_NULL:
				continue;

			    default:
				i = 1;
				break;
			}
			break;
		    }
		    if (i)
			fprintf (fact, "%*sstruct %s *%s = %s;\n\n",
				(level + 1) * 4, "", id, id, buf1);
		}
		(void) strcpy (bp = buf2, id);
		bp += strlen (bp);
		yp -> yp_declexp = new_string (id);

		level++;
	    }
	    if (dp)
		(void) strcpy (dp, newid);
	    yp -> yp_varexp = new_string (buf1);
	    for (y = yp -> yp_type, i = 0; y; y = y -> yp_next) {
		if (y -> yp_flags & YP_OPTIONAL)
		    switch (y -> yp_code) {
		        case YP_BOOL:
		        case YP_INT:
		        case YP_INTLIST:
			case YP_ENUMLIST:
			case YP_REAL:
			case YP_NULL:
		    	    {
				char obuf[BUFSIZ];
				
				if (i == 0)
				    fprintf (fdef, "%*sint     optionals;\n",
					     level * 4, "");
				if (y -> yp_flags & YP_ID)
				    modsym_aux (y -> yp_id, cp = buf1);
				else {
				    cp = gensym (ep, NULLCP);
				    (void) strcpy (buf1, cp);
				    free (cp);
				    cp = buf1;
				}
				(void) sprintf (obuf, "%s_%s",
						modsym (mymodule,
							top ? eval : id,
							"opt"), cp);
				fprintf (fdef, "#define\t%s (0%08o)\n", obuf,
					 1 << i);
				y -> yp_optcontrol = new_string (obuf);
				y -> yp_flags |= YP_OPTCONTROL;

				i ++;
				if (i >= 8 * sizeof (int))
				    yyerror ("too many optionals in structure");
			    }
			    break;
			}
	    }
	    if (i > 0) fprintf (fdef, "\n");

	    for (y = yp -> yp_type, i = 1; y; y = y -> yp_next, i++) {
		if (y -> yp_flags & YP_ID)
		    modsym_aux (y -> yp_id, cp = buf1);
		else
		    cp = gensym (ep, NULLCP);
		(void) sprintf (bp, " -> %s", cp);
		posy (y, 0, level, cp, ep, buf2, 0);
		*bp = NULL;
		if (!(y -> yp_flags & YP_ID))
		    free (cp);
		if (y -> yp_next)
		    fprintf (fdef, "\n");
	    }
	    if (i == 1)
		fprintf (fdef, "%*schar    dummy;\n", level * 4, "");
	    if (!top) {
		level--;

		(void) strcpy (bp = buf2, var);
		bp += strlen (bp);
		if (fflag) {
		    fprintf (fact, "\n%*sif (%s)\n%*sfree ((char *) %s);\n%*s%s = NULL;\n",
			    (level + 1) * 4, "", yp -> yp_varexp,
			    (level + 2) * 4, "", yp -> yp_varexp,
			    (level + 1) * 4, "", yp -> yp_varexp);
		    fprintf (fact, "%*s}\n", level * 4, "");
		}

		fprintf (fdef, "%*s} *%s;\n", level * 4, "",
			 array(newid, arrayflg));
		if (!Hflag)
		    free (id);
	    }
	    break;

	case YP_CHOICE: 
	    if ((cp = rindex (buf2, ' ')) && *++cp) {
		if ((dp = rindex (cp, '.')) && *++dp)
		    cp = dp;
		(void) sprintf (dp = buf1, "%*.*s",
			cp - buf2, cp - buf2, buf2);
		dp += strlen (dp);
	    }
	    else {
		(void) strcpy (buf1, buf2);
		dp = NULL;
	    }
	    newid = yp -> yp_ptrname ? yp -> yp_ptrname : id;
	    if (!top) {
		if (yp -> yp_structname)
		    id = yp -> yp_structname;
		else if (!Hflag)
		    id = gensym ("choice", NULLCP);
		if (aflag)
		    printag (yp, level + 4, NULLCP);
		fprintf (fdef, "%*sstruct %s {\n", level * 4, "", id);

		if (dp)
		    (void) strcpy (dp, newid);
		if (fflag) {
		    fprintf (fact, "%*sif (%s) {\n%*sstruct %s *%s = %s;\n\n",
			    level * 4, "", buf1,
			    (level + 1) * 4, "", id, id, buf1);
		}
		(void) strcpy (bp = buf2, id);
		bp += strlen (bp);
		yp -> yp_declexp = new_string (id);

		level++;
	    }
	    if (dp)
		(void) strcpy (dp, newid);
	    yp -> yp_varexp = new_string (buf1);
	    fprintf (fdef, "%*sint     offset;\n", level * 4, "");
	    if (fflag)
		fprintf (fact, "%*sswitch (%s -> offset) {\n",
			level * 4, "", id);
	    if (top)
		cp = modsym (mymodule, val, "type");
	    else
		cp = id;
	    (void) sprintf (ep = buf1, "%s_", cp);
	    ep += strlen (ep);
	    for (y = yp -> yp_type, i = 1; y; y = y -> yp_next, i++) {
		if (y -> yp_flags & YP_ID)
		    modsym_aux (y -> yp_id, ep);
		else
		    (void) sprintf (ep, "%d", i);
		y -> yp_offset = new_string (buf1);
		fprintf (fdef, "#define\t%s\t%d\n", y -> yp_offset, i);
	    }
	    fprintf (fdef, "\n%*sunion {\n", level * 4, "");
	    level++;
	    for (y = yp -> yp_type; y; y = y -> yp_next) {
		if (y -> yp_flags & YP_ID)
		    modsym_aux (y -> yp_id, cp = buf1);
		else
		    cp = gensym ("choice", NULLCP);
		if (fflag) {
		    fprintf (fact, "%*scase %s:\n", level * 4, "",
			    y -> yp_offset);
		    level++;
		}
		(void) sprintf (bp, " -> un.%s", cp);
		posy (y, 0, level, cp, "choice", buf2, 0);
		*bp = NULL;
		if (fflag) {
		    fprintf (fact, "%*sbreak;\n", level * 4, "");
		    if (y -> yp_next)
			fprintf (fact, "\n");
		    level--;
		}
		if (!(y -> yp_flags & YP_ID))
		    free (cp);
		if (y -> yp_next)
		    fprintf (fdef, "\n");
	    }
	    level--;
	    fprintf (fdef, "%*s}       un;\n", level * 4, "");
	    if (fflag)
		fprintf (fact, "%*s}\n", level * 4, "");
	    if (!top) {
		level--;

		(void) strcpy (bp = buf2, var);
		bp += strlen (bp);
		if (fflag) {
		    fprintf (fact, "\n%*sif (%s)\n%*sfree ((char *) %s);\n%*s%s = NULL;\n",
			    (level + 1) * 4, "", yp -> yp_varexp,
			    (level + 2) * 4, "", yp -> yp_varexp,
			    (level + 1) * 4, "", yp -> yp_varexp);
		    fprintf (fact, "%*s}\n", level * 4, "");
		}

		fprintf (fdef, "%*s} *%s;\n", level * 4, "",
			 array(newid, arrayflg));
		if (!Hflag)
		    free (id);
	    }
	    break;

	case YP_OID: 
	    if (!top) {
		if (aflag)
		    printag (yp, level + 4, NULLCP);
		fprintf (fdef, "%*sOID     %s;\n", level * 4, "",
			 array(id, arrayflg));
	    }
	    if (fflag) {
		fprintf (fact, "%*sif (%s)\n", level * 4, "", buf2);
		fprintf (fact,
			"%*soid_free (%s),\n%*s%s = NULLOID;\n",
			(level + 1) * 4, "", buf2,
			(level + 2) * 4, "", buf2);
	    }
	    yp -> yp_varexp = new_string (buf2);
	    break;

	case YP_IDEFINED: 
	    if (aflag)
		printag (yp, level + 4, NULLCP);
	    fprintf (fdef, "%*sstruct %s *%s;\n", level * 4, "",
		    modsym (yp -> yp_module, yp -> yp_identifier, "type"),
		    array(id, arrayflg));
	    if (fflag) {
		fprintf (fact, "%*sif (%s)\n", level * 4, "", buf2);
		fprintf (fact,
			"%*s%s (%s),\n%*s%s = NULL;\n",
			(level + 1) * 4, "",
			modsym (yp -> yp_module, yp -> yp_identifier, "free"),
			buf2, (level + 2) * 4, "", buf2);
	    }
	    yp -> yp_varexp = new_string (buf2);
	    break;

	default: 
	    myyerror ("unknown type: %d", yp -> yp_code);
    }
}

/*  */

static	printag (yp, level, pullup)
register YP	yp;
int	level;
char   *pullup;
{
    fprintf (fdef, "%*s/* ", level * 4, "");
    switch (yp -> yp_code) {
	case YP_IDEFINED: 
	    if (yp -> yp_module && strcmp (yp -> yp_module, mymodule))
		fprintf (fdef, "%s.", yp -> yp_module);
	    fprintf (fdef, "%s", yp -> yp_identifier);
	    break;

	default: 
	    fprintf (fdef, "%s", tags[yp -> yp_code]);
	    break;
    }
    if (pullup)
	fprintf (fdef, " pulled up from %s", pullup);
    fprintf (fdef, " */\n");
}

/*  */

static	xalloc (yp, top, level, arg, type, brackets)
register YP	yp;
int	top,
	level,
	brackets;
char   *arg,
       *type;
{
    int	    didone;
    register YP	    y;

    if (hflag && !arg && !type)
	return;

    didone = 0;

    if (arg && type) {
	if (brackets && !didone) {
	    printf ("%*s%%{\n", level * 4, "");
	    level++, didone++;
	}

	if (h2flag && (yp -> yp_code == YP_SEQTYPE ||
		       yp -> yp_code == YP_SETTYPE)) {
	    printf ("%*s{\n%*sPE      xx_pe = prim2%s ($$);\n\n",
		    level * 4, "", (level +1) * 4, "",
		    yp -> yp_code == YP_SEQTYPE ? "seq" : "set");
	    printf ("%*sn_%s = xx_pe -> pe_cardinal > 0 ",
		    (level + 1) * 4, "", arg);
	    printf ("? xx_pe -> pe_cardinal : 0;\n%*s}\n", level * 4, "");
	    printf ("%*sif ((*(%s) = (struct %s *)\n",
		    level * 4, "", arg, type);
	    printf ("%*scalloc (1 + (unsigned) n_%s, sizeof **(%s)",
		    (level + 2) * 4, "", arg, arg);
	    printf (")) == ((struct %s *) 0)) {\n", type);
	    printf ("%*sadvise (NULLCP, \"%%s\", PEPY_ERR_NOMEM);\n",
		    (level + 1) * 4, "");
	    printf ("%*sreturn NOTOK;\n%*s}\n", (level + 1) * 4, "",
		    level * 4, "");
	    printf ("%*s(*%s) -> nelem = n_%s;\n", level * 4, "", arg, arg);
	    printf ("%*sn_%s = 0;\n", level * 4, "", arg);
	} else {
	    printf ("%*sif ((*(%s) = (struct %s *)\n",
		    level * 4, "", arg, type);
	    printf ("%*scalloc (1, sizeof **(%s))) == ((struct %s *) 0)) {\n",
		    (level + 2) * 4, "", arg, type);
	    printf ("%*sadvise (NULLCP, \"%%s\", PEPY_ERR_NOMEM);\n",
		    (level + 1) * 4, "");
	    printf ("%*sreturn NOTOK;\n%*s}\n", (level + 1) * 4, "",
		    level * 4, "");
	}
    }
    switch (yp -> yp_code) {
    case YP_SEQTYPE:
    case YP_SETTYPE:
	if (top) break;
    case YP_CHOICE:
    case YP_SEQLIST:
    case YP_SETLIST:
	for (y = yp -> yp_type; y; y = y -> yp_next)
	    switch (y -> yp_code) {
	    case YP_SEQTYPE: 
	    case YP_SETTYPE:
		if (h2flag && (yp -> yp_code == YP_SETLIST ||
			       yp -> yp_code == YP_SEQLIST)) {
		    /* include allocation here - no chance later */
		    if (brackets && !didone) {
			printf ("%*s%%{\n", level * 4, "");
			level++, didone++;
		    }
		    if (y -> yp_declexp)
			printf ("%*s%s = &(%s);\n", level * 4, "",
				y -> yp_declexp,
				y -> yp_varexp);
		    xalloc (y, top, level, y -> yp_declexp,
			    y -> yp_declexp, 0);
		}
		/* and continue ... */
	    case YP_SEQLIST: 
	    case YP_SETLIST: 
	    case YP_CHOICE:
		if (brackets && !didone) {
		    printf ("%*s%%{\n", level * 4, "");
		    level++, didone++;
		}
		printf ("%*s%s = &(%s);\n",
			level * 4, "", y -> yp_declexp,
			y -> yp_varexp);
		break;
	    }
	break;
    }

    if (brackets && didone) {
	level--;
	printf ("%*s%%}\n", level * 4, "");
    }
}


static	balloc (yp, var, action2, level)
register YP	yp;
char   *var, *action2;
int	level;
{
    printf ("\n%*s%%{\n", level * 4, "");
    level++;

    printf ("%*sif ((%s%s = prim2bit (pe_cpy ($$))) == NULLPE) {\n",
	    level * 4, "", var, SVAL (yp -> yp_varexp));
    printf ("%*sadvise (NULLCP, \"%%s\", PEPY_ERR_NOMEM);\n", (level + 1) * 4, "");
    printf ("%*sreturn NOTOK;\n%*s}\n", (level + 1) * 4, "", level * 4, "");

    if (action2)
	printf ("\n%*s%s\n", level * 4, "", action2);

    level--;
    printf ("%*s%%}", level * 4, "");
}

#ifdef	notdef
static	qalloc (yp, var, action2, level)
register YP	yp;
char   *var,
       *action2;
int	level;
{
    printf ("\n%*s%%{\n", level * 4, "");
    level++;

    printf ("%*sif ((%s%s = str2qb ($$, $$_len, 1)) == ((struct qbuf *) 0)) {\n",
	    level * 4, "", var, SVAL (yp -> yp_varexp));
    printf ("%*sadvise (NULLCP, \"%%s\", PEPY_ERR_NOMEM);\n", (level + 1) * 4, "");
    printf ("%*sreturn NOTOK;\n%*s}\n", (level + 1) * 4, "", level * 4, "");

    if (action2)
	printf ("\n%*s%s\n", level * 4, "", action2);

    level--;
    printf ("%*s%%}", level * 4, "");
}
#endif

/*  */

static	choice_pullup (yp, partial)
register YP	yp;
int	partial;
{
    register YP	   *x,
		    y,
		    z,
		   *z1,
		    z2,
		    z3;

    for (x = &yp -> yp_type; y = *x; x = &y -> yp_next) {
	if (y -> yp_flags & (YP_TAG | YP_BOUND))
	    continue;

	switch (y -> yp_code) {
	    case YP_IDEFINED:
		if ((z = lookup_type (y -> yp_module, y -> yp_identifier))
			== NULLYP
			|| z -> yp_code != YP_CHOICE)
		    continue;
		choice_pullup (z2 = copy_type (z), CH_FULLY);
		goto patch;

	    case YP_CHOICE:
		choice_pullup (z2 = copy_type (y), CH_FULLY);
patch: ;
		if (partial) {
		    *x = z2;
		    z2 -> yp_next =  y -> yp_next;
		    continue;
		}
		break;

	    default:
		continue;
	}
	z = z3 = z2 -> yp_type;
	for (z1 = &z -> yp_next; z2 = *z1; z1 = &z2 -> yp_next)
	    z3 = z2;
	*z1 = y -> yp_next;
	*x = z;
	y = z3;
    }
}

/*  */

static	components_pullup (yp)
register YP	yp;
{
    register YP    *x,
		    y,
		    z,
		    z1,
		    z2;

    for (x = &yp -> yp_type; y = *x; x = &y -> yp_next) {
	if (!(y -> yp_flags & YP_COMPONENTS))
	    continue;

	switch (y -> yp_code) {
	    case YP_SEQLIST:
	    case YP_SETLIST:
	        z = y;
		break;

	    case YP_IDEFINED:
		if ((z = lookup_type (y -> yp_module, y -> yp_identifier))
		        == NULLYP) {
		    warning ("COMPONENTS OF target \"%s\" is undefined",
			     y -> yp_identifier);
		    continue;
		}
		break;
	}
	if (yp -> yp_code != z -> yp_code) {
	    warning ("COMPONENTS OF target \"%s\" is wrong type, should be %s",
		     y -> yp_code == YP_IDEFINED ? y -> yp_identifier
		     				 : y -> yp_id ? y -> yp_id
						 : "",
		     yp -> yp_code == YP_SEQLIST ? "SEQUENCE" : "SET");
	    continue;
	}
	if (z -> yp_type == NULLYP)
	    continue;
	components_pullup (z = copy_type (z));
	*x = z2 = z -> yp_type;
	for (x = &z -> yp_type; z1 = *x; x = &z1 -> yp_next)
	    z2 = z1;
	*x = y -> yp_next;
	y = z2;
    }
}

/*    VALUE HANDLING */

static int  val2int (yv)
register YV	yv;
{
    switch (yv -> yv_code) {
	case YV_BOOL:
	case YV_NUMBER:
	    return yv -> yv_number;

	case YV_STRING:
	    yyerror ("need an integer, not a string");

	case YV_IDEFINED:
	case YV_IDLIST:
	    yyerror ("haven't written symbol table for values yet");

	case YV_NULL:
	    yyerror ("need an integer, not NULL");

	default:
	    myyerror ("unknown value: %d", yv -> yv_code);
    }
/* NOTREACHED */
}

static double  val2real (yv)
register YV	yv;
{
    switch (yv -> yv_code) {
	case YV_NUMBER:
	    return yv -> yv_number;

	case YV_REAL:
	    return yv -> yv_real;

	case YV_STRING:
	    yyerror ("need an integer, not a string");

	case YV_IDEFINED:
	case YV_IDLIST:
	    yyerror ("haven't written symbol table for values yet");

	case YV_NULL:
	    yyerror ("need an integer, not NULL");

	default:
	    myyerror ("unknown value: %d", yv -> yv_code);
    }
/* NOTREACHED */
}

/*  */

static	val2prf (yv, level)
register YV	yv;
int	level;
{
    register YV    y;

    if (yv -> yv_flags & YV_ID)
	printf ("%s ", yv -> yv_id);

    if (yv -> yv_flags & YV_TYPE)	/* will this REALLY work??? */
	do_type1 (yv -> yv_type, 0, level, NULLCP, NULLCP, NULLCP, NULL);

    switch (yv -> yv_code) {
	case YV_BOOL: 
	    printf (yv -> yv_number ? "TRUE" : "FALSE");
	    break;

	case YV_NUMBER: 
	    if (yv -> yv_named)
		printf ("%s", yv -> yv_named);
	    else
		printf ("%d", yv -> yv_number);
	    break;

	case YV_REAL:
	    dump_real (yv -> yv_real);
	    break;

	case YV_STRING: 
	    printf ("\"%s\"", yv -> yv_string);
	    break;

	case YV_IDEFINED: 
	    if (yv -> yv_module)
		printf ("%s.", yv -> yv_module);
	    printf ("%s", yv -> yv_identifier);
	    break;

	case YV_IDLIST: 
	case YV_VALIST: 
	    printf ("{");
	    for (y = yv -> yv_idlist; y; y = y -> yv_next) {
		printf (" ");
		val2prf (y, level + 1);
		printf (y -> yv_next ? ", " : " ");
	    }
	    printf ("}");
	    break;

	case YV_NULL: 
	    printf ("NULL");
	    break;

	default: 
	    myyerror ("unknown value: %d", yv -> yv_code);
	/* NOTREACHED */
    }
}
static dump_real (r)
double	r;
{
#ifndef	BSD44
	extern char *ecvt ();
	char	*cp;
	char	sbuf[128];
	int	decpt, sign;

	cp = ecvt (r, 20, &decpt, &sign);
	(void) strcpy (sbuf, cp);	/* cp gets overwritten by printf */
	printf ("{ %s%s, 10, %d }", sign ? "-" : "", sbuf,
		decpt - strlen (sbuf));
#else
    register char   *cp,
		    *dp,
		    *sp;
    char    sbuf[128];

    (void) sprintf (sbuf, "%.19e", r);
    if (*(dp = sbuf) == '-')
	sp = "-", dp++;
    else
	sp = "";
    
    if (dp[1] != '.' || (cp = index (dp, 'e')) == NULL) {
	printf ("{ 0, 10, 0 } -- %s --", sbuf);
	return;
    }
    *cp++ = NULL;
    printf ("{ %s%c%s, 10, %d }",
	    sp, *dp, dp + 2, atoi (cp) - strlen (dp + 2));
#endif
}

/*  */

static int  dfl2int (yp)
register YP	yp;
{
    register YV	    yv,
		    y;

    yv = yp -> yp_default;
    switch (yv -> yv_code) {
	case YV_BOOL:
	case YV_NUMBER:
	    return yv -> yv_number;

	case YV_STRING:
	    yyerror ("need an integer, not a string");

	case YV_REAL:
	    yyerror ("need an integer, not a real");

	case YV_IDEFINED:
	    for (y = yp -> yp_value; y; y = y -> yv_next)
		if (y -> yv_code == YV_NUMBER
			&& (y -> yv_flags & YV_NAMED)
			&& strcmp (yv -> yv_identifier, y -> yv_named) == 0)
		    return y -> yv_number;
	    /* and fall */

	case YV_IDLIST:
	    yyerror ("haven't written symbol table for values yet");

	case YV_NULL:
	    yyerror ("need an integer, not NULL");

	default:
	    myyerror ("unknown value: %d", yv -> yv_code);
    }
/* NOTREACHED */
}

/*    DEBUG */

print_type (yp, level)
register YP	yp;
register int	level;
{
    register YP	    y;
    register YV	    yv;

    if (yp == NULLYP)
	return;

    fprintf (stderr, "%*scode=0x%x flags=%s direction=0x%x\n", level * 4, "",
	    yp -> yp_code, sprintb (yp -> yp_flags, YPBITS),
	    yp -> yp_direction);
    fprintf (stderr,
	    "%*sintexp=\"%s\" strexp=\"%s\" prfexp=0%o declexp=\"%s\" varexp=\"%s\"\n",
	    level * 4, "", yp -> yp_intexp, yp -> yp_strexp, yp -> yp_prfexp,
	    yp -> yp_declexp, yp -> yp_varexp);
    if (yp -> yp_param_type)
	fprintf (stderr, "%*sparameter type=\"%s\"\n", level * 4, "",
		 yp -> yp_param_type);
    if (yp -> yp_action0)
	fprintf (stderr, "%*saction0 at line %d=\"%s\"\n", level * 4, "",
		yp -> yp_act0_lineno, yp -> yp_action0);
    if (yp -> yp_action05)
	fprintf (stderr, "%*saction05 at line %d=\"%s\"\n", level * 4, "",
		yp -> yp_act05_lineno, yp -> yp_action05);
    if (yp -> yp_action1)
	fprintf (stderr, "%*saction1 at line %d=\"%s\"\n", level * 4, "",
		yp -> yp_act1_lineno, yp -> yp_action1);
    if (yp -> yp_action2)
	fprintf (stderr, "%*saction2 at line %d=\"%s\"\n", level * 4, "",
		yp -> yp_act2_lineno, yp -> yp_action2);
    if (yp -> yp_action3)
	fprintf (stderr, "%*saction3 at line %d=\"%s\"\n", level * 4, "",
		yp -> yp_act3_lineno, yp -> yp_action3);

    if (yp -> yp_flags & YP_TAG) {
	fprintf (stderr, "%*stag class=0x%x value=0x%x\n", level * 4, "",
		yp -> yp_tag -> yt_class, yp -> yp_tag -> yt_value);
	print_value (yp -> yp_tag -> yt_value, level + 1);
    }

    if (yp -> yp_flags & YP_DEFAULT) {
	fprintf (stderr, "%*sdefault=0x%x\n", level * 4, "", yp -> yp_default);
	print_value (yp -> yp_default, level + 1);
    }

    if (yp -> yp_flags & YP_ID)
	fprintf (stderr, "%*sid=\"%s\"\n", level * 4, "", yp -> yp_id);

    if (yp -> yp_flags & YP_BOUND)
	fprintf (stderr, "%*sbound=\"%s\"\n", level * 4, "", yp -> yp_bound);

    if (yp -> yp_offset)
	fprintf (stderr, "%*soffset=\"%s\"\n", level * 4, "", yp -> yp_offset);

    switch (yp -> yp_code) {
	case YP_INTLIST:
        case YP_ENUMLIST:
	case YP_BITLIST:
	    fprintf (stderr, "%*svalue=0x%x\n", level * 4, "", yp -> yp_value);
	    for (yv = yp -> yp_value; yv; yv = yv -> yv_next) {
		print_value (yv, level + 1);
		fprintf (stderr, "%*s----\n", (level + 1) * 4, "");
	    }
	    break;

	case YP_SEQTYPE:
	case YP_SEQLIST:
	case YP_SETTYPE:
	case YP_SETLIST:
	case YP_CHOICE:
	    fprintf (stderr, "%*stype=0x%x\n", level * 4, "", yp -> yp_type);
	    for (y = yp -> yp_type; y; y = y -> yp_next) {
		print_type (y, level + 1);
		fprintf (stderr, "%*s----\n", (level + 1) * 4, "");
	    }
	    break;

	case YP_IDEFINED:
	    fprintf (stderr, "%*smodule=\"%s\" identifier=\"%s\"\n",
		    level * 4, "", yp -> yp_module ? yp -> yp_module : "",
		    yp -> yp_identifier);
	    break;

	default:
	    break;
    }
}

/*  */

static	print_value (yv, level)
register YV	yv;
register int	level;
{
    register YV	    y;

    if (yv == NULLYV)
	return;

    fprintf (stderr, "%*scode=0x%x flags=%s\n", level * 4, "",
	    yv -> yv_code, sprintb (yv -> yv_flags, YVBITS));

    if (yv -> yv_action)
	fprintf (stderr, "%*saction at line %d=\"%s\"\n", level * 4, "",
		yv -> yv_act_lineno, yv -> yv_action);

    if (yv -> yv_flags & YV_ID)
	fprintf (stderr, "%*sid=\"%s\"\n", level * 4, "", yv -> yv_id);

    if (yv -> yv_flags & YV_NAMED)
	fprintf (stderr, "%*snamed=\"%s\"\n", level * 4, "", yv -> yv_named);

    if (yv -> yv_flags & YV_TYPE) {
	fprintf (stderr, "%*stype=0x%x\n", level * 4, "", yv -> yv_type);
	print_type (yv -> yv_type, level + 1);
    }

    switch (yv -> yv_code) {
	case YV_NUMBER:
	case YV_BOOL:
	    fprintf (stderr, "%*snumber=0x%x\n", level * 4, "",
		    yv -> yv_number);
	    break;

	case YV_STRING:
	    fprintf (stderr, "%*sstring=\"%s\"\n", level * 4, "",
		    yv -> yv_string);
	    break;

	case YV_IDEFINED:
	    if (yv -> yv_flags & YV_BOUND)
		fprintf (stderr, "%*smodule=\"%s\" identifier=\"%s\"\n",
			level * 4, "", yv -> yv_module, yv -> yv_identifier);
	    else
		fprintf (stderr, "%*sbound identifier=\"%s\"\n",
			level * 4, "", yv -> yv_identifier);
	    break;

	case YV_IDLIST:
	case YV_VALIST:
	    for (y = yv -> yv_idlist; y; y = y -> yv_next) {
		print_value (y, level + 1);
		fprintf (stderr, "%*s----\n", (level + 1) * 4, "");
	    }
	    break;

	default:
	    break;
    }
}

/*    SYMBOLS */

static SY  new_symbol (encpref, decpref, prfpref, mod, id, type)
register char  *encpref,
	       *decpref,
	       *prfpref,
	       *mod,
	       *id;
register YP	type;
{
    register SY    sy;

    if ((sy = (SY) calloc (1, sizeof *sy)) == NULLSY)
	yyerror ("out of memory");
    sy -> sy_encpref = encpref;
    sy -> sy_decpref = decpref;
    sy -> sy_prfpref = prfpref;
    sy -> sy_module = mod;
    sy -> sy_name = id;
    sy -> sy_type = type;

    return sy;
}


static SY  add_symbol (s1, s2)
register SY	s1,
		s2;
{
    register SY	    sy;

    if (s1 == NULLSY)
	return s2;

    for (sy = s1; sy -> sy_next; sy = sy -> sy_next)
	continue;
    sy -> sy_next = s2;

    return s1;
}

/*    MODULES */

static MD  lookup_module (module)
char   *module;
{
    register MD	    md;

    for (md = mymodules; md; md = md -> md_next)
	if (strcmp (md -> md_module, module) == 0)
	    return md;

    if ((md = (MD) calloc (1, sizeof *md)) == NULLMD)
	yyerror ("out of memory");
    md -> md_module = new_string (module);

    if (mymodules != NULLMD)
	md -> md_next = mymodules;
    mymodules = md;

    return NULLMD;
}

/*    TYPES */

YP	new_type (code)
int	code;
{
    register YP    yp;

    if ((yp = (YP) calloc (1, sizeof *yp)) == NULLYP)
	yyerror ("out of memory");
    yp -> yp_code = code;

    return yp;
}


YP	add_type (y, z)
register YP	y,
		z;
{
    register YP	    yp;

    for (yp = y; yp -> yp_next; yp = yp -> yp_next)
	continue;
    yp -> yp_next = z;

    return y;
}

/*  */

YP	copy_type (yp)
register YP	yp;
{
    register YP	    y;

    if (yp == NULLYP)
	return NULLYP;

    y = new_type (yp -> yp_code);
    y -> yp_direction = yp -> yp_direction;

    switch (yp -> yp_code) {
	case YP_IDEFINED:
	    if (yp -> yp_module)
		y -> yp_module = new_string (yp -> yp_module);
	    y -> yp_identifier = new_string (yp -> yp_identifier);
	    y -> yp_modid = oid_cpy (yp -> yp_modid);
	    break;

	case YP_SEQTYPE:
	case YP_SEQLIST:
	case YP_SETTYPE:
	case YP_SETLIST:
	case YP_CHOICE:
	    y -> yp_type = copy_type (yp -> yp_type);
	    break;

	case YP_INTLIST:
	case YP_ENUMLIST:
	case YP_BITLIST:
	    y -> yp_value = copy_value (yp -> yp_value);
	    break;

	default:
	    break;
    }

    y -> yp_intexp = yp -> yp_intexp;
    y -> yp_strexp = yp -> yp_strexp;
    y -> yp_prfexp = yp -> yp_prfexp;

    y -> yp_declexp = yp -> yp_declexp;
    y -> yp_varexp = yp -> yp_varexp;

    if (yp -> yp_structname)
	y -> yp_structname = new_string (yp -> yp_structname);
    if (yp -> yp_ptrname)
	y -> yp_ptrname = new_string (yp -> yp_ptrname);

    if (yp -> yp_param_type)
	y -> yp_param_type = new_string (yp -> yp_param_type);

    if (yp -> yp_action0) {
	y -> yp_action0 = new_string (yp -> yp_action0);
	y -> yp_act0_lineno = yp -> yp_act0_lineno;
    }

    if (yp -> yp_action05) {
	y -> yp_action05 = new_string (yp -> yp_action05);
	y -> yp_act05_lineno = yp -> yp_act05_lineno;
    }

    if (yp -> yp_action1) {
	y -> yp_action1 = new_string (yp -> yp_action1);
	y -> yp_act1_lineno = yp -> yp_act1_lineno;
    }

    if (yp -> yp_action2) {
	y -> yp_action2 = new_string (yp -> yp_action2);
	y -> yp_act2_lineno = yp -> yp_act2_lineno;
    }

    if (yp -> yp_action3) {
	y -> yp_action3 = new_string (yp -> yp_action3);
	y -> yp_act3_lineno = yp -> yp_act3_lineno;
    }

    y -> yp_flags = yp -> yp_flags;

    if (yp -> yp_flags & YP_DEFAULT)
	y -> yp_default = copy_value (yp -> yp_default);

    if (yp -> yp_flags & YP_ID)
	y -> yp_id = new_string (yp -> yp_id);

    if (yp -> yp_flags & YP_TAG)
	y -> yp_tag = copy_tag (yp -> yp_tag);

    if (yp -> yp_flags & YP_BOUND)
	y -> yp_bound = new_string (yp -> yp_bound);

    if (yp -> yp_flags & YP_PARMVAL)
	y -> yp_parm = new_string (yp -> yp_parm);

    if (yp -> yp_flags & YP_CONTROLLED)
        y -> yp_control = new_string (yp -> yp_control);

    if (yp -> yp_flags & YP_OPTCONTROL)
        y -> yp_optcontrol = new_string (yp -> yp_optcontrol);

    if (yp -> yp_offset)
	y -> yp_offset = new_string (yp -> yp_offset);

    if (yp -> yp_next)
	y -> yp_next = copy_type (yp -> yp_next);

    return y;
}

/*    VALUES */

YV	new_value (code)
int	code;
{
    register YV    yv;

    if ((yv = (YV) calloc (1, sizeof *yv)) == NULLYV)
	yyerror ("out of memory");
    yv -> yv_code = code;

    return yv;
}


YV	add_value (y, z)
register YV	y,
		z;
{
    register YV	    yv;

    for (yv = y; yv -> yv_next; yv = yv -> yv_next)
	continue;
    yv -> yv_next = z;

    return y;
}

/*  */

YV	copy_value (yv)
register YV	yv;
{
    register YV	    y;

    if (yv == NULLYV)
	return NULLYV;

    y = new_value (yv -> yv_code);
    y -> yv_flags = yv -> yv_flags;

    if (yv -> yv_action) {
	y -> yv_action = new_string (yv -> yv_action);
	y -> yv_act_lineno = yv -> yv_act_lineno;
    }

    if (yv -> yv_flags & YV_ID)
	y -> yv_id = new_string (yv -> yv_id);

    if (yv -> yv_flags & YV_NAMED)
	y -> yv_named = new_string (yv -> yv_named);

    if (yv -> yv_flags & YV_TYPE)
	y -> yv_type = copy_type (yv -> yv_type);

    switch (yv -> yv_code) {
	case YV_NUMBER:
	case YV_BOOL:
	    y -> yv_number = yv -> yv_number;
	    break;

	case YV_STRING:
	    y -> yv_string = new_string (yv -> yv_string);
	    break;

	case YV_IDEFINED:
	    if (yv -> yv_module)
		y -> yv_module = new_string (yv -> yv_module);
	    y -> yv_identifier = new_string (yv -> yv_identifier);
	    break;

	case YV_IDLIST:
	case YV_VALIST:
	    y -> yv_idlist = copy_value (yv -> yv_idlist);
	    break;

	default:
	    break;
    }

    if (yv -> yv_next)
	y -> yv_next = copy_value (yv -> yv_next);

    return y;
}

/*    TAGS */

YT	new_tag (class)
PElementClass	class;
{
    register YT    yt;

    if ((yt = (YT) calloc (1, sizeof *yt)) == NULLYT)
	yyerror ("out of memory");
    yt -> yt_class = class;

    return yt;
}

/*  */

YT	copy_tag (yt)
register YT	yt;
{
    register YT	    y;

    if (yt == NULLYT)
	return NULLYT;

    y = new_tag (yt -> yt_class);

    y -> yt_value = copy_value (yt -> yt_value);

    return y;
}

/*    STRINGS */

char   *new_string (s)
register char  *s;
{
    register char  *p;

    if ((p = malloc ((unsigned) (strlen (s) + 1))) == NULLCP)
	yyerror ("out of memory");

    (void) strcpy (p, s);
    return p;
}

/*    SYMBOLS */

static struct triple {
    char	   *t_name;
    PElementClass   t_class;
    PElementID	    t_id;
}		triples[] = {
    "IA5String", PE_CLASS_UNIV,	PE_DEFN_IA5S,
    "ISO646String", PE_CLASS_UNIV, PE_DEFN_IA5S,
    "NumericString", PE_CLASS_UNIV, PE_DEFN_NUMS,
    "PrintableString", PE_CLASS_UNIV, PE_DEFN_PRTS,
    "T61String", PE_CLASS_UNIV, PE_DEFN_T61S,
    "TeletexString", PE_CLASS_UNIV, PE_DEFN_T61S,
    "VideotexString", PE_CLASS_UNIV, PE_DEFN_VTXS,
    "GeneralizedTime", PE_CLASS_UNIV, PE_DEFN_GENT,
    "GeneralisedTime", PE_CLASS_UNIV, PE_DEFN_GENT,
    "UTCTime", PE_CLASS_UNIV, PE_DEFN_UTCT,
    "UniversalTime", PE_CLASS_UNIV, PE_DEFN_UTCT,
    "GraphicString", PE_CLASS_UNIV, PE_DEFN_GFXS,
    "VisibleString", PE_CLASS_UNIV, PE_DEFN_VISS,
    "GeneralString", PE_CLASS_UNIV, PE_DEFN_GENS,
    "EXTERNAL", PE_CLASS_UNIV, PE_CONS_EXTN,
    "ObjectDescriptor", PE_CLASS_UNIV, PE_PRIM_ODE,

    NULL
};

/*  */

static char *modsym (module, id, prefix)
register char  *module,
	       *id;
char   *prefix;
{
    char    buf1[BUFSIZ],
            buf2[BUFSIZ],
            buf3[BUFSIZ];
    register struct triple *t;
    static char buffer[BUFSIZ];

    if (module == NULLCP)
	for (t = triples; t -> t_name; t++)
	    if (strcmp (t -> t_name, id) == 0) {
		module = "UNIV";
		break;
	    }

    if (prefix)
	modsym_aux (prefix, buf1);
    modsym_aux (module ? module : mymodule, buf2);
    modsym_aux (id, buf3);
    if (prefix)
	(void) sprintf (buffer, "%s_%s_%s", buf1, buf2, buf3);
    else
	(void) sprintf (buffer, "%s_%s", buf2, buf3);

    return buffer;
}


static	modsym_aux (name, bp)
register char  *name,
	       *bp;
{
    register char   c;

    while (c = *name++)
	switch (c) {
	    case '-':
		*bp++ = '_';
		*bp++ = '_';
		break;

	    default:
		*bp++ = c;
		break;
	}

    *bp = NULL;
}

/*  */

static char *gensym (s, a)
char   *s, *a;
{
    int     i;
    register char  *p;
    char    buffer[BUFSIZ];
    static int  cP = 0;
    static int  eP = 0;
    static int  mP = 0;

    switch (*s) {
	case 'c': 
	    i = cP++;
	    break;
	case 'e': 
	    i = eP++;
	    break;
	case 'm': 
	    i = mP++;
	    break;

	default: 
	    myyerror ("unknown gensym argument \"%s\"", s);
	/* NOTREACHED */
    }
    if (a)
	(void) sprintf (buffer, "%s_%s_%d[n_%s]", s, modulename, i, a);
    else
	(void) sprintf (buffer, "%s_%s_%d", s, modulename, i);

    if ((p = malloc ((unsigned) (strlen (buffer) + 11))) == NULLCP)
	yyerror ("out of memory");

    (void) strcpy (p, buffer);
    return p;
}

/* pepy compatible routines - you know how it is ... */
init_new_file () 
{
    ;
}

end_file ()
{
    ;
}

static char *array (s, flg)
char	*s;
int	flg;
{
    static char buf[BUFSIZ];
    char	*p;

    if (!flg) return s;

    if (p = index (s, '[')) {
	(void) sprintf (buf, "%*.*s[1]", p - s, p - s, s);
	return buf;
    }
    return s;
}

static void prime_default (yp, level)
YP	yp;
int	level;
{
    switch (yp -> yp_code) {
    case YP_BOOL:
	printf ("%*s%s = %d;\n", level * 4, "",
		SVAL (yp->yp_varexp),
		val2int (yp -> yp_default) ? 1 : 0);
	break;

    case YP_INT:
	printf ("%*s%s = %d;\n", level * 4, "",
		SVAL (yp -> yp_varexp), val2int (yp -> yp_default));
	break;

    case YP_INTLIST:
    case YP_ENUMLIST:
	printf ("%*s%s = %d;\n", level * 4, "",
		SVAL (yp -> yp_varexp), dfl2int (yp));
	break;

    case YP_REAL:
	printf ("%*s%s = %g;\n", level * 4, "",
		SVAL (yp -> yp_varexp),
		val2real (yp -> yp_default));

    default:
	break;
    }
}
