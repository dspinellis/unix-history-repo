/* rosy.c - RO stub-generator (yacc-based) */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rosy/RCS/rosy.c,v 7.5 91/02/22 09:41:47 mrose Interim $";
#endif

/*
 * $Header: /f/osi/rosy/RCS/rosy.c,v 7.5 91/02/22 09:41:47 mrose Interim $
 *
 *
 * $Log:	rosy.c,v $
 * Revision 7.5  91/02/22  09:41:47  mrose
 * Interim 6.8
 * 
 * Revision 7.4  90/11/20  15:27:57  mrose
 * update
 * 
 * Revision 7.3  90/07/01  21:06:20  mrose
 * pepsy
 * 
 * Revision 7.2  90/01/27  10:27:17  mrose
 * touch-up
 * 
 * Revision 7.1  90/01/11  18:37:50  mrose
 * real-sync
 * 
 * Revision 7.0  89/11/23  22:21:50  mrose
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
#define pepyversion rosyversion
#include "rosy-defs.h"
#include "../pepsy/pass2.h"

/*    DATA */

int	Cflag = 0;		/* rosy */
int	dflag = 0;
int     Pflag = 0;		/* pepy compat... */
int	Pepsyflag = 0;		/* Pepsy compatability */
int	Defsflag = 0;		/* Produce #define function names for Pepsy */
int	doexternals;
static int linepos = 0;
static int mflag = 0;
static int rosydebug = 0;
char   *bflag = NULLCP;
static int sflag = 0;

static  char *eval = NULLCP;

char   *mymodule = "";
static char   *mymodulename = NULL;
static char    mymodaux[BUFSIZ];
static char    oplistorig[BUFSIZ];
#define NOPS 128
static char *opvp[NOPS];
static int opvc;
OID	mymoduleid;

int yysection = NULL;
char *yyencpref = "none";
char *yydecpref = "none";
char *yyprfpref = "none";
char *yyencdflt = "none";
char *yydecdflt = "none";
char *yyprfdflt = "none";

static char *yymode = "";

static char *classes[] = {
    "UNIVERSAL ",
    "APPLICATION ",
    "",
    "PRIVATE "
};

static char autogen[BUFSIZ];

char *sysin = NULLCP;
static char sysout[BUFSIZ];
static char sysdef[BUFSIZ];
static char systbl[BUFSIZ];
static char systub[BUFSIZ];

static FILE *fdef;
static FILE *ftbl;
static FILE *fstb;

typedef struct symlist {
    char   *sy_encpref;
    char   *sy_decpref;
    char   *sy_prfpref;
    char   *sy_module;
    char   *sy_name;

    union {
	YO	sy_un_op;

	YE	sy_un_err;

	YP	sy_un_type;
    }	sy_un;
#define	sy_op	sy_un.sy_un_op
#define	sy_err	sy_un.sy_un_err
#define	sy_type	sy_un.sy_un_type

    struct symlist *sy_next;
}		symlist, *SY;
#define	NULLSY	((SY) 0)

static	SY	myoperations = NULLSY;

static int      erroff = 0;
static	SY	myerrors = NULLSY;

static	SY	mytypes = NULLSY;


char   *modsym ();
char   *cmodsym ();
char   *csymmod ();
SY	new_symbol (), add_symbol ();

YE	lookup_err ();
YP	lookup_type ();

/*    MAIN */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    register char  *cp,
		   *sp;

    fprintf (stderr, "%s\n", rosyversion);

    sysout[0] = sysdef[0] = systbl[0] = systub[0] = NULL;
    for (argc--, argv++; argc > 0; argc--, argv++) {
	cp = *argv;

	if (strcmp (cp, "-pepsy") == 0) {
       	    Pepsyflag++;
	    continue;
	}
	if (strcmp (cp, "-defs") == 0) {
	    Defsflag++;
	    continue;
	}
	if (strcmp (cp, "-d") == 0) {
	    dflag++;
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
	if (strcmp (cp, "-b") == 0) {
	    if (bflag) {
		fprintf (stderr, "too many prefixes\n");
		exit (1);
	    }
	    argc--, argv++;
	    if ((bflag = *argv) == NULL || *bflag == '-')
		goto usage;
	    continue;
	}
	if (strcmp (cp, "-s") == 0) {
	    sflag++;
	    continue;
	}
	if (strcmp (cp, "-T") == 0) {
	    if (mymodulename) {
		fprintf (stderr, "too many table names\n");
		exit (1);
	    }
	    argc --; argv ++;
	    if ((mymodulename = *argv) == NULL || *mymodulename == '-')
		goto usage;
	    continue;
	}
	if (strcmp (cp, "-O") == 0) {
	    argc --; argv ++;
	    if ((cp = *argv) == NULL || (*cp == '-'))
		goto usage;
	    if (oplistorig[0])
		(void) strcat (oplistorig, cp);
	    else
		(void) strcpy (oplistorig, cp);
	    continue;
	}

	if (sysin) {
usage: ;
	    fprintf (stderr,
		"usage: rosy [-pepsy] [-d] [-o module.py] [-s] module.ry\n");
	    exit (1);
	}

	if (*cp == '-') {
	    if (*++cp != NULL)
		goto usage;
	    sysin = "";
	}
	sysin = cp;

	if (sysout[0])
	    continue;
	if (sp = rindex (cp, '/'))
	    sp++;
	if (sp == NULL || *sp == NULL)
	    sp = cp;
	sp += strlen (cp = sp) - 3;
	if (sp > cp && strcmp (sp, ".ry") == 0)
	    (void) sprintf (sysout, "%.*s.py", sp - cp, cp);
	else
	    (void) sprintf (sysout, "%s.py", cp);
    }

    switch (rosydebug = (cp = getenv ("ROSYTEST")) && *cp ? atoi (cp) : 0) {
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

    if (cp = index (rosyversion, ')'))
	for (cp++; *cp != ' '; cp++)
	    if (*cp == NULL) {
		cp = NULL;
		break;
	    }
    if (cp == NULL)
	cp = rosyversion + strlen (rosyversion);
    (void) sprintf (autogen, "%*.*s", 
	    cp - rosyversion, cp - rosyversion, rosyversion);
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
    if (*systbl)
	(void) unlink (systbl);
    if (*systub)
	(void) unlink (systub);

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
	fprintf (stderr, "%s %s: ", yymode, eval);
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

#ifdef        notyet
#ifndef       lint
static        pyyerror (va_alist)
va_dcl
{
    char    buffer[BUFSIZ];
    register YP       yp;

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
    if (*systbl)
      (void) unlink (systbl);
    if (*systub)
      (void) unlink (systub);

    exit (1);
}
#else
/* VARARGS */
+
static        pyyerror (yp, fmt)
YP    yp;
char   *fmt;
{
    pyyerror (yp, fmt);
}
#endif
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
char   *s;
int	f,
	top;
{
}

static  yyprint_aux (s, mode)
char   *s,
       *mode;
{
    int	    len;
    static int nameoutput = 0;
    static int outputlinelen = 79;

    if (sflag)
	return;

    if (strcmp (yymode, mode)) {
	if (linepos)
	    fprintf (stderr, "\n\n");

	fprintf (stderr, "%s", mymodule);
	nameoutput = (linepos = strlen (mymodule)) + 1;

	fprintf (stderr, " %ss", yymode = mode);
	linepos += strlen (yymode) + 1;
	fprintf (stderr, ":");
	linepos += 2;
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
    if (mymoduleid)
	printf ("%s ", oidprint(mymoduleid));
    printf ("DEFINITIONS ::=\n\n");
}

/*  */

pass1_op (mod, id, arg, result, errors, linked, opcode)
char   *mod,
       *id;
YP	arg,
	result;
YV	errors;
YV	linked;
int	opcode;
{
    register SY	    sy;
    register YO	    yo;

    if ((yo = (YO) calloc (1, sizeof *yo)) == NULLYO)
	yyerror ("out of memory");

    yo -> yo_name = id;
    yo -> yo_arg = arg;
    yo -> yo_result = result;
    yo -> yo_errors = errors;
    yo -> yo_linked = linked;
    yo -> yo_opcode = opcode;

    if (rosydebug) {
	if (linepos)
	    fprintf (stderr, "\n"), linepos = 0;

	fprintf (stderr, "%s.%s\n", mod ? mod : mymodule, id);
	print_op (yo, 0);
	fprintf (stderr, "--------\n");
    }
    else
	yyprint_aux (id, "operation");

    sy = new_symbol (NULLCP, NULLCP, NULLCP, mod, id);
    sy -> sy_op = yo;
    myoperations = add_symbol (myoperations, sy);
}

/*  */

pass1_err (mod, id, param, errcode)
char   *mod,
       *id;
YP	param;
int	errcode;
{
    register SY	    sy;
    register YE	    ye;

    if ((ye = (YE) calloc (1, sizeof *ye)) == NULLYE)
	yyerror ("out of memory");

    ye -> ye_name = id;
    ye -> ye_param = param;
    ye -> ye_errcode = errcode;
    ye -> ye_offset = erroff++;

    if (rosydebug) {
	if (linepos)
	    fprintf (stderr, "\n"), linepos = 0;

	fprintf (stderr, "%s.%s\n", mod ? mod : mymodule, id);
	print_err (ye, 0);
	fprintf (stderr, "--------\n");
    }
    else
	yyprint_aux (id, "error");

    sy = new_symbol (NULLCP, NULLCP, NULLCP, mod, id);
    sy -> sy_err = ye;
    myerrors = add_symbol (myerrors, sy);
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

    if (dflag && lookup_type (mod, id))	/* no duplicate entries, please... */
	return;

    if (rosydebug) {
	if (linepos)
	    fprintf (stderr, "\n"), linepos = 0;

	fprintf (stderr, "%s.%s\n", mod ? mod : mymodule, id);
	print_type (yp, 0);
	fprintf (stderr, "--------\n");
    }
    else
	if (!(yp -> yp_flags & YP_IMPORTED))
	    yyprint_aux (id, "type");

    sy = new_symbol (encpref, decpref, prfpref, mod, id);
    sy -> sy_type = yp;
    mytypes = add_symbol (mytypes, sy);
}

/*    PASS2 */

pass2 () {
    int	    i;
    register SY	    sy,
		    sy2;
    register YP	    yp;

    if (!bflag)
	bflag = mymodule;
    if (!sflag)
	(void) fflush (stderr);

    if(!mymodulename)
	mymodulename = mymodule;
    modsym_aux (mymodulename, mymodaux);
    if (oplistorig[0]) {
	opvc = sstr2arg (oplistorig, NOPS, opvp, ", \n");
	if (opvc < 0)
	    opvc = 0;
    }

    (void) sprintf (sysdef, "%s-ops.h", bflag);
    if ((fdef = fopen (sysdef, "w")) == NULL)
	myyerror ("unable to write %s", sysdef);
    fprintf (fdef, "/* automatically generated by %s, do not edit! */\n\n",
	    autogen);
    (void) sprintf (systbl, "%s-ops.c", bflag);
    if ((ftbl = fopen (systbl, "w")) == NULL)
	myyerror ("unable to write %s", systbl);
    fprintf (ftbl, "/* automatically generated by %s, do not edit! */\n\n",
	    autogen);

    if (!Pepsyflag) {
        fprintf (fdef, "#include %s\n\n\n",
		 mflag ? "\"rosy.h\"" : "<isode/rosy.h>");
    } else {
	fprintf (fdef,
		 "#ifndef\tPEPSY_VERSION\n#define\tPEPSY_VERSION\t\t%d\n",
		 PEPSY_VERSION_NUMBER);
	fprintf (fdef, "#endif\n");
        fprintf (ftbl, "#include \"%s-types.h\"\n\n", bflag); /* XXX */
        fprintf (fdef, "#include %s\n\n\n",
		 mflag ? "\"rosy.h\"" : "<isode/rosy.h>");
    }

    fprintf (ftbl, "#include <stdio.h>\n");
    fprintf (ftbl, "#include \"%s\"\n\n\n", sysdef);
    if (!Pepsyflag) {
	fprintf (ftbl, "#include \"%s-types.h\"\n\n", bflag);	/* XXX */
    }

    (void) sprintf (systub, "%s-stubs.c", bflag);
    if ((fstb = fopen (systub, "w")) == NULL)
	myyerror ("unable to write %s", systbl);
    fprintf (fstb, "/* automatically generated by %s, do not edit! */\n\n",
	    autogen);
    fprintf (fstb, "#include <stdio.h>\n");
    fprintf (fstb, "#include \"%s\"\n", sysdef);
    fprintf (fstb, "#include \"%s-types.h\"\n\n", bflag);	/* XXX */

    fprintf (fdef, "\t\t\t\t\t/* OPERATIONS */\n\n");
    fprintf (fdef, "extern struct RyOperation table_%s_Operations[];\n\n",
	mymodaux);

    fprintf (ftbl, "\t\t\t\t\t/* OPERATIONS */\n\n");

    yymode = "operation";
    for (sy = myoperations; sy; sy = sy -> sy_next) {
	if (sy -> sy_module == NULLCP)
	    yyerror ("no module name associated with symbol");

	eval = sy -> sy_name;
	if ((i = sy -> sy_op -> yo_opcode) < 0)
	    yyerror_aux ("negative operation code (warning)");
	for (sy2 = sy -> sy_next; sy2; sy2 = sy2 -> sy_next)
	    if (i == sy2 -> sy_op -> yo_opcode) {
		yyerror_aux ("non-unique operation codes (warning)");
		fprintf (stderr, "\tvalue=%d op1=%s op2=%s\n", i,
			sy -> sy_op -> yo_name, sy2 -> sy_op -> yo_name);
	    }
	if (opvc) {
	    for (i = 0; i < opvc; i++)
		if (strcmp (opvp[i], sy -> sy_op -> yo_name) == 0)
		    break;
	    if (i >= opvc)
		continue;
	}
	do_op1 (sy -> sy_op, eval);
    }

    fprintf (fdef, "\n#ifndef\tlint\n");
    fprintf (fstb, "\n#ifdef\tlint\n");
    fprintf (ftbl, "struct RyOperation table_%s_Operations[] = {\n", mymodaux);
    for (sy = myoperations; sy; sy = sy -> sy_next) {
	if (opvc) {
	    for (i = 0; i < opvc; i++)
		if (strcmp (opvp[i], sy -> sy_op -> yo_name) == 0)
		    break;
	    if (i >= opvc)
		continue;
	}
	do_op2 (sy -> sy_op, eval = sy -> sy_name);
    }
    fprintf (fdef, "#endif\n");
    fprintf (fstb, "#endif\n");
    fprintf (ftbl, "    NULL\n};\n\n");

    fprintf (fdef, "\n\n\t\t\t\t\t/* ERRORS */\n\n");
    fprintf (fdef, "extern struct RyError table_%s_Errors[];\n\n", mymodaux);
	
    fprintf (ftbl, "\n\t\t\t\t\t/* ERRORS */\n\n");

    yymode = "error";
    for (sy = myerrors; sy; sy = sy -> sy_next) {
	if (sy -> sy_module == NULLCP)
	    yyerror ("no module name associated with symbol");

	eval = sy -> sy_name;
	if ((i = sy -> sy_err -> ye_errcode) < 0)
	    yyerror_aux ("negative error code (warning)");
	for (sy2 = sy -> sy_next; sy2; sy2 = sy2 -> sy_next)
	    if (i == sy2 -> sy_err -> ye_errcode) {
		yyerror_aux ("non-unique error codes (warning)");
		fprintf (stderr, "\tvalue=%d err1=%s err2=%s\n", i,
			sy -> sy_err -> ye_name, sy2 -> sy_err -> ye_name);
	    }
	do_err1 (sy -> sy_err, eval);
    }

    fprintf (ftbl, "struct RyError table_%s_Errors[] = {\n", mymodaux);
    for (sy = myerrors; sy; sy = sy -> sy_next)
	do_err2 (sy -> sy_err, eval = sy -> sy_name);
    fprintf (ftbl, "    NULL\n};\n");

    if (Cflag)
	printf ("\n");
    printf ("BEGIN\n");

    yymode = "type";
    yyencpref = yydecpref = yyprfpref = "none";
    for (sy = mytypes; sy; sy = sy -> sy_next) {
	eval = sy -> sy_name;
	yp = sy -> sy_type;
	if (sy -> sy_module == NULLCP)
	    yyerror ("no module name associated with symbol");
	if (yp -> yp_flags & YP_IMPORTED)
	    continue;

	if (!dflag) {
	    if (!(yp -> yp_direction & YP_ENCODER))
		sy -> sy_encpref = "none";
	    if (!(yp -> yp_direction & YP_DECODER))
		sy -> sy_decpref = "none";
	    if (!(yp -> yp_direction & YP_PRINTER))
		sy -> sy_prfpref = "none";
	    if (strcmp (yyencpref, sy -> sy_encpref)
		    || strcmp (yydecpref, sy -> sy_decpref)
		    || strcmp (yyprfpref, sy -> sy_prfpref))
		printf ("\nSECTIONS %s %s %s\n",
			yyencpref = sy -> sy_encpref,
			yydecpref = sy -> sy_decpref,
			yyprfpref = sy -> sy_prfpref);
	}
	printf ("\n%s", sy -> sy_name);
	if (yp -> yp_action0)
	    act2prf (yp -> yp_action0, 1, "\n%*s%%{", "%%}\n%*s");
	else
	    printf (" ");
	printf ("::=\n");
	if (!dflag && !(yp -> yp_flags & YP_PULLEDUP) && yp -> yp_action1) {
	    act2prf (yp -> yp_action1, 1, "%*s%%{", "%%}\n");
	    yp -> yp_flags |= YP_PULLEDUP;
	}
	do_type (yp, (yp -> yp_flags & YP_TAG) ? 1 : 2, eval);
	printf ("\n");

	if (ferror (stdout) || ferror (fdef) || ferror (ftbl) || ferror (fstb))
	    myyerror ("write error - %s", sys_errname (errno));

    }

    printf ("\nEND\n");

    (void) fflush (stdout);
    (void) fflush (fdef);
    (void) fclose (ftbl);
    (void) fclose (fstb);

    if (ferror (stdout) || ferror (fdef) || ferror (ftbl) || ferror (fstb))
	myyerror ("write error - %s", sys_errname (errno));

    (void) fclose (fdef);

    (void) fclose (ftbl);

    (void) fclose (fstb);
}

/*  */

/* ARGSUSED */

static	do_op1 (yo, id)
register YO	yo;
char   *id;
{
    register YE	    ye;
    register YP	    yp;
    register YV	    yv;

    fprintf (fdef, "\t\t\t\t\t/* OPERATION %s */\n", yo -> yo_name);
    fprintf (fdef, "#define operation_%s\t%d\n\n",
	    modsym (mymodule, yo -> yo_name, NULLCP), yo -> yo_opcode);

    fprintf (ftbl, "\t\t\t\t\t/* OPERATION %s */\n", yo -> yo_name);

    normalize (&yo -> yo_arg, yo -> yo_name);
    if (!Pepsyflag && (yp = yo -> yo_arg)) {
	fprintf (ftbl, "int\t%s (),\n",
		modsym (yp -> yp_module, yp -> yp_identifier, "encode"));
	fprintf (ftbl, "\t%s (),\n",
		modsym (yp -> yp_module, yp -> yp_identifier, "decode"));
	fprintf (ftbl, "\t%s ();\n",
		modsym (yp -> yp_module, yp -> yp_identifier, "free"));
    }

    normalize (&yo -> yo_result, yo -> yo_name);
    if (!Pepsyflag && (yp = yo -> yo_result)) {
	fprintf (ftbl, "int\t%s (),\n",
		modsym (yp -> yp_module, yp -> yp_identifier, "encode"));
	fprintf (ftbl, "\t%s (),\n",
		modsym (yp -> yp_module, yp -> yp_identifier, "decode"));
	fprintf (ftbl, "\t%s ();\n\n",
		modsym (yp -> yp_module, yp -> yp_identifier, "free"));
    }

    if (!Pepsyflag) {
	fprintf (fdef, "#ifdef\tINVOKER\n");
    }
    if (!Pepsyflag || Defsflag) {
	fprintf (fdef, "#define\t%s_argument\t",
		modsym (mymodule, yo -> yo_name, "encode"));
	if (yp = yo -> yo_arg)
	    fprintf (fdef, "%s\n",
		    modsym (yp -> yp_module, yp -> yp_identifier, "encode"));
	else
	    fprintf (fdef, "NULLIFP\n");
	fprintf (fdef, "#define\t%s_result\t",
		modsym (mymodule, yo -> yo_name, "decode"));
	if (yp = yo -> yo_result)
	    fprintf (fdef, "%s\n",
		    modsym (yp -> yp_module, yp -> yp_identifier, "decode"));
	else
	    fprintf (fdef, "NULLIFP\n");
	fprintf (fdef, "#define\t%s_result\t",
		modsym (mymodule, yo -> yo_name, "free"));
	if (yp = yo -> yo_result)
	    fprintf (fdef, "%s\n",
		    modsym (yp -> yp_module, yp -> yp_identifier, "free"));
	else
	    fprintf (fdef, "NULLIFP\n");
    }
    if (!Pepsyflag) {
	fprintf (fdef, "#else\n");
	fprintf (fdef, "#define\t%s_argument\tNULLIFP\n",
		modsym (mymodule, yo -> yo_name, "encode"));
	fprintf (fdef, "#define\t%s_result\tNULLIFP\n",
		modsym (mymodule, yo -> yo_name, "decode"));
	fprintf (fdef, "#define\t%s_result\tNULLIFP\n",
		modsym (mymodule, yo -> yo_name, "free"));
	fprintf (fdef, "#endif\n\n");

	fprintf (fdef, "#ifdef\tPERFORMER\n");
    }
    if (!Pepsyflag || Defsflag) {
	fprintf (fdef, "#define\t%s_argument\t",
		modsym (mymodule, yo -> yo_name, "decode"));
	if (yp = yo -> yo_arg)
	    fprintf (fdef, "%s\n",
		    modsym (yp -> yp_module, yp -> yp_identifier, "decode"));
	else
	    fprintf (fdef, "NULLIFP\n");
	fprintf (fdef, "#define\t%s_argument\t",
		modsym (mymodule, yo -> yo_name, "free"));
	if (yp = yo -> yo_arg)
	    fprintf (fdef, "%s\n",
		    modsym (yp -> yp_module, yp -> yp_identifier, "free"));
	else
	    fprintf (fdef, "NULLIFP\n");
	fprintf (fdef, "#define\t%s_result\t",
		modsym (mymodule, yo -> yo_name, "encode"));
	if (yp = yo -> yo_result)
	    fprintf (fdef, "%s\n",
		    modsym (yp -> yp_module, yp -> yp_identifier, "encode"));
	else
	    fprintf (fdef, "NULLIFP\n");
    }
    if (!Pepsyflag) {
	fprintf (fdef, "#else\n");
	fprintf (fdef, "#define\t%s_argument\tNULLIFP\n",
		modsym (mymodule, yo -> yo_name, "decode"));
	fprintf (fdef, "#define\t%s_argument\tNULLIFP\n",
		modsym (mymodule, yo -> yo_name, "free"));
	fprintf (fdef, "#define\t%s_result\tNULLIFP\n",
		modsym (mymodule, yo -> yo_name, "encode"));
	fprintf (fdef, "#endif\n\n");
    }

    if (yv = yo -> yo_errors) {
	if (yv -> yv_code != YV_VALIST)
	    myyerror ("unexpected value: %d", yv -> yv_code);
	fprintf (ftbl, "static struct RyError *errors_%s[] = {\n",
	    modsym (mymodulename, yo -> yo_name, NULLCP));
	for (yv = yv -> yv_idlist; yv; yv = yv -> yv_next) {
	    ye = lookup_err (yv);
	    fprintf (ftbl, "    &table_%s_Errors[%d]%s\n", mymodaux,
		    ye -> ye_offset, yv -> yv_next ? "," : "");
	}
	fprintf (ftbl, "};\n\n");
    }

    fprintf (ftbl, "\n");
}

/*  */

/* ARGSUSED */

static	do_op2 (yo, id)
register YO	yo;
char   *id;
{
    register YP	    yp;

    fprintf (fdef, "\n#define stub_%s(sd,id,in,rfx,efx,class,roi)\\\n",
	     modsym (mymodule, yo -> yo_name, NULLCP));
    fprintf (fdef, "RyStub ((sd), table_%s_Operations,", mymodaux);
    fprintf (fdef, " operation_%s, (id), NULLIP,\\\n",
	    modsym (mymodule, yo -> yo_name, NULLCP));
    fprintf (fdef, "\t(caddr_t) (in), (rfx), (efx), (class), (roi))\n");

    fprintf (fdef, "\n#define op_%s(sd,in,out,rsp,roi)\\\n",
	    modsym (mymodule, yo -> yo_name, NULLCP));
    fprintf (fdef, "RyOperation ((sd), table_%s_Operations,", mymodaux);
    fprintf (fdef,
	     " operation_%s,\\\n\t(caddr_t) (in), (out), (rsp), (roi))\n",
	     modsym (mymodule, yo -> yo_name, NULLCP));

    fprintf (fstb, "\nint\tstub_%s (sd, id, in, rfx, efx, class, roi)\n",
	    modsym (mymodule, yo -> yo_name, NULLCP));
    fprintf (fstb, "int\tsd,\n\tid,\n\tclass;\n");
    if (yp = yo -> yo_arg)
	fprintf (fstb, "struct %s*",
		modsym (yp -> yp_module, yp -> yp_identifier, "type"));
    else
	fprintf (fstb, "caddr_t");
    fprintf (fstb, " in;\n");
    fprintf (fstb,
	     "IFP\trfx,\n\tefx;\nstruct RoSAPindication *roi;\n");
    fprintf (fstb, "{\n    return RyStub (sd, table_%s_Operations, ",
		mymodaux);
    fprintf (fstb,
	     "operation_%s, id, NULLIP,\n\t\t(caddr_t) in, rfx, efx, class, roi);\n",
	     modsym (mymodule, yo -> yo_name, NULLCP));
    fprintf (fstb, "}\n");

    fprintf (fstb, "\nint\top_%s (sd, in, out, rsp, roi)\n",
	    modsym (mymodule, yo -> yo_name, NULLCP));
    fprintf (fstb, "int\tsd;\n");
    if (yp = yo -> yo_arg)
	fprintf (fstb, "struct %s*",
		modsym (yp -> yp_module, yp -> yp_identifier, "type"));
    else
	fprintf (fstb, "caddr_t");
    fprintf (fstb, " in;\n");
    fprintf (fstb,
	    "caddr_t *out;\nint    *rsp;\nstruct RoSAPindication *roi;\n");
    fprintf (fstb, "{\n    return RyOperation (sd, table_%s_Operations, ",
		mymodaux);
    fprintf (fstb, "operation_%s,\n\t\t(caddr_t) in, out, rsp, roi);\n",
	    modsym (mymodule, yo -> yo_name, NULLCP));
    fprintf (fstb, "}\n");

    fprintf (ftbl, "\t\t\t\t\t/* OPERATION %s */\n", yo -> yo_name);

    fprintf (ftbl, "    \"%s\", operation_%s,\n",
	    yo -> yo_name, modsym (mymodule, yo -> yo_name, NULLCP));
    if (Pepsyflag) {
	if ((yp = yo->yo_arg)) {
	    if (yp->yp_code != YP_IDEFINED) {
		fprintf(stderr, "\ndo_op2:arg: internal error for %s\n",
			yo->yo_name);
		exit(1);
	    }
	    fprintf (ftbl, "\t&%s,\n ",
	        cmodsym(yp->yp_module, MODTYP_SUFFIX, PREFIX,
			yp->yp_identifier));
	    fprintf (ftbl, "\t%s,\n",
	        csymmod(yp->yp_module, yp->yp_identifier, PREFIX));
	} else {
	    fprintf (ftbl, "\tNULL,\n ");
	    fprintf (ftbl, "\tNULL,\n");
	}
    } else {
	fprintf (ftbl, "\t%s_argument,\n ",
		modsym (mymodule, yo -> yo_name, "encode"));
	fprintf (ftbl, "\t%s_argument,\n",
		modsym (mymodule, yo -> yo_name, "decode"));
	fprintf (ftbl, "\t%s_argument,\n",
		modsym (mymodule, yo -> yo_name, "free"));
    }

    if (Pepsyflag) {
	fprintf (ftbl, "\t%d,\n", yo -> yo_result ? 1 : 0);
	if ((yp = yo->yo_result)) {
	    if (yp->yp_code != YP_IDEFINED) {
		fprintf(stderr, "\ndo_op2:result: internal error for %s\n",
		     yo->yo_name);
		exit(1);
	    }
	    fprintf (ftbl, "\t&%s,\n ",
	        cmodsym(yp->yp_module, MODTYP_SUFFIX, PREFIX,
			yp -> yp_identifier));
	    fprintf (ftbl, "\t%s,\n",
	        csymmod(yp->yp_module, yp->yp_identifier, PREFIX));
	} else {
	    fprintf (ftbl, "\tNULL,\n ");
	    fprintf (ftbl, "\tNULL,\n");
	}
    } else {
	fprintf (ftbl, "\t%d, %s_result,\n",
		yo -> yo_result ? 1 : 0,
		modsym (mymodule, yo -> yo_name, "encode"));
	fprintf (ftbl, "\t   %s_result,\n",
		modsym (mymodule, yo -> yo_name, "decode"));
	fprintf (ftbl, "\t   %s_result,\n",
		modsym (mymodule, yo -> yo_name, "free"));
    }

    if (yo -> yo_errors)
	fprintf (ftbl, "\terrors_%s",
		modsym (mymodule, yo -> yo_name, NULLCP));
    else
	fprintf (ftbl, "\tNULL");
    fprintf (ftbl, ",\n\n");
}

/*  */

/* ARGSUSED */

static	do_err1 (ye, id)
register YE	ye;
char   *id;
{
    register YP	    yp;

    fprintf (fdef, "\t\t\t\t\t/* ERROR %s */\n", ye -> ye_name);
    fprintf (fdef, "#define error_%s\t%d\n\n", 
		modsym (mymodule, ye -> ye_name, NULLCP), ye -> ye_errcode);

    normalize (&ye -> ye_param, ye -> ye_name);
    if (!Pepsyflag) {
	if (yp = ye -> ye_param) {
	    fprintf (ftbl, "int\t%s (),\n",
		    modsym (yp -> yp_module, yp -> yp_identifier, "encode"));
	    fprintf (ftbl, "\t%s (),\n",
		    modsym (yp -> yp_module, yp -> yp_identifier, "decode"));
	    fprintf (ftbl, "\t%s ();\n",
		    modsym (yp -> yp_module, yp -> yp_identifier, "free"));
	}
	fprintf (fdef, "#ifdef\tINVOKER\n");
	fprintf (fdef, "#define\t%s_parameter\t",
		modsym (mymodule, ye -> ye_name, "decode"));
	if (yp = ye -> ye_param)
	    fprintf (fdef, "%s\n",
		    modsym (yp -> yp_module, yp -> yp_identifier, "decode"));
	else
	    fprintf (fdef, "NULLIFP\n");
	fprintf (fdef, "#define\t%s_parameter\t",
		modsym (mymodule, ye -> ye_name, "free"));
	if (yp = ye -> ye_param)
	    fprintf (fdef, "%s\n",
		    modsym (yp -> yp_module, yp -> yp_identifier, "free"));
	else
	    fprintf (fdef, "NULLIFP\n");
	fprintf (fdef, "#else\n");
	fprintf (fdef, "#define\t%s_parameter\tNULLIFP\n",
		modsym (mymodule, ye -> ye_name, "decode"));
	fprintf (fdef, "#define\t%s_parameter\tNULLIFP\n",
		modsym (mymodule, ye -> ye_name, "free"));
	fprintf (fdef, "#endif\n\n");

	fprintf (fdef, "#ifdef\tPERFORMER\n");
	fprintf (fdef, "#define\t%s_parameter\t",
		modsym (mymodule, ye -> ye_name, "encode"));
	if (yp = ye -> ye_param)
	    fprintf (fdef, "%s\n",
		    modsym (yp -> yp_module, yp -> yp_identifier, "encode"));
	else
	    fprintf (fdef, "NULLIFP\n");
	fprintf (fdef, "#else\n");
	fprintf (fdef, "#define\t%s_parameter\tNULLIFP\n",
		modsym (mymodule, ye -> ye_name, "encode"));
	fprintf (fdef, "#endif\n\n\n");
     }
}

/*  */

/* ARGSUSED */

static	do_err2 (ye, id)
register YE	ye;
char   *id;
{
    register YP	yp;

    fprintf (ftbl, "\t\t\t\t\t/* ERROR %s */\n", ye -> ye_name);

    fprintf (ftbl, "    \"%s\", error_%s,\n",
	    ye -> ye_name, modsym (mymodule, ye -> ye_name, NULLCP));

    if (Pepsyflag) {
	if ((yp = ye->ye_param)) {
	    fprintf (ftbl, "\t&%s,\n ",
	        cmodsym(yp->yp_module, MODTYP_SUFFIX, PREFIX,
			yp->yp_identifier));
	    fprintf (ftbl, "\t%s,\n",
	        csymmod(yp->yp_module, yp->yp_identifier, PREFIX));
	} else {
	    fprintf (ftbl, "\tNULL,\n\tNULL,\n");
	}
    } else {
	fprintf (ftbl, "\t%s_parameter,\n",
		modsym (mymodule, ye -> ye_name, "encode"));
	fprintf (ftbl, "\t%s_parameter,\n",
		modsym (mymodule, ye -> ye_name, "decode"));
	fprintf (ftbl, "\t%s_parameter,\n\n",
		modsym (mymodule, ye -> ye_name, "free"));
    }
}

/*  */

/* ARGSUSED */

static	do_type (yp, level, id)
register YP yp;
int	level;
char   *id;
{
    register YP	    y;
    register YV	    yv;
    register YT	    yt;

    printf ("%*s", level * 4, "");

    if (yp -> yp_flags & YP_ID) {
	printf ("%s", yp -> yp_id);
	if (!(yp -> yp_flags & YP_TAG)) {
	    printf ("\n%*s", ++level * 4, "");
	    if (!dflag
		    && !(yp -> yp_flags & YP_PULLEDUP)
		    && yp -> yp_action1) {
		act2prf (yp -> yp_action1, level, "%%{", "%%}\n%*s");
		yp -> yp_flags |= YP_PULLEDUP;
	    }
	}
    }

    if (yp -> yp_flags & YP_TAG) {
	if (!(yt = yp -> yp_tag))
	    myyerror ("lost tag");
	printf ("[%s%d]\n", classes[yt -> yt_class], val2int (yt -> yt_value));
	level++;
	printf ("%*s", level * 4, "");
	if (!dflag && !(yp -> yp_flags & YP_PULLEDUP) && yp -> yp_action1) {
	    act2prf (yp -> yp_action1, level, "%%{", "%%}\n%*s");
	    yp -> yp_flags |= YP_PULLEDUP;
	}
	if (yp -> yp_flags & YP_IMPLICIT)
	    printf ("IMPLICIT ");
    }
    if (yp -> yp_flags & YP_BOUND)
	printf ("%s < ", yp -> yp_bound);
    if (yp -> yp_flags & YP_COMPONENTS)
	printf ("COMPONENTS OF ");
    if (yp -> yp_flags & YP_ENCRYPTED)
	printf ("ENCRYPTED ");

    switch (yp -> yp_code) {
	case YP_BOOL: 
	    printf ("BOOLEAN");
	    if (!dflag && yp -> yp_intexp)
		printf ("\n%*s[[b %s]]", level * 4, "", yp -> yp_intexp);
	    break;

	case YP_INT: 
	    printf ("INTEGER");
	    if (!dflag && yp -> yp_intexp)
		printf ("\n%*s[[i %s]]", level * 4, "", yp -> yp_intexp);
	    break;

	case YP_INTLIST: 
	case YP_ENUMLIST:
	    if (yp -> yp_code == YP_ENUMLIST)
		printf ("ENUMERATED");
	    else
		printf ("INTEGER");
	    if (!dflag && yp -> yp_intexp)
		printf ("\n%*s[[i %s]]\n%*s{\n",
		    level * 4, "", yp -> yp_intexp, level * 4, "");
	    else
		printf (" {\n");
	    level++;
	    for (yv = yp -> yp_value; yv; yv = yv -> yv_next) {
		if (!(yv -> yv_flags & YV_NAMED))
		    myyerror ("lost named number");
		printf ("%*s%s(%d)", level * 4, "", yv -> yv_named,
			val2int (yv));
		if (!dflag && yv -> yv_action)
		    printf (" %%{%s%%}", yv -> yv_action);
		printf ("%s\n", yv -> yv_next ? "," : "");
	    }
	    level--;
	    printf ("%*s}", level * 4, "");
	    break;

	case YP_BIT: 
	    printf ("BIT STRING");
	    if (!dflag && yp -> yp_strexp)
		printf ("\n%*s[[x %s$%s]]", level * 4, "", yp -> yp_strexp,
			yp -> yp_intexp);
	    break;

	case YP_BITLIST: 
	    if (!dflag && yp -> yp_strexp)
		printf ("BIT STRING\n%*s[[x %s$%s]]\n%*s{\n",
			level * 4, "", yp -> yp_strexp, yp -> yp_intexp,
			level * 4, "");
	    else
		printf ("BIT STRING {\n");
	    level++;
	    for (yv = yp -> yp_value; yv; yv = yv -> yv_next) {
		if (!(yv -> yv_flags & YV_NAMED))
		    myyerror ("lost named number");
		printf ("%*s%s(%d)", level * 4, "", yv -> yv_named,
			val2int (yv));
		if (!dflag && yv -> yv_action)
		    printf (" %%{%s%%}", yv -> yv_action);
		printf ("%s\n", yv -> yv_next ? "," : "");
	    }
	    level--;
	    printf ("%*s}", level * 4, "");
	    break;

	case YP_OCT: 
	    printf ("OCTET STRING");
	    if (dflag)
		break;
	    if (yp -> yp_intexp)
		printf ("\n%*s[[o %s$%s]]", level * 4, "", yp -> yp_strexp,
			yp -> yp_intexp);
	    else
		if (yp -> yp_strexp)
		    printf ("\n%*s[[%c %s]]", level * 4, "", yp -> yp_prfexp,
			    yp -> yp_strexp);
	    break;

	case YP_NULL: 
	    printf ("NULL");
	    break;

	case YP_REAL:
	    printf ("REAL");
	    if (!dflag && yp -> yp_strexp)
		    printf ("\n%*s[[r %s ]]", level * 4, "", yp -> yp_strexp);
	    break;

	case YP_SEQ: 
	    printf ("SEQUENCE");
	    break;

	case YP_SEQTYPE: 
	    printf ("SEQUENCE OF");
	    if (yp -> yp_structname) {
		printf (" %%[ %s ", yp -> yp_structname);
		if (yp -> yp_ptrname)
		    printf ("$ %s ", yp -> yp_ptrname);
		printf ("%%]\n");
	    }
	    else
		printf ("\n");
	    if (!dflag && yp -> yp_action3)
		act2prf (yp -> yp_action3, level + 1, "%*s%%{", "%%}\n");
	    if (yp -> yp_flags & YP_CONTROLLED)
		printf ("%*s<<%s>>\n", (level + 1) * 4, "", yp -> yp_control);
	    if (!yp -> yp_type)
		myyerror ("lost sequence type");
	    do_type (yp -> yp_type, level + 1, "element");
	    break;

	case YP_SEQLIST: 
	    printf ("SEQUENCE");
	    if (yp -> yp_structname) {
		printf (" %%[ %s ", yp -> yp_structname);
		if (yp -> yp_ptrname)
		    printf ("$ %s ", yp -> yp_ptrname);
		printf ("%%]");
	    }
	    if (!dflag && !(yp -> yp_flags & YP_PULLEDUP) && yp -> yp_action1)
		act2prf (yp -> yp_action1, level, "\n%*s    %%{",
			"    %%}\n%*s{\n");
	    else
		printf (yp -> yp_type ? " {\n" : " {");
	    for (y = yp -> yp_type; y; y = y -> yp_next) {
		do_type (y,
			level + ((y -> yp_flags & (YP_ID | YP_TAG)) ? 1 : 2),
			"element");
		printf ("%s\n", y -> yp_next ? ",\n" : "");
	    }
	    printf (yp -> yp_type ? "%*s}" : "}", level * 4, "");
	    break;

	case YP_SET: 
	    printf ("SET");
	    break;

	case YP_SETTYPE: 
	    printf ("SET OF");
	    if (yp -> yp_structname) {
		printf (" %%[ %s ", yp -> yp_structname);
		if (yp -> yp_ptrname)
		    printf ("$ %s ", yp -> yp_ptrname);
		printf ("%%]\n");
	    }
	    else
		printf ("\n");
	    if (!dflag && yp -> yp_action3)
		act2prf (yp -> yp_action3, level + 1, "%*s%%{", "%%}\n");
	    if (yp -> yp_flags & YP_CONTROLLED)
		printf ("%*s<<%s>>\n", (level + 1) * 4, "", yp -> yp_control);
	    if (!yp -> yp_type)
		myyerror ("lost set type");
	    do_type (yp -> yp_type, level + 1, "member");
	    break;

	case YP_SETLIST: 
	    printf ("SET");
	    if (yp -> yp_structname) {
		printf (" %%[ %s ", yp -> yp_structname);
		if (yp -> yp_ptrname)
		    printf ("$ %s ", yp -> yp_ptrname);
		printf ("%%]");
	    }
	    if (!dflag && !(yp -> yp_flags & YP_PULLEDUP) && yp -> yp_action1)
		act2prf (yp -> yp_action1, level, "\n%*s    %%{",
			"    %%}\n%*s{\n");
	    else
		printf (yp -> yp_type ? " {\n" : " {");
	    for (y = yp -> yp_type; y; y = y -> yp_next) {
		do_type (y,
			level + ((y -> yp_flags & (YP_ID | YP_TAG)) ? 1 : 2),
			"member");
		printf ("%s\n", y -> yp_next ? ",\n" : "");
	    }
	    printf (yp -> yp_type ? "%*s}" : "}", level * 4, "");
	    break;

	case YP_CHOICE: 
	    printf ("CHOICE");
	    if (yp -> yp_structname) {
		printf (" %%[ %s ", yp -> yp_structname);
		if (yp -> yp_ptrname)
		    printf ("$ %s ", yp -> yp_ptrname);
		printf ("%%]");
	    }
	    if (!dflag
		    && !(yp -> yp_flags & YP_PULLEDUP)
		    && yp -> yp_action1) {
		act2prf (yp -> yp_action1, level, "\n%*s    %%{",
			"    %%}\n%*s");
		if (yp -> yp_flags & YP_CONTROLLED)
		    printf ("    ");
	    }
	    else
		printf (" ");
	    if (yp -> yp_flags & YP_CONTROLLED)
		printf ("<<%s>> ", yp -> yp_control);
	    printf ("{\n");
	    for (y = yp -> yp_type; y; y = y -> yp_next) {
		do_type (y,
			level + ((y -> yp_flags & (YP_ID | YP_TAG)) ? 1 : 2),
			"choice");
		printf ("%s\n", y -> yp_next ? ",\n" : "");
	    }
	    printf ("%*s}", level * 4, "");
	    break;

	case YP_ANY: 
	    printf ("ANY");
	    break;

	case YP_OID: 
	    printf ("OBJECT IDENTIFIER");
	    if (!dflag && yp -> yp_strexp)
		printf ("\n%*s[[O %s]]", level * 4, "", yp -> yp_strexp);
	    break;

	case YP_IDEFINED: 
	    if (yp -> yp_module && strcmp (yp -> yp_module, mymodule))
		printf ("%s.", yp -> yp_module);
	    printf ("%s", yp -> yp_identifier);
	    if (yp -> yp_intexp) {
		if (yp -> yp_strexp)
		    printf ("\n%*s[[%c %s$%s]]", level * 4, "",
			    yp -> yp_prfexp, yp -> yp_strexp, yp -> yp_intexp);
		else
		    printf ("\n%*s[[%c %s]]", level * 4, "",
			    yp -> yp_prfexp, yp -> yp_intexp);
	    }
	    else
		if (yp -> yp_strexp)
		    printf ("\n%*s[[%c %s]]", level * 4, "",
			    yp -> yp_prfexp, yp -> yp_strexp);
	    if (yp -> yp_flags & YP_PARMVAL)
		printf ("\n%*s[[p %s]]", level * 4, "", yp -> yp_parm);
	    break;

	default: 
	    myyerror ("unknown type: %d", yp -> yp_code);
    }

    if (!dflag && yp -> yp_action2)
	act2prf (yp -> yp_action2, level, "\n%*s%%{", "%%}");

    if (yp -> yp_flags & YP_OPTIONAL)
	printf ("\n%*sOPTIONAL", level * 4, "");
    else
	if (yp -> yp_flags & YP_DEFAULT) {
	    if (!yp -> yp_default)
		myyerror ("lost default");
	    printf ("\n%*sDEFAULT ", level * 4, "");
	    val2prf (yp -> yp_default, level + 2);
	}
    if (yp -> yp_flags & YP_OPTCONTROL)
	printf (" <<%s>>", yp -> yp_optcontrol);
}

/*    ERROR HANDLING */

static YE  lookup_err (yv)
YV	yv;
{
    register char  *id,
                   *mod;
    register    SY sy;

    if (yv -> yv_code != YV_IDEFINED)
	myyerror ("unexpected value: %d", yv -> yv_code);
    id = yv -> yv_identifier;
    mod = yv -> yv_module;

    for (sy = myerrors; sy; sy = sy -> sy_next) {
	if (mod) {
	    if (strcmp (sy -> sy_module, mod))
		continue;
	}
	else
	    if (strcmp (sy -> sy_module, mymodule))
		continue;

	if (strcmp (sy -> sy_name, id) == 0)
	    return sy -> sy_err;
    }

    if (mod)
	myyerror ("error %s.%s undefined", mod, id);
    else
	myyerror ("error %s undefined", id);
/* NOTREACHED */
}

/*    TYPE HANDLING */

static YP  lookup_type (mod, id)
register char *mod,
	      *id;
{
    register SY	    sy;

    for (sy = mytypes; sy; sy = sy -> sy_next) {
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

static  normalize (yp, id)
YP     *yp;
char   *id;
{
    int	    i;
    register YP	    y,
		    z;
    char    buffer[BUFSIZ];

    if ((y = *yp) == NULLYP || y -> yp_code == YP_IDEFINED)
	return;
    y -> yp_id = NULLCP;
    y -> yp_flags &= ~YP_ID;

    (void) sprintf (buffer, "Pseudo-%s", id);
    for (i = 1; lookup_type (mymodule, buffer); i++)
	(void) sprintf (buffer, "Pseudo-%s-%d", id, i);

    z = new_type (YP_IDEFINED);
    z -> yp_identifier = new_string (buffer);
    *yp = z;

    pass1_type (yyencpref, yydecpref, yyprfpref, mymodule, new_string (buffer),
	    y);
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

	case YV_VALIST:
	    yyerror ("need an integer, not a list of values");

	case YV_NULL:
	    yyerror ("need an integer, not NULL");

	case YV_REAL:
	    yyerror ("need and integer, not a REAL");

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
	do_type (yv -> yv_type, level, NULLCP);

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

	case YV_REAL:
	    dump_real (yv -> yv_real);
	    break;

	default: 
	    myyerror ("unknown value: %d", yv -> yv_code);
	/* NOTREACHED */
    }
}

static dump_real (r)
double  r;
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


/*    ACTION HANDLING */

static	act2prf (cp, level, e1, e2)
char   *cp,
       *e1,
       *e2;
int	level;
{
    register int    i,
                    j,
                    l4;
    register char  *dp,
                   *ep,
                   *fp;
    char   *gp;

    if (e1)
	printf (e1, level * 4, "");

    if (!(ep = index (dp = cp, '\n'))) {
	printf ("%s", dp);
	goto out;
    }

    for (;;) {
	i = expand (dp, ep, &gp);
	if (gp) {
	    if (i == 0)
		printf ("%*.*s\n", ep - dp, ep - dp, dp);
	    else
		break;
	}

	if (!(ep = index (dp = ep + 1, '\n'))) {
	    printf ("%s", dp);
	    return;
	}
    }


    printf ("\n");
    l4 = (level + 1) * 4;
    for (; *dp; dp = fp) {
	if (ep = index (dp, '\n'))
	    fp = ep + 1;
	else
	    fp = ep = dp + strlen (dp);

	j = expand (dp, ep, &gp);
	if (gp == NULL) {
	    if (*fp)
		printf ("\n");
	    continue;
	}

	if (j < i)
	    j = i;
	if (j)
	    printf ("%*s", l4 + j - i, "");
	printf ("%*.*s\n", ep - gp, ep - gp, gp);
    }

    printf ("%*s", level * 4, "");
out: ;
    if (e2)
	printf (e2, level * 4, "");
}


static	expand (dp, ep, gp)
register char  *dp,
	       *ep;
char  **gp;
{
    register int    i;

    *gp = NULL;
    for (i = 0; dp < ep; dp++) {
	switch (*dp) {
	    case ' ': 
		i++;
		continue;

	    case '\t': 
		i += 8 - (i % 8);
		continue;

	    default: 
		*gp = dp;
		break;
	}
	break;
    }

    return i;
}

/*    DEBUG */

static	print_op (yo, level)
register YO	yo;
register int	level;
{
    if (yo == NULLYO)
	return;

    fprintf (stderr, "%*sname=%s opcode=%d\n", level * 4, "",
	    yo -> yo_name, yo -> yo_opcode);

    if (yo -> yo_arg) {
	fprintf (stderr, "%*sargument\n", level * 4, "");
	print_type (yo -> yo_arg, level + 1);
    }
    if (yo -> yo_result) {
	fprintf (stderr, "%*sresult\n", level * 4, "");
	print_type (yo -> yo_result, level + 1);
    }
    if (yo -> yo_errors) {
	fprintf (stderr, "%*serrors\n", level * 4, "");
	print_value (yo -> yo_errors, level + 1);
    }
}

/*  */

static	print_err (ye, level)
register YE	ye;
register int	level;
{
    if (ye == NULLYE)
	return;

    fprintf (stderr, "%*sname=%s opcode=%d\n", level * 4, "",
	    ye -> ye_name, ye -> ye_errcode);

    if (ye -> ye_param) {
	fprintf (stderr, "%*sparameter\n", level * 4, "");
	print_type (ye -> ye_param, level + 1);
    }
}

/*  */

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
	    fprintf (stderr, "%*sstring=0x%x\n", level * 4, "",
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

static SY  new_symbol (encpref, decpref, prfpref, mod, id)
register char  *encpref,
	       *decpref,
	       *prfpref,
	       *mod,
	       *id;
{
    register SY    sy;

    if ((sy = (SY) calloc (1, sizeof *sy)) == NULLSY)
	yyerror ("out of memory");
    sy -> sy_encpref = encpref;
    sy -> sy_decpref = decpref;
    sy -> sy_prfpref = prfpref;
    sy -> sy_module = mod;
    sy -> sy_name = id;

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


/*
 * we do the same as modsym except we generate a more "compress" name,
 * no underscores between components and dash is translated to only one
 * underscore to be compatiable with pepsy. Hence name Compress MODule SYMbol
 */
static char *cmodsym (module, id, prefix, realid)
register char  *module,
	       *id, *realid;
char   *prefix;
{
    char    buf1[BUFSIZ],
            buf2[BUFSIZ],
            buf3[BUFSIZ];
    register struct triple *t;
    static char buffer[BUFSIZ];

    if (module == NULLCP)
	for (t = triples; t -> t_name; t++)
	    if (strcmp (t -> t_name, realid) == 0) {
		module = "UNIV";
		break;
	    }

    if (prefix)
	cmodsym_aux (prefix, buf1);
    cmodsym_aux (module ? module : mymodule, buf2);
    cmodsym_aux (id, buf3);
    if (prefix)
	(void) sprintf (buffer, "%s%s%s", buf1, buf2, buf3);
    else
	(void) sprintf (buffer, "%s%s", buf2, buf3);

    return buffer;
}


/* like cmodsym except we put identifier (sym) then the module (mod) hence its
 * name symmod
 */
static char *csymmod (module, id, prefix)
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
	cmodsym_aux (prefix, buf1);
    cmodsym_aux (id, buf2);
    cmodsym_aux (module ? module : mymodule, buf3);
    if (prefix)
	(void) sprintf (buffer, "%s%s%s", buf1, buf2, buf3);
    else
	(void) sprintf (buffer, "%s%s", buf2, buf3);

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

static	cmodsym_aux (name, bp)
register char  *name,
	       *bp;
{
    register char   c;

    while (c = *name++)
	switch (c) {
	    case '-':
		*bp++ = '_';
		break;

	    default:
		*bp++ = c;
		break;
	}

    *bp = NULL;
}
