/* pepy.c - PE parser (yacc-based) */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/pepy/RCS/pepy.c,v 7.3 91/02/22 09:34:58 mrose Interim $";
#endif

/*
 * $Header: /f/osi/pepy/RCS/pepy.c,v 7.3 91/02/22 09:34:58 mrose Interim $
 *
 *
 * $Log:	pepy.c,v $
 * Revision 7.3  91/02/22  09:34:58  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/11/21  11:32:19  mrose
 * sun
 * 
 * Revision 7.1  90/09/07  17:35:02  mrose
 * touch-up
 * 
 * Revision 7.0  89/11/23  22:11:44  mrose
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

/*    DATA */

static char *aflag = NULL;
int	Cflag = 1;		/* pepy */
int	dflag = 0;
int	hflag = 0;
int	Pflag = 0;
int	rflag = 0;
char   *bflag = NULLCP;
static int bwidth = 1;
char   *module_actions = NULLCP;
int	pepydebug = 0;
int	doexternals = 1;
static int linepos = 0;
static int mflag = 0;
static int pflag = 0;
static int sflag = 0;

static  char *eval = NULLCP;

char   *mymodule = "";
OID	mymoduleid = NULLOID;

int yysection = YP_DECODER;
char *yyencpref = "build";
char *yydecpref = "parse";
char *yyprfpref = "print";
char *yyencdflt = "build";
char *yydecdflt = "parse";
char *yyprfdflt = "print";
static char *yyprefix;

static struct section {
    char   *s_name;
    int	    s_mode;
}	sections[] = {
    "ENCODE", YP_ENCODER,
    "DECODE", YP_DECODER,
    "PRINT", YP_PRINTER,

    NULL
};
    
char   *sysin = NULLCP;
static char sysout[BUFSIZ];

typedef struct modlist {
    char   *md_module;
    OID	   md_oid;

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


char   *gensym (), *modsym ();
MD	lookup_module ();
FILE   *open_ph_file ();
SY	new_symbol (), add_symbol ();

YP	lookup_type (), lookup_binding ();
YT	lookup_tag ();

/*    MAIN */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    register char  *cp,
		   *sp;
    register struct section *sectp;

    fprintf (stderr, "%s\n", pepyversion);

    sysout[0] = NULL;
    for (argc--, argv++; argc > 0; argc--, argv++) {
	cp = *argv;

	if (strcmp (cp, "-a") == 0) {
	    argc--, argv++;
	    if ((aflag = *argv) == NULL || *aflag == '-')
		goto usage;
	    continue;
	}
	if (strcmp (cp, "-A") == 0) {
	    yysection = YP_ENCODER | YP_DECODER | YP_PRINTER;
	    continue;
	}
	if (strcmp (cp, "-d") == 0) {
	    dflag++;
	    continue;
	}
	if (strcmp (cp, "-h") == 0) {
	    hflag++;
	    continue;
	}
	if (strcmp (cp, "-m") == 0) {
	    mflag++;
	    continue;
	}
	if (strcmp (cp, "-P") == 0) {
	    Pflag++;
	    continue;
	}
	if (strcmp (cp, "-p") == 0) {
	    pflag++;
	    continue;
	}
	if (strcmp (cp, "-o") == 0) {
	    if (sysout[0]) {
		fprintf (stderr, "too many output files\n");
		exit (1);
	    }
	    if (bflag) {
not_practical: ;
		fprintf (stderr, "-b & -o together is not practical\n");
		exit (1);
	    }
	    argc--, argv++;
	    if ((cp = *argv) == NULL || (*cp == '-' && cp[1] != NULL))
		goto usage;
	    (void) strcpy (sysout, cp);

	    continue;
	}
	if (strcmp (cp, "-r") == 0) {
	    rflag++;
	    continue;
	}
	if (strcmp (cp, "-s") == 0) {
	    sflag++;
	    continue;
	}
	if (strcmp (cp, "-S") == 0) {
	    
	    argc--, argv++;
	    if ((cp = *argv) == NULL || *cp == '-')
		goto usage;

	    for (sectp = sections; sectp -> s_name; sectp++)
		if (strcmp (sectp -> s_name, cp) == 0) {
		    yysection = sectp -> s_mode;
		    break;
		}
	    if (!sectp -> s_name) {
		fprintf (stderr, "unknown section name \"%s\"\n", cp);
		exit (1);
	    }
	    continue;
	}
	if (strcmp (cp, "-b") == 0) {
	    if (bflag) {
		fprintf (stderr, "too many prefixes\n");
		exit (1);
	    }
	    if (sysout[0])
		goto not_practical;
	    argc--, argv++;
	    if ((bflag = *argv) == NULL || *bflag == '-')
		goto usage;
	    continue;
	}

	if (sysin) {
usage: ;
	    fprintf (stderr,
		"usage: pepy [-d] [-h] [-p] [-o module.c] [-r] [-s] [-S section] [-b prefix] module.py\n");
	    exit (1);
	}

	if (*cp == '-') {
	    if (*++cp != NULL)
		goto usage;
	    sysin = "";
	}
	sysin = cp;

	if (sysout[0] || bflag)
	    continue;
	if (sp = rindex (cp, '/'))
	    sp++;
	if (sp == NULL || *sp == NULL)
	    sp = cp;
	sp += strlen (cp = sp) - 3;
	if (sp > cp && strcmp (sp, ".py") == 0)
	    (void) sprintf (sysout, "%.*s.c", sp - cp, cp);
	else
	    (void) sprintf (sysout, "%s.c", cp);
    }

    switch (pepydebug = (cp = getenv ("PEPYTEST")) && *cp ? atoi (cp) : 0) {
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

    if (pflag)
	exit (pp ());

    if (strcmp (sysout, "-") == 0)
	sysout[0] = NULL;
    if (!bflag && *sysout && freopen (sysout, "w", stdout) == NULL) {
	fprintf (stderr, "unable to write "), perror (sysout);
	exit (1);
    }

    if (!bflag)
	prologue ();

    initoidtbl ();

    exit (yyparse ());		/* NOTREACHED */
}

static prologue ()
{
    char *cp;

    if (cp = index (pepyversion, ')'))
	for (cp++; *cp != ' '; cp++)
	    if (*cp == NULL) {
		cp = NULL;
		break;
	    }
    if (cp == NULL)
	cp = pepyversion + strlen (pepyversion);
    printf ("/* automatically generated by %*.*s, do not edit! */\n\n",
	    cp - pepyversion, cp - pepyversion, pepyversion);
    printf ("#include %s\n\n", mflag ? "\"psap.h\"" : "<isode/psap.h>");
    if (!bflag)
	printf ("static char *pepyid = \"%s\";\n\n", pepyversion);
    if (aflag)
	printf ("#define\tadvise\t%s\n\n", aflag);
    printf ("void\tadvise ();\n");
}
/*    ERRORS */

yyerror (s)
register char   *s;
{
    yyerror_aux (s);

    if (*sysout)
	(void) unlink (sysout);

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

yyerror_aux (s)
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
pyyerror (va_alist)
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

    exit (1);
}
#else
/* VARARGS */
pyyerror (yp, fmt)
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

yyprint (s, f, top)
char    *s;
int	f;
int	top;
{
    int	    len;
    static int didf = 0;
    static int nameoutput = 0;
    static int outputlinelen = 79;

    if (sflag)
	return;

    if (f && didf == 0) {
	if (linepos)
	    fprintf (stderr, "\n\n");

	fprintf (stderr, "%s:", mymodule);
	linepos = (nameoutput = strlen (mymodule) + 1) + 1;

	didf = 1;	
    }

    if (!nameoutput || top) {
	if (linepos)
	    fprintf (stderr, "\n\n");

	fprintf (stderr, "%s", mymodule);
	nameoutput = (linepos = strlen (mymodule)) + 1;
	    
#define	section(flag,prefix) \
	if (yysection & (flag)) { \
	    fprintf (stderr, " %s", (prefix)); \
	    linepos += strlen (prefix) + 1; \
	} \
	else \
	    fprintf (stderr, " none"), linepos += 5
	section (YP_ENCODER, yyencpref);
	section (YP_DECODER, yydecpref);
	section (YP_PRINTER, yyprfpref);

	fprintf (stderr, ":");
	linepos += 2;

	if (top)
	    return;
    }

    len = strlen (s) + (f ? 2 : 0);
    if (linepos != nameoutput)
	if (len + linepos + 1 > outputlinelen)
	    fprintf (stderr, "\n%*s", linepos = nameoutput, "");
	else
	    fprintf (stderr, " "), linepos++;
    fprintf (stderr, f ? "(%s)" : "%s", s);
    linepos += len;
}

/*    PASS1 */

pass1 ()
{
    if (!bflag)
	prologue3 ();
}

static prologue3 ()
{
    printf ("\n/* Generated from module %s", mymodule);
    if (mymoduleid)
	printf (", Object Identifier %s", sprintoid (mymoduleid));
    printf (" */\n");
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
    register SY	    sy;
    register YP	    yp;

    if (!sflag)
	(void) fflush (stderr);

    if (bflag) {
	register int    i,
			j;

	i = 2, j = 10;
	for (sy = mysymbols; sy; sy = sy -> sy_next)
	    if (!(sy -> sy_type -> yp_flags & YP_IMPORTED))
		if (++i >= j)
		    bwidth++, j *= 10;
    }
    else
	prologue2 ();

    if (strcmp (mymodule, "UNIV"))
	(void) lookup_module ("UNIV", NULLOID);

    for (sy = mysymbols; sy; sy = sy -> sy_next) {
	eval = sy -> sy_name;
	yp = sy -> sy_type;
	if (sy -> sy_module == NULLCP)
	    yyerror ("no module name associated with symbol");
	if (yp -> yp_flags & YP_IMPORTED)
	    continue;

	if (yp -> yp_direction & YP_ENCODER) {
	    if (bflag)
		init_new_file ();
	    yyprefix = sy -> sy_encpref;
	    printf ("\n/* ARGSUSED */\n\n%sint\t%s ",
		    !doexternals && (yp -> yp_flags & YP_EXPORTED) ?
		    "static " : "",
		    modsym (sy -> sy_module,
		    sy -> sy_name, YP_ENCODER));
	    do_type (yp, 1, eval, "(*pe)");
	    printf ("\n    return OK;\n}\n");
	    if (bflag)
		end_file ();
	}
	if (yp -> yp_direction & YP_DECODER) {
	    if (bflag)
		init_new_file ();
	    yyprefix = sy -> sy_decpref;
	    printf ("\n/* ARGSUSED */\n\n%sint\t%s ",
		    !doexternals && (yp -> yp_flags & YP_EXPORTED) ?
		    "static " : "",
		    modsym (sy -> sy_module,
		    sy -> sy_name, YP_DECODER));
	    undo_type (yp, 1, eval, "pe", 0);
	    printf ("\n    return OK;\n}\n");
	    if (bflag)
		end_file ();
	}
	if (yp -> yp_direction & YP_PRINTER) {
	    if (bflag)
		init_new_file ();
	    yyprefix = sy -> sy_prfpref;
	    printf ("\n/* ARGSUSED */\n\n%sint\t%s ",
		    !doexternals && (yp -> yp_flags & YP_EXPORTED) ?
		    "static " : "",
		    modsym (sy -> sy_module,
		    sy -> sy_name, YP_PRINTER));
	    undo_type (yp, 1, eval, "pe", 1);
	    printf ("\n    return OK;\n}\n");
	    if (bflag)
		end_file ();
	}
	if (!bflag && ferror (stdout))
	    myyerror ("write error - %s", sys_errname (errno));
    }

    write_ph_file ();
}

static prologue2 ()
{
    printf("\n#ifndef PEPYPARM\n#define PEPYPARM char *\n");
    printf ("#endif /* PEPYPARM */\n"); /* keep ansi happy ... */
    printf("extern PEPYPARM NullParm;\n");
}

/*  */

struct tuple tuples[] = {
	YP_BOOL, "PE_CLASS_UNIV", "PE_FORM_PRIM", "PE_PRIM_BOOL",
		PE_CLASS_UNIV, PE_PRIM_BOOL,
	YP_INT, "PE_CLASS_UNIV", "PE_FORM_PRIM", "PE_PRIM_INT",
		PE_CLASS_UNIV, PE_PRIM_INT,
	YP_INTLIST, "PE_CLASS_UNIV", "PE_FORM_PRIM", "PE_PRIM_INT",
		PE_CLASS_UNIV, PE_PRIM_INT,
	YP_BIT, "PE_CLASS_UNIV", NULLCP, "PE_PRIM_BITS",
		PE_CLASS_UNIV, PE_PRIM_BITS,
	YP_BITLIST, "PE_CLASS_UNIV", NULLCP, "PE_PRIM_BITS",
		PE_CLASS_UNIV, PE_PRIM_BITS,
	YP_OCT, "PE_CLASS_UNIV", NULLCP, "PE_PRIM_OCTS",
		PE_CLASS_UNIV, PE_PRIM_OCTS,
	YP_NULL, "PE_CLASS_UNIV", NULLCP, "PE_PRIM_NULL",
		PE_CLASS_UNIV, PE_PRIM_NULL,
	YP_OID, "PE_CLASS_UNIV", "PE_FORM_PRIM", "PE_PRIM_OID",
		PE_CLASS_UNIV, PE_PRIM_OID,
	YP_SEQ, "PE_CLASS_UNIV", "PE_FORM_CONS", "PE_CONS_SEQ",
		PE_CLASS_UNIV, PE_CONS_SEQ,
	YP_SEQTYPE, "PE_CLASS_UNIV", "PE_FORM_CONS", "PE_CONS_SEQ",
		PE_CLASS_UNIV, PE_CONS_SEQ,
	YP_SEQLIST, "PE_CLASS_UNIV", "PE_FORM_CONS", "PE_CONS_SEQ",
		PE_CLASS_UNIV, PE_CONS_SEQ,
	YP_SET, "PE_CLASS_UNIV", "PE_FORM_CONS", "PE_CONS_SET",
		PE_CLASS_UNIV, PE_CONS_SET,
	YP_SETTYPE, "PE_CLASS_UNIV", "PE_FORM_CONS", "PE_CONS_SET",
		PE_CLASS_UNIV, PE_CONS_SET,
	YP_SETLIST, "PE_CLASS_UNIV", "PE_FORM_CONS", "PE_CONS_SET",
		PE_CLASS_UNIV, PE_CONS_SET,
	YP_ENUMLIST, "PE_CLASS_UNIV", "PE_FORM_PRIM", "PE_PRIM_ENUM",
		PE_CLASS_UNIV, PE_PRIM_ENUM,
	YP_REAL,     "PE_CLASS_UNIV", "PE_FORM_PRIM", "PE_PRIM_REAL",
		PE_CLASS_UNIV, PE_PRIM_REAL,

	YP_UNDF
};

/*    PULLUP */

choice_pullup (yp, partial)
register YP	yp;
int	partial;	/* pullup fully, or just enough? */
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
		if (partial)
		    continue;
		if ((z = lookup_type (y -> yp_module, y -> yp_identifier))
			== NULLYP
			|| z -> yp_code != YP_CHOICE)
		    continue;

		choice_pullup (z2 = copy_type (z), partial);
		break;

	    case YP_CHOICE:
		choice_pullup (z2 = copy_type (y), partial);
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

tag_pullup (yp, level, arg, whatsit)
register YP	yp;
register int	level;
char   *arg,
       *whatsit;
{
    char   *narg;
    char   *id = yp -> yp_flags & YP_ID ? yp -> yp_id : "member";

    printf ("%*s{\t/* %s TAG PULLUP */\n%*sregister PE %s;\n\n",
	    level * 4, "", whatsit, (level + 1) * 4, "", narg = gensym ());
    level++;

    printf ("%*sif ((%s = prim2set (%s)) == NULLPE) {\n",
	    level * 4, "", narg, arg);
    printf ("%*sadvise (NULLCP, \"%s %%s%s: %%s\", PEPY_ERR_BAD,\n",
	    (level + 1) * 4, "", id, whatsit);
    printf ("%*spe_error (%s -> pe_errno));\n",
	    (level + 3) * 4, "", arg);
    printf ("%*sreturn NOTOK;\n%*s}\n",
	    (level + 1) * 4, "", level * 4, "");
    printf ("%*sif (%s -> pe_cardinal != 1) {\n",
	    level * 4, "", narg);
    printf ("%*sadvise (NULLCP, \"%s %%s %s: %%d\", PEPY_ERR_TOO_MANY_TAGGED,\n",
	    (level + 1) * 4, "", id, whatsit);
    printf ("%*s%s -> pe_cardinal);\n", (level + 3) * 4, "", narg);
    printf ("%*sreturn NOTOK;\n%*s}\n",
	    (level + 1) * 4, "", level * 4, "");
    printf ("%*s%s = first_member (%s);\n%*s}\n",
	    level * 4, "", arg, narg, (level - 1) * 4, "");
}


tag_pushdown (yp, level, arg, whatsit)
register YP     yp;
register int    level;
char   *arg,
       *whatsit;
{
    char   *narg;

    printf ("%*s{\t/* %s TAG PUSHDOWN */\n%*sPE %s_z;\n",
	    level * 4, "", whatsit, (level + 1) * 4, "", narg = gensym ());
    level++;
    printf ("%*sregister PE *%s = &%s_z;\n\n", level * 4, "", narg, narg);

    printf ("%*sif ((*%s = pe_alloc (PE_CLASS_%s, PE_FORM_CONS, %d)) == NULLPE) {\n",
	    level * 4, "", narg, pe_classlist[yp -> yp_tag -> yt_class],
	    val2int (yp -> yp_tag -> yt_value));
    printf ("%*sadvise (NULLCP, \"%s: %%s\", PEPY_ERR_NOMEM);\n",
	    (level + 1) * 4, "", whatsit);
    printf ("%*sreturn NOTOK;\n%*s}\n", (level + 1) * 4, "", level * 4, "");
    printf ("%*s(*%s) -> pe_cons = %s;\n", level * 4, "", narg, arg);
    printf ("%*s%s = *%s;\n", level * 4, "", arg, narg);

    level--;
    printf ("%*s}\n", level * 4, "");
}

/*    TYPE HANDLING */

tag_type (yp)
register YP	yp;
{
    register struct tuple *t;
    register YT	    yt;
    register YP	    y;

    switch (yp -> yp_code) {
	case YP_IDEFINED:
	    if (yp -> yp_flags & YP_BOUND) {
		if ((y = lookup_binding (yp -> yp_module, yp -> yp_identifier,
			    yp -> yp_bound)) == NULLYP)
		    myyerror ("type \"%s\" isn't defined for binding",
			    yp -> yp_identifier);
		if (!(y -> yp_flags & YP_TAG))
		    myyerror ("type \"%s\" isn't tagged for binding",
			    yp -> yp_identifier);
		yp -> yp_flags |= YP_TAG;
		yp -> yp_tag = copy_tag (y -> yp_tag);
		return;
	    }

	    if (yt = lookup_tag (yp)) {
		yp -> yp_flags |= YP_TAG | YP_IMPLICIT;
		yp -> yp_tag = copy_tag (yt);
		return;
	    }
	    if (!lookup_type (yp -> yp_module, yp -> yp_identifier))
		pyyerror (yp, "don't know how to tag an undefined type");
	    break;

	default:
	    for (t = tuples; t ->t_type != YP_UNDF; t++)
		if (t -> t_type == yp -> yp_code) {
		    yp -> yp_flags |= YP_TAG | YP_IMPLICIT;
		    yp -> yp_tag = new_tag (t -> t_classnum);
		    yp -> yp_tag -> yt_value = new_value (YV_NUMBER);
		    yp -> yp_tag -> yt_value -> yv_number = t -> t_idnum;
		    return;
		}
	    break;
    }

    pyyerror (yp, "don't know how to do a set/choice member that isn't tagged or bound");
}

/*  */

YP  lookup_type (mod, id)
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

static YP  lookup_binding (mod, id, binding)
register char  *mod,
	       *id,
	       *binding;
{
    register YP	    yp,
		    z;

    if ((yp = lookup_type (mod, id)) == NULLYP)
	return NULLYP;

    if (yp -> yp_code != YP_CHOICE)
	myyerror ("type \"%s\" isn't a CHOICE type", id);
    for (z = yp -> yp_type; z; z = z -> yp_next)
	if ((z -> yp_flags & YP_ID)
		&& strcmp (z -> yp_id, binding) == 0)
	    return z;

    myyerror ("type \"%s\" doesn't bind \"%s\"", id, binding);
/* NOTREACHED */
}

/*  */

check_type (type, level, class, form, id, arg)
register char  *type,
	       *class,
	       *form,
	       *id,
	       *arg;
register int	level;
{
    int	    explicit;

    if (level == 1) {
	printf ("%*sif (explicit) {\n", level * 4, ""), level++;
	explicit = 1;
    }
    else
	explicit = 0;

    printf ("%*sif (%s -> pe_class != %s", level * 4, "", arg, class);
    if (form)
	printf ("\n%*s|| %s -> pe_form != %s\n%*s",
		(level + 2) * 4, "", arg, form, (level + 2) * 4 - 1, "");
    printf (" || %s -> pe_id != %s) {\n", arg, id);
    printf ("%*sadvise (NULLCP, \"%s bad class/form/id: %%s/%%d/0x%%x\",\n",
	    (level + 1) * 4, "", type);
    printf ("%*spe_classlist[%s -> pe_class], %s -> pe_form, %s -> pe_id);\n",
	    (level + 3) * 4, "", arg, arg, arg);
    printf ("%*sreturn NOTOK;\n%*s}\n",
	    (level + 1) * 4, "", level * 4, "");

    if (explicit) {
	level--, printf ("%*s}\n", level * 4, "");
	if (form) {
	    printf ("%*selse\n%*sif (%s -> pe_form != %s) {\n",
		    level * 4, "", (level + 1) * 4, "", arg, form);
	    printf ("%*sadvise (NULLCP, \"%s bad form: %%d\", %s -> pe_form);\n",
		    (level + 2) * 4, "", type, arg);
	    printf ("%*sreturn NOTOK;\n%*s}\n",
		    (level + 2) * 4, "", (level + 1) * 4, "");
	}
    }

    printf ("\n");
}

/*  */

int  is_any_type (yp)
register YP	yp;
{
    register    YP z;

    while (yp -> yp_code == YP_IDEFINED) {
	if (yp -> yp_flags & YP_TAG)
	    return 0;

	if (yp -> yp_module && strcmp (yp -> yp_module, mymodule))
	    (void) lookup_module (yp -> yp_module, yp -> yp_modid);

	if (z = lookup_type (yp -> yp_module, yp -> yp_identifier)) {
	    yp = z;

	    continue;
	}

	break;
    }

    return (yp -> yp_code == YP_ANY && !(yp -> yp_flags & YP_TAG));
}

int  is_nonimplicit_type (yp)
register YP	yp;
{
    register    YP z;

    while (yp -> yp_code == YP_IDEFINED) {
	if ((yp -> yp_flags & (YP_TAG | YP_IMPLICIT)) ==
	    (YP_TAG))
	    return 0;

	if (yp -> yp_module && strcmp (yp -> yp_module, mymodule))
	    (void) lookup_module (yp -> yp_module, yp -> yp_modid);

	if (z = lookup_type (yp -> yp_module, yp -> yp_identifier)) {
	    yp = z;

	    continue;
	}

	break;
    }

    if (yp -> yp_code == YP_CHOICE || yp -> yp_code == YP_ANY) {
	if ((yp -> yp_flags & (YP_TAG | YP_IMPLICIT)) ==
	    YP_TAG)
	    return 0;
	return 1;
    }
    return 0;
}

/*  */

uniqint (yv)
register YV	yv;
{
    register int    i;
    register YV	    y;

    for (; yv; yv = yv -> yv_next) {
	i = val2int (yv);

	for (y = yv -> yv_next; y; y = y -> yv_next)
	    if (i == val2int (y)) {
		warning ("non-unique values in list");
		fprintf (stderr, "\tvalue=%d", i);
		if (yv -> yv_flags & YV_NAMED)
		    fprintf (stderr, " name1=%s", yv -> yv_named);
		if (y -> yv_flags & YV_NAMED)
		    fprintf (stderr, " name2=%s", y -> yv_named);
		fprintf (stderr, "\n");
	    }
    }
}

/*  */

uniqtag (y, z)
register YP	y,
		z;
{
    int     i;
    register int    id;
    register    YT yt;
    register    YP yp;

    for (; y != z; y = y -> yp_next) {
	if ((yt = lookup_tag (y)) == NULLYT)
	    continue;

	id = PE_ID (yt -> yt_class, i = val2int (yt -> yt_value));

	for (yp = y -> yp_next; yp != z; yp = yp -> yp_next) {
	    if ((yt = lookup_tag (yp)) == NULLYT)
		continue;

	    if (id == PE_ID (yt -> yt_class, val2int (yt -> yt_value))) {
		warning ("non-unique tags in list");
		fprintf (stderr, "\ttag=%s/%d", pe_classlist[yt -> yt_class],
			i);
		if (y -> yp_code == YP_IDEFINED)
		    fprintf (stderr, " id1=%s", y -> yp_identifier);
		if (yp -> yp_code == YP_IDEFINED)
		    fprintf (stderr, " id2=%s", yp -> yp_identifier);
		fprintf (stderr, "\n");
	    }
	}
    }
}

/*  */

int  val2int (yv)
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

	default:
	    myyerror ("unknown value: %d", yv -> yv_code);
    }
/* NOTREACHED */
}

/*    PH FILES */

/* really need much more information in the .ph file... */

static	read_ph_file (module, oid)
register char *module;
OID	oid;
{
    int     class,
	    value,
	    direction;
    char    buffer[BUFSIZ],
	    file[BUFSIZ],
	    id[BUFSIZ],
    	    encpref[BUFSIZ],
    	    decpref[BUFSIZ],
    	    printpref[BUFSIZ];
    char    *p, *ep, *dp, *ppp;
    register FILE  *fp;
    register YP	    yp;
    register YT	    yt;
    register YV	    yv;

    (void) sprintf (file, "%s.ph", module);
    if (oid)
	(void) sprintf (p = buffer, "%s.ph", sprintoid(oid));
    else
	p = NULLCP;
    if ((fp = open_ph_file (file, p, "r")) == NULL)
    {
	warning ("Can't find file %s%s%s failed\n",
		     file, p ? "/" : "", p ? p : "");
	return;
    }

    if (strcmp (module, "UNIV"))
	yyprint (module, 1, 0);

    while (fgets (buffer, sizeof buffer, fp)) {
	if (sscanf (buffer, "%d/%d/%d: %s",
		    &class, &value, &direction, id) !=4) {
	    myyerror ("bad external definition in %s: %s",
		    file, buffer);
	    continue;
	}
	ppp = dp = ep = NULLCP;
	if (p = index(buffer, '|')) {
	    if( sscanf (p+1, "%s %s %s\n", encpref, decpref, printpref) == 3) {
		ppp = new_string (printpref);
		dp = new_string (decpref);
		ep = new_string (encpref);
	    }
	}
		
	yp = new_type (YP_ANY);
	yp -> yp_flags = YP_IMPORTED;
	if (class >= 0) {
	    yp -> yp_flags |= YP_TAG;
	    yp -> yp_tag = yt = new_tag ((PElementClass) class);
	    yt -> yt_value = yv = new_value (YV_NUMBER);
	    yv -> yv_number = value;
	}
	yp -> yp_direction = direction;
	pass1_type (ep, dp, ppp, new_string (module),
		new_string (id), yp);
    }

    (void) fclose (fp);
}

/*  */

static	write_ph_file () {
    int	    msave;
    char    file[BUFSIZ];
    char    fileoid[BUFSIZ];
    char	*cp;
    register FILE  *fp;
    register SY	    sy;
    register YT	    yt;
    register YP	    yp;

    (void) sprintf (file, "%s.ph", mymodule);
    if (mymoduleid)
	(void) sprintf (cp = fileoid, "%s.ph", sprintoid(mymoduleid));
    else
	cp = NULLCP;
    msave = mflag, mflag = 0;
    if ((fp = open_ph_file (file, cp, "w")) == NULL)
	myyerror ("unable to write %s", file);
    mflag = msave;

    for (sy = mysymbols; sy; sy = sy -> sy_next) {
	yp = sy -> sy_type;
	if (yp -> yp_flags & YP_IMPORTED)
	    continue;
	if (doexternals == 0 && (yp->yp_flags & YP_EXPORTED) == 0)
	    continue;

	if (is_any_type (yp)) {
	    fprintf (fp, "-1/0/%d: %s", yp -> yp_direction, sy -> sy_name);
	    fprintf (fp, " |%s %s %s\n", yyencpref, yydecpref, yyprfpref);
	}
	else
	    if ((yt = lookup_tag (yp)) && yt -> yt_class != PE_CLASS_CONT) {
		fprintf (fp, "%d/%d/%d: %s", yt -> yt_class,
			    val2int (yt -> yt_value), yp -> yp_direction,
			    sy -> sy_name);
		fprintf (fp, " |%s %s %s\n", yyencpref, yydecpref, yyprfpref);
	    }
    }

    (void) fclose (fp);
}

/*  */

#ifndef	PEPYPATH
#define	PEPYPATH	""
#endif


static FILE *open_ph_file (fn, fnoid, mode)
char *fn,
     *fnoid,
     *mode;
{
    register char  *dst,
		   *path;
    char    fnb[BUFSIZ];
    register FILE  *fp;
    static char *pepypath = NULL;

    if (*fn == '/')
	return fopen (fn, mode);

    if (mflag) {	/* MOBY HACK */
	if (fnoid && (fp = fopen (fnoid, mode)) != NULL)
	    return fp;
	if ((fp = fopen (fn, mode)) != NULL)
	    return fp;

	if (fnoid) {
	    (void) sprintf (fnb, "../pepy/%s", fnoid);
	    if ((fp = fopen (fnb, mode)) != NULL)
		return fp;
	}
	(void) sprintf (fnb, "../pepy/%s", fn);
	if ((fp = fopen (fnb, mode)) != NULL)
	    return fp;

	if (fnoid) {
	    (void) sprintf (fnb, "../../pepy/%s", fnoid);
	    if ((fp = fopen (fnb, mode)) != NULL)
		return fp;
	}
	(void) sprintf (fnb, "../../pepy/%s", fn);
	return fopen (fnb, mode);
    }

    if (pepypath == NULL && (pepypath = getenv ("PEPYPATH")) == NULL)
	pepypath = PEPYPATH;
    path = pepypath;

    do {
	dst = fnb;
	while (*path && *path != ':')
	    *dst++ = *path++;
	if (dst != fnb)
	    *dst++ = '/';
	if (fnoid) {
	    (void) strcpy (dst, fnoid);
	    if ((fp = fopen (fnb, mode)) != NULL)
		break;
	}
	(void) strcpy (dst, fn);
	if ((fp = fopen (fnb, mode)) != NULL)
	    break;
    } while (*path++);

    return fp;
}

/*    PRETTY-PRINTING */

#define	S0	0
#define	S1	1
#define	S2	2
#define	S3	3
#define S4	4
#define S5	5
#define S6	6
#define S7	7
#define S8	8
#define S9	9

static int  pp () {
    register int    c,
		    s;
    register char  *bp,
		   *wp;
    char    buffer[BUFSIZ];

    for (s = S0, bp = buffer; (c = getchar ()) != EOF;)
	switch (s) {
	    case S0:
		if (c == '%')
		    s = S1;
		else if (c == '<')
		    s = S4;
		else if (c == '[')
		    s = S7;
		else
		    if (isspace ((u_char) c))
			*bp++ = c;
		    else {
flush: 	;
			if (bp != buffer) {
			    for (wp = buffer; wp < bp; wp++)
				putchar (*wp);
			    bp = buffer;
			}
			putchar (c);
		    }
		break;

	    case S1:
		if (c == '{') {
		    bp = buffer;
		    s = S2;
		    break;
		}
		*bp++ = '%';
		s = S0;
		goto flush;

	    case S2:
		if (c == '%')
		    s = S3;
		break;

	    case S3:
		s = c == '}' ? S0 : S2;
		break;

	    case S4:
		if ( c == '<') {
		    bp = buffer;
		    s = S5;
		    break;
		}
		*bp++ = '<';
		s = S0;
		goto flush;

	    case S5:
		if (c == '>')
		    s = S6;
		break;

	    case S6:
		s = c == '>' ? S0 : S5;
		break;

	    case S7:
		if ( c == '[') {
		    bp = buffer;
		    s = S8;
		    break;
		}
		*bp ++ = '[';
		s = S0;
		goto flush;

	    case S8:
		if (c == ']')
		    s = S9;
		break;

	    case S9:
		s = c == ']' ? S0 : S8;
		break;

	    default:
		printf ("s=%d\n", s);
		break;
	}

    if (bp != buffer)
	for (wp = buffer; wp < bp; wp++)
	    putchar (*wp);

    return 0;
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
	    "%*sintexp=\"%s\" strexp=\"%s\" prfexp=%c declexp=\"%s\" varexp=\"%s\"\n",
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

static MD  lookup_module (module, oid)
char   *module;
OID	oid;
{
    register MD	    md;

    for (md = mymodules; md; md = md -> md_next) {
	if (module && md -> md_module && strcmp (md -> md_module, module) == 0)
	    return md;
	if (oid && md -> md_oid && oid_cmp(oid, md->md_oid) == 0)
	    return md;
    }

    read_ph_file (module, oid);

    if ((md = (MD) calloc (1, sizeof *md)) == NULLMD)
	yyerror ("out of memory");
    md -> md_module = new_string (module);
    if (oid)
	md -> md_oid = oid_cpy(oid);
    else
	md -> md_oid = NULLOID;

    if (mymodules != NULLMD)
	md -> md_next = mymodules;

    return (mymodules = md);
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

    if (y == NULLYV)
	return z;

    if (z == NULLYV)
	return y;

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

/*  */

YT  lookup_tag (yp)
register YP	yp;
{
    register struct tuple *t;
    static struct ypt ypts;
    register YT	    yt = &ypts;
    static struct ypv ypvs;
    register YV	    yv = &ypvs;
    register YP	    z;

    if (yp -> yp_flags & YP_TAG)
	return yp -> yp_tag;

    while (yp -> yp_code == YP_IDEFINED) {
	if (yp -> yp_module && strcmp (yp -> yp_module, mymodule))
	    (void) lookup_module (yp -> yp_module, yp -> yp_modid);

	if (z = lookup_type (yp -> yp_module, yp -> yp_identifier)) {
	    yp = z;

	    if (yp -> yp_flags & YP_TAG)
		return yp -> yp_tag;

	    continue;
	}

	break;
    }

    for (t = tuples; t -> t_type != YP_UNDF; t++)
	if (t -> t_type == yp -> yp_code) {
	    yt -> yt_class = t -> t_classnum;
	    yt -> yt_value = yv;
	    yv -> yv_code = YV_NUMBER;
	    yv -> yv_number = t -> t_idnum;

	    return yt;
	}

    return NULLYT;
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

char *modsym (module, id, direct)
register char  *module,
	       *id;
int direct;
{
    char    buf1[BUFSIZ],
	    buf2[BUFSIZ],
	    buf3[BUFSIZ];
    char   *pref;
    register struct triple *t;
    static char buffer[BUFSIZ];

    pref = NULLCP;
    if (module == NULLCP)
	for (t = triples; t -> t_name; t++)
	    if (strcmp (t -> t_name, id) == 0) {
		module = "UNIV";
		break;
	    }

    if (module && strcmp (module, mymodule))
	switch (direct) {
	    case YP_DECODER:
		pref = yydecdflt;
		break;

	    case YP_ENCODER:
		pref = yyencdflt;
		break;

	    case YP_PRINTER:
		pref = yyprfdflt;
		break;
	}

    modsym_aux (pref ? pref : yyprefix, buf1);
    modsym_aux (module ? module : mymodule, buf2);
    modsym_aux (id, buf3);
    (void) sprintf (buffer, "%s_%s_%s", buf1, buf2, buf3);

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

char *gensym () {
    char    buffer[BUFSIZ];
    static int  i = 0;

    (void) sprintf (buffer, "p%d", i++);
    return new_string (buffer);
}

init_new_file ()
{
    static int file_no = 0;
    char	buffer[BUFSIZ];

    (void) sprintf (buffer, "%s-%.*d.c", bflag, bwidth, ++file_no);
    if (freopen (buffer, "w", stdout) == NULL) {
	fprintf (stderr, "unable to write "), perror (buffer);
	exit (1);
    }

    prologue ();
    prologue3 ();

    if (module_actions)
	fputs (module_actions, stdout);

    prologue2 ();
}

end_file ()
{
    (void) fflush (stdout);
    if (ferror (stdout))
	myyerror ("write error - %s", sys_errname (errno));
    
}
