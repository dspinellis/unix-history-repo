/* pass2.c */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/pepsy/RCS/pass2.c,v 7.10 91/02/22 09:49:18 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/pepsy/RCS/pass2.c,v 7.10 91/02/22 09:49:18 mrose Interim $
 *
 *
 * $Log:	pass2.c,v $
 * Revision 7.10  91/02/22  09:49:18  mrose
 * Interim 6.8
 * 
 * Revision 7.9  91/01/08  12:49:45  mrose
 * update
 * 
 * Revision 7.8  90/12/23  17:24:52  mrose
 * patches
 * 
 * Revision 7.7  90/12/11  10:33:43  mrose
 * sync
 * 
 * Revision 7.6  90/11/20  15:27:14  mrose
 * update
 * 
 * Revision 7.5  90/11/11  10:53:58  mrose
 * update
 * 
 * Revision 7.4  90/11/04  19:18:57  mrose
 * update
 * 
 * Revision 7.3  90/08/08  14:02:27  mrose
 * stuff
 * 
 * Revision 7.2  90/07/27  08:49:22  mrose
 * update
 * 
 * Revision 7.1  90/07/01  20:00:32  mrose
 * update
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


#include <stdio.h>
#include <ctype.h>
#include "pepsydefs.h"
#include "pass2.h"
#include "mine.h"
#include "sym.h"

extern int doexternals;
extern int sflag, mflag;

extern char *eval;

extern int Aflag;
extern SY mysymbols;
extern char *modsym();
extern char *my_strcat();
extern char *notidtoid();
extern char	*rm_indirect();
extern char	*getid();
extern char	*getidordot();
extern char	*getfield();
extern char	*sym2type();
extern YP	lookup_type();

char	*tab;	/* mymodule - with - changed to _ */

Action	start_action, final_action;
int	e_actions, d_actions, p_actions; /* number of actions of each type */

FILE   *ffopen();

peri_pass2()
{
    char *inc;	/* *_pre_defs.h file */

    if (!sflag)
	(void) fflush(stderr);

    tab = notidtoid(mymodule);

    if (strcmp(mymodule, "UNIV"))
	(void) lookup_module("UNIV", NULLOID);

    inc = my_strcat (mymodule, HFILE2);

    gen_typesfile(inc);

    gen_tablefile(inc);

}

/*
 * generate the table file which contains:
 * #include types file
 * #include - types files for other (external) modules referenced
 * definitions of data structures used: strings and stuff for defaults
 * 		declaractions of functions referenced.
 *		tpe tables - one per type assignment for each of
 *			encoding, decoding and printing
 *		tables of pointers to all the above tpe tables - one for
 *			type of tpe table
 *		pointer table - used to hold pointers as tpe table cannot
 *		   hold pointers but only integers which index into this table
 *		module definition - the key to all the data structures os this
 *			module. contains references to all the tables.
 *		lint declaractions for the "pepy" functions
 */
gen_tablefile(inc)
char	*inc;
{

    int     nentries;
    int     encflag = 1, decflag = 1, prntflag = 1;
    char   *buf, *act;
    SY      sy;
    YP      yp;
    FILE   *fphh, *fptab;

    fptab = ffopen(my_strcat(mymodule, TBLNAME));

    /* Only need two files <stdio.h> and our types file which includes
     * everything else we need
     * Assumption. types file in the same directory as the _tables
     */
    (void) fprintf(fptab, "#include <stdio.h>\n");
    (void) fprintf(fptab, "#include \"%s%s\"\n\n", mymodule, GENTYPES);



#ifdef ACT_CODE
    (void) fprintf(fptab, "#include \"%s\"\n", act);
#endif
    
    if (start_action) {
	(void) fprintf (fptab, "\n# line %d \"%s\"\n", start_action->a_line, sysin);
	fputs (start_action->a_data, fptab);
    }


    /*
     * loop through and generate all the default values definitions
     * and what ever else needs to be processed for all the entries
     * e.g. external module references and coding function declarations
     */
    for (sy = mysymbols; sy; sy = sy->sy_next) {
	yp = sy->sy_type;
	if (yp->yp_flags & YP_IMPORTED)
	    continue;
	if (yp->yp_direction & (YP_ENCODER | YP_DECODER | YP_PRINTER)
	    || Aflag) {
	    gen_dflts(fptab, sy->sy_type, sy->sy_name);
	}
    }

    (void) fprintf(fptab, "\n#define OFFSET(t,f)\t((int ) &(((t *)0)->f))\n\n");
    (void) fprintf(fptab, "\n#define AOFFSET(t,f)\t((int ) (((t *)0)->f))\n\n");
    nentries = 0;
    /*
     * generate tpe tables
     */
    for (sy = mysymbols; sy; sy = sy->sy_next) {
	yp = sy->sy_type;
	if (yp->yp_flags & YP_IMPORTED)
	    continue;
	if (yp->yp_direction & YP_ENCODER || Aflag) {
	    nentries++;
	    gen_enctbl(fptab, sy);
	}
    }
    for (sy = mysymbols; sy; sy = sy->sy_next) {
	yp = sy->sy_type;
	if (yp->yp_flags & YP_IMPORTED)
	    continue;
	if (yp->yp_direction & YP_DECODER || Aflag)
	    gen_dectbl(fptab, sy);
    }

    for (sy = mysymbols; sy; sy = sy->sy_next) {
	yp = sy->sy_type;
	if (yp->yp_flags & YP_IMPORTED)
	    continue;
	if (yp->yp_direction & YP_PRINTER || Aflag)
	    gen_prnttbl(fptab, sy);
    }

    fphh = ffopen(inc);
    (void) fprintf(fphh, "\nextern modtyp	%s%s%s;\n",
	    PREFIX, tab, MODTYP_SUFFIX);
    out_final_defs(fphh);
    close(fphh);

    gen_actfunct(fptab);

    gen_tpe(fptab);  /* generate table of pointers to tpe tables */

    gen_modtype(fptab, nentries, encflag, decflag, prntflag);

    if (final_action) {
	(void) fprintf (fptab, "\n# line %d \"%s\"\n", final_action->a_line, sysin);
	fputs (final_action->a_data, fptab);
    }

   gen_lint(fptab);

   fclose(fptab);
}

/*
 * generate the *-types.h file
 */
gen_typesfile(inc)
char	*inc;
{
    int     encflag = 1, decflag = 1, prntflag = 1;
    char   *buf, *act;
    SY      sy;
    YP      yp;
    FILE   *fph, *fpe, *fpd, *fpp;
    FILE   *fpa;

    fph = ffopen(my_strcat(mymodule, HFILE1));
    if (mflag) {
	(void) fprintf (fph, "#ifndef\tPEPYPATH\n");
	(void) fprintf (fph, "#include <isode/pepsy/%s>\n", inc);
	(void) fprintf (fph, "#else\n");
	(void) fprintf (fph, "#include \"%s\"\n", inc);
	(void) fprintf (fph, "#endif\n\n\n");
    }
    else {
	if (is_stand (inc))
	    (void) fprintf (fph, "#include <isode/pepsy/%s>\n", inc);
	else
	    (void) fprintf (fph, "#include \"%s\"\n", inc);
    }

#ifdef ACT_CODE
    act = my_strcat(mymodule, ACTIONDEFS);

    fpe = ffopen(my_strcat(mymodule, ENCFILENAME));
    file_header(fpe, act);
    fpd = ffopen(my_strcat(mymodule, DECFILENAME));
    file_header(fpd, act);
    fpp = ffopen(my_strcat(mymodule, PRNTFILENAME));
    file_header(fpp, act);
    fpa = ffopen(act);
#endif

    /* define the macros to support posy functions */

    fprintf(fph, "\n#ifndef\tlint\n");
    for (sy = mysymbols; sy; sy = sy->sy_next) {
	eval = sy->sy_name;
	yp = sy->sy_type;

	if (sy->sy_module == NULLCP)
	    yyerror("no module name associated with symbol");
	if (yp->yp_flags & YP_IMPORTED)
	    continue;
	if (strcmp(sy->sy_module, mymodule)) {
	    fprintf(stderr, "mymodule unsuitable for module name e.g %s and %s(mymodule)\n", sy->sy_module, mymodule);
	    exit(1);
	}
	if (yp->yp_direction & YP_ENCODER || Aflag) {
	    buf = modsym (sy -> sy_module, sy -> sy_name, yyencdflt);
	    fprintf(fph, "#define %s", buf);
	    fprintf(fph, "(pe, top, len, buffer, parm) \\\n");
	    fprintf(fph, "    %s(%s%s, ", ENCFNCNAME, PREFIX, proc_name(sy->sy_name, 1));
	    fprintf(fph, "&%s%s%s, ", PREFIX, tab, MODTYP_SUFFIX);
	    fprintf(fph, "pe, top, len, buffer, (char *) parm)\n\n");
#ifdef ACT_CODE
	    if (encflag) {
		fprintf(fpe, "%s%s", ENC_FNCNAME, tab);
		open_func(fpe);
		encflag--;
	    }
#endif
	    if (bflag)
		init_new_file();
	    if (bflag)
		end_file();
	}
	if (yp->yp_direction & YP_DECODER || Aflag) {
	    buf = modsym (sy -> sy_module, sy -> sy_name, yydecdflt);
	    if (bflag)
		init_new_file();
	    fprintf(fph, "#define %s", buf);
	    fprintf(fph, "(pe, top, len, buffer, parm) \\\n");
	    fprintf(fph, "    %s(%s%s, ", DECFNCNAME, PREFIX, proc_name(sy->sy_name, 1));
	    fprintf(fph, "&%s%s%s, ", PREFIX, tab, MODTYP_SUFFIX);
	    fprintf(fph, "pe, top, len, buffer, (char **) parm)\n\n");
#if ACT_CODE
	    if (decflag) {
		fprintf(fpd, "%s%s", DEC_FNCNAME, tab);
		open_func(fpd);
		decflag--;
	    }
#endif
	    if (bflag)
		end_file();
	}
	if (yp->yp_direction & YP_PRINTER || Aflag) {
	    buf = modsym (sy -> sy_module, sy -> sy_name, yyprfdflt);
	    if (bflag)
		init_new_file();
	    fprintf(fph, "#define %s", buf);
	    fprintf(fph, "(pe, top, len, buffer, parm) \\\n");
	    fprintf(fph, "    %s(%s%s, ", PRNTFNCNAME, PREFIX, proc_name(sy->sy_name, 1));
	    fprintf(fph, "&%s%s%s, ", PREFIX, tab, MODTYP_SUFFIX);
	    fprintf(fph, "pe, top, len, buffer)\n");
	    fprintf(fph, "#define %s_P", buf);
	    fprintf(fph, "    %s%s, ", PREFIX,
		    proc_name(sy->sy_name, 1));
	    fprintf(fph, "&%s%s%s\n\n", PREFIX, tab, MODTYP_SUFFIX);
#ifdef  ACT_CODE
	    if (prntflag) {
		fprintf(fpp, "%s%s", PRNT_FNCNAME, tab);
		open_func(fpp);
		prntflag--;
	    }
#endif
	    if (bflag)
		end_file();
	}
	if (!bflag && ferror(stdout))
	    myyerror("write error - %s", sys_errname(errno));
    }
    fprintf(fph, "\n#endif   /* lint */\n");

#ifdef	ACT_CODE
    if (!encflag) {
	close_func(fpe);
    }
    if (!decflag) {
	close_func(fpd);
    }
    if (!prntflag) {
	close_func(fpp);
    }
#endif

    close(fph);
#ifdef	ACT_CODE
    close(fpe);
    close(fpd);
    close(fpp);
    close(fpa);
#endif
}


gen_enctbl(fp, sy)
FILE	*fp;
SY      sy;
{
    YP	yp;

    yp = sy->sy_type;
    fprintf(fp,"static tpe %s%s[] = {\n", ETABLE, proc_name(sy->sy_name, 0));
    fprintf(fp, "\t{ PE_START, 0, 0, 0 },\n");

    tenc_typ(fp, yp, sy->sy_name, NULLCP);

    fprintf(fp, "\t{ PE_END, 0, 0, 0 }\n");
    fprintf(fp, "\t};\n");
    fprintf(fp, "\n");
}

gen_dectbl(fp, sy)
FILE	*fp;
SY      sy;
{
    YP	yp;

    yp = sy->sy_type;
    fprintf(fp,"static tpe %s%s[] = {\n", DTABLE, proc_name(sy->sy_name, 0));
    fprintf(fp, "\t{ PE_START, 0, 0, 0 },\n");

    tdec_typ(fp, sy->sy_type, sy->sy_name, NULLCP);

    fprintf(fp, "\t{ PE_END, 0, 0, 0 }\n");
    fprintf(fp, "\t};\n");
    fprintf(fp, "\n");
}


gen_prnttbl(fp, sy)
FILE	*fp;
SY      sy;
{
    YP	yp;

    yp = sy->sy_type;

    fprintf(fp,"static ptpe %s%s[] = {\n",PTABLE, proc_name(sy->sy_name, 0));
    fprintf(fp, "\t{ PE_START, 0, 0, 0, \"%s\" },\n", sy->sy_name);

    tprnt_typ(fp, sy->sy_type, sy->sy_name, NULLCP);

    fprintf(fp, "\t{ PE_END, 0, 0, 0, NULL }\n");
    fprintf(fp, "\t};\n");
    fprintf(fp, "\n");
}


/*
 * define the tpe index tables and the pointer table
 */
gen_tpe(fp)
FILE	*fp;
{
    SY      sy;

    fprintf(fp, "static tpe *etabl[] = {\n");
    for (sy = mysymbols; sy; sy = sy->sy_next) {
	if (sy->sy_type->yp_flags & YP_IMPORTED)
	    continue;
	if (sy->sy_type->yp_direction & YP_ENCODER || Aflag)
	    fprintf(fp, "\t%s%s,\n", ETABLE, proc_name(sy->sy_name, 0));
    }
    fprintf(fp, "\t};\n\n");

    fprintf(fp, "static tpe *dtabl[] = {\n");
    for (sy = mysymbols; sy; sy = sy->sy_next) {
	if (sy->sy_type->yp_flags & YP_IMPORTED)
	    continue;
	if (sy->sy_type->yp_direction & YP_DECODER || Aflag)
	    fprintf(fp, "\t%s%s,\n", DTABLE, proc_name(sy->sy_name, 0));
    }
    fprintf(fp, "\t};\n\n");

    fprintf(fp, "static ptpe *ptabl[] = {\n");
    for (sy = mysymbols; sy; sy = sy->sy_next) {
	if (sy->sy_type->yp_flags & YP_IMPORTED)
	    continue;
	if (sy->sy_type->yp_direction & YP_PRINTER || Aflag)
	    fprintf(fp, "\t%s%s,\n", PTABLE, proc_name(sy->sy_name, 0));
    }
    fprintf(fp, "\t};\n\n");

    /* produce pointer table */
    dump_ptrtab(fp);
}

/*
 * output the module structure for this module
 */
gen_modtype(fp, no, f1, f2, f3)
FILE	*fp;
int     no;
int     f1, f2, f3;
{
    if (!f1)
	fprintf(fp, "extern PE\t%s%s();\n", ENC_FNCNAME, tab);
    if (!f2)
	fprintf(fp, "extern PE\t%s%s();\n", DEC_FNCNAME, tab);
    if (!f3)
	fprintf(fp, "extern PE\t%s%s();\n", PRNT_FNCNAME, tab);
    fprintf(fp, "\n");
    fprintf(fp, "modtyp %s%s%s = {\n", PREFIX, tab, MODTYP_SUFFIX);
    fprintf(fp, "\t\"%s\",\n", mymodule);	/* name */
    fprintf(fp, "\t%d,\n", no);		/* number of entries */
					/* coding tables */
    fprintf(fp, "\tetabl,\n");
    fprintf(fp, "\tdtabl,\n");
    fprintf(fp, "\tptabl,\n");
					/* action tables */
    if (e_actions > 0)
	fprintf(fp, "\tefn_%s,\n", tab);
    else
	fprintf(fp, "\t0,\n");	/* no action code */

    if (d_actions > 0)
	fprintf(fp, "\tdfn_%s,\n", tab);
    else
	fprintf(fp, "\t0,\n");	/* no action code */

    if (p_actions > 0)
	fprintf(fp, "\tpfn_%s,\n", tab);
    else
	fprintf(fp, "\t0,\n");	/* no action code */

    fprintf(fp, "\t%s%s%s,\n", PREFIX, PTR_TABNAME, tab);
    fprintf(fp, "\t};\n\n");
}

/*
 * open a file called name
 */
FILE   *
ffopen(name)
char   *name;
{
    FILE   *fp;

    if ((fp = fopen(name, "w")) == NULL) {
	fprintf(stderr, "Can't create the file %s", name);
	exit(1);
    }
    return fp;
}

#ifdef ACT_CODE
/*
 * output the file prologue to the file specified by fp
 */
file_header(fp, act)
FILE   *fp;
char   *act;
{
    fprintf(fp, "#include %s\n", PSAP_DOT_H);
    fprintf(fp, "#include \"%s\"\n", INCFILE1);
    fprintf(fp, "#include \"%s\"\n", act);
    fprintf(fp, "#include \"%s%s\"\n\n", mymodule, GENTYPES);
    fprintf(fp, "#ifndef PEPYPARM\n");
    fprintf(fp, "#define PEPYPARM char *\n");
    fprintf(fp, "#endif\n");
    fprintf(fp, "extern PEPYPARM NullParm;\n\n");
}

#endif

/*
 * output the function prologue to the file specified by fp
 */
open_func(fp)
FILE   *fp;
{

    fprintf(fp, "(pe, parm, p, mod)\n");
    fprintf(fp, "PE\tpe;\n");
    fprintf(fp, "PEPYPARM\tparm;\n");
    fprintf(fp, "tpe\t*p;\n");
    fprintf(fp, "modtyp\t*mod;\n");
    fprintf(fp, "{\n");
    /* action 0 ???? */
    fprintf(fp, "\tswitch (p->pe_ucode) {\n");
}

/*
 * output the function epilogue to the file specified by fp
 */
close_func(fp)
FILE   *fp;
{
    fprintf(fp, "\t\tdefault:\n");
    fprintf(fp, "\t\t\tbreak;\n");
    fprintf(fp, "\t}\n");
    fprintf(fp, "\treturn OK;\n}\n\n");
}

/*
 * print the table id_table
 */
print_table()
{
    int     i;
    id_entry *t;

    for (i = 0; i < TABLESIZE; i++) {
	for (t = id_table[i]; t != NULL; t = t->next)
	    (void) printf("%s(%d) -->   ", t->r_value, t->def_value);
	if (id_table[i] != NULL)
	    (void) printf("NULL -- %d\n", i);
    }
}
static struct univ_typ univ_tab[] = {
    {"EXTERNAL", "struct	type_UNIV_EXTERNAL	*", "EXTERNAL", 8, 0,
    "UNIV", UNF_EXTMOD|UNF_HASDATA, YP_SEQLIST, },
    {"GeneralString", "struct	qbuf	*", "OCTETSTRING", 27, 0,
    "UNIV", 0, YP_OCT, },
    {"GeneralisedTime", "struct	qbuf	*", "OCTETSTRING", 24, 0,
    "UNIV", 0, YP_OCT, },
    {"GeneralizedTime", "struct	qbuf	*", "OCTETSTRING", 24, 0,
    "UNIV", 0, YP_OCT, },
    {"GraphicString", "struct	qbuf	*", "OCTETSTRING", 25, 0,
    "UNIV", 0, YP_OCT, },
    {"IA5String", "struct	qbuf	*", "OCTETSTRING", 22, 0,
    "UNIV", 0, YP_OCT, },
    {"ISO646String", "struct	qbuf	*", "OCTETSTRING", 26, 0,
    "UNIV", 0, YP_OCT, },
    {"NumericString", "struct	qbuf	*", "OCTETSTRING", 18, 0,
    "UNIV", 0, YP_OCT, },
    {"PrintableString", "struct	qbuf	*", "OCTETSTRING", 19, 0,
    "UNIV", 0, YP_OCT, },
    {"TeletexString", "struct	qbuf	*", "OCTETSTRING", 20, 0,
    "UNIV", 0, YP_OCT, },
    {"T61String", "struct	qbuf	*", "OCTETSTRING", 20, 0,
    "UNIV", 0, YP_OCT, },
    {"UTCTime", "struct	qbuf	*", "OCTETSTRING", 23, 0,
    "UNIV", 0, YP_OCT, },
    {"UniversalTime", "struct	qbuf	*", "OCTETSTRING", 23, 0,
    "UNIV", 0, YP_OCT, },
    {"VideotexString", "struct	qbuf	*", "OCTETSTRING", 21, 0,
    "UNIV", 0, YP_OCT, },
    {"VisibleString", "struct	qbuf	*", "OCTETSTRING", 26, 0,
    "UNIV", 0, YP_OCT, },
};

extern struct univ_typ *simptyp();

/*
 * Determine wether the type name matches one of the Universal types
 * which are to be treated specially. If so return a pointer to the
 * data structure which contains the parameters describing how it
 * should be processed
 */
struct univ_typ *
univtyp(name)
char   *name;
{
    int     low, high, i;
    struct univ_typ *p;

    low = 0;
    high = NENTRIES(univ_tab) - 1;
    while (low <= high) {
	p = univ_tab + (low + high) / 2;
	if ((i = scmp(name, p->univ_name)) == 0)
	    return (p);
	if (low == high)
	    return (NULL);
	if (i < 0)
	    high = p - univ_tab - 1;
	else
	    low = p - univ_tab + 1;
    }

#if OPTIMISED
    if ((p = simptyp(name)) == NULL)
	return (p);
#endif

    return (NULL);
}

/*
 * Compare two strings returning a number representing the character
 * where they differ or 0 if are the same - I wrote this because I
 * couldn't trust strcmp to work the same way between numbers and
 * letters everywhere. longer strings are greater shorter strings
 * numbers are greater then all letters lower case are greater then
 * upper case There must be a better way !
 */
scmp(s1, s2)
char   *s1, *s2;
{
    while (*s1 == *s2 && *s2)
	s1++, s2++;
    if (*s1 == '\0' && *s2 == '\0')
	return (0);
    if (*s1 == '\0')
	return (-1);
    if (*s2 == '\0')
	return (1);
    if (isalpha(*s1) && isalpha(*s2)) {
	if (isupper(*s1) && isupper(*s2))
	    return (*s1 - *s2);
	if (islower(*s1) && islower(*s1))
	    return (*s1 - *s2);
	if (isupper(*s1))
	    return (-1);
	if (islower(*s1))
	    return (1);
    }
    if (isdigit(*s1) && isdigit(*s2))
	return (*s1 - *s2);
    if (isdigit(*s1))
	return (1);
    if (isdigit(*s2))
	return (-1);
    return (*s1 - *s2);
}

/*
 * lookup a symbol and return a pointer to it
 */
SY
syfind(name)
char   *name;
{
    SY      sy;

    for (sy = mysymbols; sy; sy = sy->sy_next) {
	if (sy->sy_type->yp_flags & YP_IMPORTED)
	    continue;
	if (strcmp(name, sy->sy_name) == 0)
	    return (sy);
    }
    return (NULL);
}
/*
 * determine if the symbol is a simple type that is optimised
 */
struct univ_typ *
simptyp(yp)
YP      yp;
{
    struct univ_typ *p;
    YP      y;

    static struct univ_typ bitstring =
    {"Bitstring", "struct	PElement	*", "BITSTRING", 3, 0},
            octetstring =
    {"GeneralString", "struct	qbuf	*", "OCTETSTRING", 4, 0},
            oid =
    {"Object Identifier", "struct	OIDentifier	*", "OBJIDENT", 6, 0},
            obj =
    {"Module", "struct	OIDentifier	*",
    "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", 27, 0};

#define MSTRING 30		/* number of Xs in the above string */

    switch (yp->yp_code) {
    case YP_BIT:
    case YP_BITLIST:
	return (&bitstring);

    case YP_SEQ:
    case YP_SET:
    case YP_ANY:
	return (&bitstring);

    case YP_OCT:
	return (&octetstring);

    case YP_OID:
	return (&oid);

    case YP_IDEFINED:
	strncpy(p->univ_tab, yp->yp_identifier, MSTRING);
	return (&obj);


    case YP_SEQLIST:
    case YP_SETLIST:
    case YP_CHOICE:

    default:
	return (NULL);
    }
}

/*
 * lookup a type name until you get something that is not a name
 */
YP
lkup(yp)
YP	yp;
{
    YP	yp1;

    if (yp == NULLYP)
	return (yp);
    
    while (yp->yp_code == YP_IDEFINED) {
        if ((yp1 = lookup_type(yp->yp_module, yp->yp_identifier)) == NULLYP)
	    return (yp);
	yp = yp1;
    }

    return (yp);
}
/*
 * compute the type of tag it should be given the tag and the type it is
 * being applied to
 */
comptag(tag, yp)
int	tag;
YP	yp;
{
    static int	warned = 0;
    YP	yp1;
    struct univ_typ	*p;
    int	code;

    if (tag == 0)
	return (0);
    
    yp1 = lkup(yp);

    if (yp1->yp_code == YP_IDEFINED) {
	if ((p = univtyp(yp1->yp_identifier)) == NULL
	  || p->univ_type <= YP_UNDF) {
		switch (chkil(yp1->yp_identifier)) {
		case ER_NORMAL:
		    return (tag);

		case ER_EXPLICIT:
		    return (0);

		default:
		    break;
		}
		    
	    if (warned++ <= 3) {
		ferrs(0, "\ncomptag:warning implicit tag of unknown type %s\n",
		    yp1->yp_identifier);
		ferr(0, "\tcomptag:treated as implicit\n");
	    }
	    return (tag);
	}
	code = p->univ_type;
    } else {
	code = yp1->yp_code;
	if (code == YP_ANY && yp1->yp_flags & YP_WANTIMPLICIT)
	    return (tag);
    }

    if (code == YP_CHOICE || code == YP_ANY)
	return (0);

    return (tag);
}

/*
 * Generate function definitions for all the macros so that lint
 * can type check all thier uses
 */
gen_lint(fp)
FILE	*fp;
{
    char   *buf; 
    SY      sy;
    YP      yp;

    fprintf(fp, "\n#ifdef\tlint\n");
    for (sy = mysymbols; sy; sy = sy->sy_next) {
	eval = sy->sy_name;
	yp = sy->sy_type;

	if (sy->sy_module == NULLCP)
	    yyerror("no module name associated with symbol");
	if (yp->yp_flags & YP_IMPORTED)
	    continue;
	if (strcmp(sy->sy_module, mymodule)) {
	    fprintf(stderr,
	      "mymodule unsuitable for module name e.g %s and %s(mymodule)\n",
	      sy->sy_module, mymodule);
	    exit(1);
	}
	/* Encoding routine */
	buf = modsym (sy -> sy_module, sy -> sy_name, yyencdflt);
	fprintf(fp, "\n#undef %s\n", buf);
	fprintf(fp, "int	%s", buf);
	fprintf(fp, "(pe, top, len, buffer, parm)\n");
	fprintf(fp, "PE     *pe;\n");
	fprintf(fp, "int\ttop,\n\tlen;\n");
	fprintf(fp, "char   *buffer;\n");
	fprintf(fp, "%s *parm;\n", sym2type(sy));
	fprintf(fp, "{\n  return (%s(%s%s, ",
	    ENCFNCNAME, PREFIX, proc_name(sy->sy_name, 1));
	fprintf(fp, "&%s%s%s, ", PREFIX, tab, MODTYP_SUFFIX);
	fprintf(fp, "pe, top, len, buffer,\n\t\t(char *) parm));\n}\n");

	/* Decoding routine */
	buf = modsym (sy -> sy_module, sy -> sy_name, yydecdflt);
	fprintf(fp, "\n#undef %s\n", buf);
	fprintf(fp, "int	%s", buf);
	fprintf(fp, "(pe, top, len, buffer, parm)\n");
	fprintf(fp, "PE\tpe;\n");
	fprintf(fp, "int\ttop,\n       *len;\n");
	fprintf(fp, "char  **buffer;\n");
	fprintf(fp, "%s **parm;\n", sym2type(sy));
	fprintf(fp, "{\n  return (%s(%s%s, ",
	    DECFNCNAME, PREFIX, proc_name(sy->sy_name, 1));
	fprintf(fp, "&%s%s%s, ", PREFIX, tab, MODTYP_SUFFIX);
	fprintf(fp, "pe, top, len, buffer,\n\t\t(char **) parm));\n}\n");

	/* Printing routine */
	buf = modsym (sy -> sy_module, sy -> sy_name, yyprfdflt);
	fprintf(fp, "\n#undef %s\n/* ARGSUSED */\n", buf);
	fprintf(fp, "int	%s", buf);
	fprintf(fp, "(pe, top, len, buffer, parm)\n");
	fprintf(fp, "PE\tpe;\n");
	fprintf(fp, "int\ttop,\n       *len;\n");
	fprintf(fp, "char  **buffer;\n");
	fprintf(fp, "%s *parm;\n", sym2type(sy));
	fprintf(fp, "{\n  return (%s(%s%s, ",
	    PRNTFNCNAME, PREFIX, proc_name(sy->sy_name, 1));
	fprintf(fp, "&%s%s%s, ", PREFIX, tab, MODTYP_SUFFIX);
	fprintf(fp, "pe, top, len, buffer));\n}\n");

	if (ferror(fp))
	    myyerror("write error - %s", sys_errname(errno));
    }
    fprintf(fp, "\n#endif\t/* lint */\n");
}

/*
 * pointer table support routines
 */
static char	*ptr_tab[MAXPTRS];	/* reference of the pointer */
static int	ptr_cnt = 0;

/*
 * add the given pointer to the pointer table and return its index
 */
addptr(p)
char	*p;
{
    int	ind;
    register int i;
    register char *s;

    /*
     * try to eliminate common pointers by returning a ponter if it matches
     * previously
     */
    for (i = 0; i < ptr_cnt; i++)
	if (strcmp(p, ptr_tab[i]) == 0)
	     return (i);

    if (ptr_cnt >= MAXPTRS) {
	fprintf(stderr, "\npointer table overflow %d\n", ptr_cnt);
	exit(1);
    }

    if ((s = malloc ((unsigned) (strlen (p) + 1))) == NULLCP) {
	fprintf(stderr, "\naddptr:out of memory\n");
	exit(1);
    }
    (void) strcpy (s, p);

    ptr_tab[ind = ptr_cnt++] = s;

    return (ind);
}


dump_ptrtab(fp)
FILE	*fp;
{
    int		i;


    fprintf(fp, "\n/* Pointer table %d entries */\n", ptr_cnt);
    fprintf(fp, "static caddr_t %s%s%s[] = {\n", PREFIX, PTR_TABNAME, tab);

    for (i = 0; i < ptr_cnt; i++)
	fprintf(fp, "    (caddr_t ) %s,\n", ptr_tab[i]);
    
    if (ptr_cnt <= 0)
	fprintf(fp, "    (caddr_t ) 0,\n");	/* for fussy C compilers */

    fprintf(fp, "};\n");
}

/*
 * routines to parse and analyse C types to support the pepy-like extensions
 * of pepsy
 */

/*
 * remove a level of indirection from the given type. If possible. if not
 * return NULLCP, otherwise return the new type in a temporary buffer
 */
char	*
rm_indirect(p)
char	*p;
{
    static char	buf[STRSIZE];
    int		i;

    if (p == NULLCP || *p == '\0' || (i = strlen(p)) >= STRSIZE)
	return (NULLCP);

    strncpy(buf, p, STRSIZE);

    for (; i >= 0; i--) {
	if (buf[i] == '*') {
	    buf[i] = '\0';
	    return (buf);
	}
    }

    return (NULLCP);

}

/*
 * extract the field from the C arguement and the following constant
 * expression for the bit number. 
 * if it fails return NULLCP
 */
char	*
getfldbit(p, pstr)
register char *p;
char **pstr;
{
    static char buf[STRSIZE];

    if (p == NULLCP || pstr == (char **)0)
	return (NULLCP);

    if ((p = getidordot(p, buf, STRSIZE)) == NULLCP)
	return (NULLCP);

    while (*p && isspace(*p))
	p++;
    
    if (*p != '$') {	/* must be a -> */

	if (strncmp(p, "->", 2) != 0)
	    return (NULLCP);
	
	p += 2;

	if ((p = getidordot(p, buf, STRSIZE)) == NULLCP)
	    return (NULLCP);

	while (*p && isspace(*p))
	    p++;
	
	if (*p != '$')
	    return (NULLCP);
    }

    *pstr = p + 1;	/* have to leave it up to the compiler to verify the
			 * constant expression for the bit number
			 */
    return (buf);
}

/* return a pointer after the current batch of white space if any */
char	*
skipspace(p)
char	*p;
{
    if (p == NULLCP)
	 return (NULLCP);

    while (*p && isspace(*p))
	p++;

    return (p);
}

/*
 * extract the field from the C arguement and return it in a static buffer
 * else return NULLCP
 */
char	*
getfield(p)
register char *p;
{
    static char buf[STRSIZE];
    char	*buf1;

    if (p == NULLCP)
	return (NULLCP);

    while (*p && isspace(*p))
	p++;
    
    if (*p == '*')	/* to support *parm field */
	return (p);

    if ((p = getidordot(p, buf, STRSIZE)) == NULLCP)
	return (NULLCP);

    while (*p && isspace(*p))
	p++;
    
    if (*p == '\0')
	return (buf);

    if (strncmp(p, "->", 2) != 0)
	return (NULLCP);
    
    p += 2;

    /* if we have an & keep it on the field */
    if (*buf == '&')
	buf1 = buf + 1;
    else
	buf1 = buf;

    if ((p = getidordot(p, buf1, STRSIZE)) == NULLCP)
	return (NULLCP);

    while (*p && isspace(*p))
	p++;
    
    if (*p == '\0')
	return (buf);

    return (NULLCP);
}

/*
 * get an identifier into the given buffer [A-Za-z_] are legal chars
 */
char	*
getid(p, buf, len)
register char	*p;
register char	*buf;
register int	len;
{
    char	*fbuf;

    fbuf = buf;

    while (*p && isspace(*p))
	p++;
    
    while (*p && (isalnum(*p) || *p == '_')) {
	if (len-- >= 0)
	    *buf++ = *p;
	p++;
    }

    if (fbuf == buf)
	return (NULLCP);

    *buf = '\0';

    return (p);

}

/*
 * get an identifier into the given buffer - '.' are considered part of an 
 * identifier - should really be called get field reference
 */
char	*
getidordot(p, buf, len)
register char	*p;
register char	*buf;
register int	len;
{
    char	*fbuf;

    fbuf = buf;

    while (*p && isspace(*p))
	p++;
    
    if (*p == '&') {
	len--;
	*buf++ = *p++;

	while (*p && isspace(*p))
	    p++;
    }

    while (*p && (isalnum(*p) || *p == '_' || *p == '.')) {
	if (len-- >= 0)
	    *buf++ = *p;
	p++;
    }

    if (fbuf == buf)
	return (NULLCP);

    *buf = '\0';

    return (p);

}
static char *noindstr[] = {
	"*", "*parm", "&", "&parm",
	NULLCP
	};

/*
 * determine if the given field means no indirection wanted and so return 1
 * else return 0
 */
noindirect(f)
char	*f;
{
    char *p, **ps;
    int		l;

    if (f == NULLCP)
	return (1);

    f = skipspace(f);

    if (f == NULLCP)
	return (1);

    if (*f == '&')
	return (1);

    for (p = f; *p && !isspace(*p); p++)
	;
    l = p - f;

    for (ps = noindstr; *ps; ps++)
	if (l == strlen(*ps) && strncmp(f, *ps, l) == 0)
	    return (1);
    
    return (0);
}

/*
 * process the T - the Type and field specifier
 * the given YP to the appropriate values for VALTYPE string
 * Namely the yp_parm_type to contain the (Parameter) type string.
 * then if there is a $ the yp_parm to the part after the $, the field
 */
setvaltype(yp, str)
YP	yp;
char	*str;
{
    char *p;

    if (str == NULLCP || *(str = skipspace(str)) == '\0')
       return (0);
    
    if (p = index(str, '$')) {
	*p++ = '\0';
	p = skipspace(p);
	yp->yp_parm = strdup(p);
	yp->yp_flags |= YP_PARMVAL;
    }

    yp->yp_param_type = strdup(str);

    return (1);

}


/*
 * generate the functions that carry out the action statements
 */
gen_actfunct(fp)
FILE	*fp;
{
    SY	sy;
    YP	yp;
    Action	act;

    if (e_actions > 0) {
	(void) fprintf(fp, "\n/*VARARGS*/");
	(void) fprintf(fp, "\nstatic\tint\nefn_%s(__p, ppe, _Zp)\ncaddr_t	__p;\n", tab);
	(void) fprintf(fp, "PE	*ppe;\ntpe	*_Zp;\n{\n");
	(void) fprintf(fp, "\t\t\n\t/* %d cases */\n    switch(_Zp->pe_ucode) {\n",
	    e_actions);
	for (sy = mysymbols; sy; sy = sy->sy_next) {
	    yp = sy->sy_type;
	    if (yp->yp_flags & YP_IMPORTED)
		continue;
	    
	    if (yp->yp_direction & YP_ENCODER || Aflag)
		gen_actions(fp, yp, G_ENC);

	}
	(void) fprintf(fp,
"    default:\n        return (pepsylose(NULLMODTYP, _Zp, *ppe, \"enf_%s:Bad table entry: %%d\",\n             _Zp->pe_ucode));\n",
		tab);
	(void) fprintf(fp, "\t\t}\t/* switch */\n    return (OK);\n}\n");
    }
    if (d_actions > 0) {
	(void) fprintf(fp, "\n/*VARARGS*/");
	(void) fprintf(fp, "\nstatic\tint\ndfn_%s(__p, pe, _Zp, _val)\ncaddr_t	__p;\n", tab);
	(void) fprintf(fp, "PE	pe;\ntpe	*_Zp;\nint _val;\n{\n");
	(void) fprintf(fp, "\t\t\n\t/* %d cases */\n    switch(_Zp->pe_ucode) {\n",
	    d_actions);
	for (sy = mysymbols; sy; sy = sy->sy_next) {
	    yp = sy->sy_type;
	    if (yp->yp_flags & YP_IMPORTED)
		continue;
	    
	     if (yp->yp_direction & YP_DECODER || Aflag)
		 gen_actions(fp, yp, G_DEC);

	}
	(void) fprintf(fp,
"    default:\n        return (pepsylose(NULLMODTYP, _Zp, pe, \"dnf_%s:Bad table entry: %%d\",\n            _Zp->pe_ucode));\n",
		tab);
	(void) fprintf(fp, "\t\t}\t/* switch */\n    return (OK);\n}\n");
    }
    if (p_actions > 0) {
	(void) fprintf(fp, "\n/*VARARGS*/");
	(void) fprintf(fp, "\nstatic\tint\npfn_%s(pe, _Zp)\n", tab);
	(void) fprintf(fp, "PE	pe;\ntpe	*_Zp;\n{\n");
	(void) fprintf(fp, "\t\t\n\t/* %d cases */\n    switch(_Zp->pe_ucode) {\n",
	    p_actions);
	for (sy = mysymbols; sy; sy = sy->sy_next) {
	    yp = sy->sy_type;

	    if (yp->yp_flags & YP_IMPORTED)
		continue;
	    
	    if (yp->yp_direction & YP_PRINTER || Aflag)
		gen_actions(fp, yp, G_PNT);

	}
	(void) fprintf(fp,
"    default:\n        return (pepsylose(NULLMODTYP, _Zp, NULLPE, \"pnf_%s:Bad table entry: %%d\",\n            _Zp->pe_ucode));\n",
		tab);
	(void) fprintf(fp, "\t\t}\t/* switch */\n    return (OK);\n}\n");
    }
}

#define GEN_RETURN 1
#define GEN_ASSIGN 2
/*
 * generate the actions for this YP unit and all its children
 */
gen_actions(fp, oyp, form)
FILE	*fp;
YP	oyp;
int	form;	/* what type of action is it */
{
    register YP	yp;
    register YAL	yal;
    register Action	act;

    for (yp = oyp; yp; yp = yp->yp_next) {

	/* do its actions first then any of its children */
	if (yal = yp->yp_bef_alist)
	    dumpact(fp, yal, form, 0);
	if (yal = yp->yp_aft_alist)
	    dumpact(fp, yal, form, 0);
	if (yal = yp -> yp_control_act) {
	    if (form == G_ENC)
		dumpact (fp, yal,form, GEN_RETURN);
	    else if (form == G_DEC)
		dumpact (fp, yal, form, GEN_ASSIGN);
	}
	if (yal = yp -> yp_optional_act) {
	    if (form == G_ENC)
		dumpact (fp, yal,form, GEN_RETURN);
	    else if (form == G_DEC)
		dumpact (fp, yal, form, 0);
	}

	switch (yp->yp_code) {
	case YP_SEQTYPE:
	case YP_SEQLIST:
	case YP_SETTYPE:
	case YP_SETLIST:
	case YP_CHOICE:
	    gen_actions(fp, yp->yp_type, form);
	    break;

	default:
	   break;
	}
    }
}
/*
 * dump out a single action
 */
dumpact(fp, yal, form, ret)
FILE	*fp;
YAL	yal;
int	form;
int	ret;
{
    char	*comm = yal->yal_comment;
    char	*type = yal->yal_type;
    Action	act;
    char	buf[STRSIZE];

    if (strlen(type) > STRSIZE)
	ferr(1, "dumpact:type too big \"%s\"\n bye\n", type);
    strncpy(buf, type, STRSIZE);
    if (form != G_DEC)
	strncat(buf, "*", STRSIZE);
    else
	strncat(buf, "**", STRSIZE);
    type = buf;
    switch (form) {
    case G_ENC:
	act = yal->yal_enc;
	break;
    case G_DEC:
	act = yal->yal_dec;
	break;
    case G_PNT:
	act = yal->yal_prn;
	break;
    }

    /* can't tell wether this table has the UCODE till here */
    if (act == NULLAction || act -> a_line == -1)
	return;

    if (form != G_PNT)
	(void) fprintf(fp, "\n#define parm	((%s )__p)\n\tcase %d: /* %s */\n",
		    type, act->a_num, comm ? comm : "");
    else 
	(void) fprintf(fp, "\n\tcase %d: /* %s */\n", act->a_num, comm ? comm : "");

    (void) fprintf(fp, "\t\t{\n# line %d \"%s\"\n", act->a_line, sysin);

    switch (ret) {
    case GEN_ASSIGN:
	if (control_act (act) == -1)
	    fprintf (fp, "\t\t/* ignored - empty expression */\n");
	else fprintf (fp, "\t\t(%s) = _val;\n", act -> a_data);
	break;
    case GEN_RETURN:
	fprintf (fp, "\t\treturn (%s);\n",act -> a_data);
	break;
    default:
	fputs (act -> a_data, fp);
	putc (';', fp);
	break;
    }

    (void) fprintf(fp, "\n\t\t}\n\t    break;\n");

    if (form != G_PNT)
	(void) fprintf(fp, "\n#undef parm\n");
    act -> a_line = -1;		/* mark as done */
}

/*
 * produce a temporary copy of the type specified in the ParameterType
 * specification
 * i.e. a [[ P type ]] specification
 */
char	*
partyp2str(yp)
YP	yp;
{
    char *p;

    if (yp->yp_param_type == NULLCP) {
	fprintf(stderr, "\npartyp2str no param_type field\n");
	exit(1);
    }

    if ((p = rm_indirect(yp->yp_param_type)) == NULLCP) {
	fprintf(stderr, "\npartyp2str can't extract direct type from %s\n",
	    yp->yp_param_type);
	exit(1);
    }

    return (p);
}
/*
 * produce a string giving the type of a symbol, in a static buffer
 */
char	*
sym2type(sy)
SY	sy;
{
    static char buffer[STRSIZE];

    if (sy->sy_type && sy->sy_type->yp_param_type)
	return (partyp2str(sy->sy_type));

    sprintf(buffer, "struct %s", modsym(sy->sy_module, sy->sy_name, "type"));

    return (buffer);
}
