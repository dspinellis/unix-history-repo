/* miscellany.c - fred miscellaneous functions */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/uips/fred/RCS/miscellany.c,v 7.18 91/02/22 09:30:52 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/quipu/uips/fred/RCS/miscellany.c,v 7.18 91/02/22 09:30:52 mrose Interim $
 *
 *
 * $Log:	miscellany.c,v $
 * Revision 7.18  91/02/22  09:30:52  mrose
 * Interim 6.8
 * 
 * Revision 7.17  91/02/12  18:25:36  mrose
 * update
 * 
 * Revision 7.16  91/01/07  12:43:24  mrose
 * update
 * 
 * Revision 7.15  90/12/17  22:14:50  mrose
 * DA-edit
 * 
 * Revision 7.14  90/10/29  11:50:15  mrose
 * more stuff
 * 
 * Revision 7.13  90/10/29  08:10:11  mrose
 * touch-up
 * 
 * Revision 7.12  90/10/28  23:21:08  mrose
 * server
 * 
 * Revision 7.11  90/09/13  17:46:03  mrose
 * thisis-botched
 * 
 * Revision 7.10  90/08/29  15:06:37  mrose
 * update
 * 
 * Revision 7.9  90/07/27  08:45:28  mrose
 * update
 * 
 * Revision 7.8  90/06/11  10:55:27  mrose
 * UFN
 * 
 * Revision 7.7  90/05/12  17:03:22  mrose
 * sync
 * 
 * Revision 7.6  90/02/19  13:10:39  mrose
 * update
 * 
 * Revision 7.5  90/01/16  21:22:42  mrose
 * one more time
 * 
 * Revision 7.4  90/01/16  20:43:35  mrose
 * last check-out
 * 
 * Revision 7.3  90/01/11  18:36:37  mrose
 * real-sync
 * 
 * Revision 7.2  89/12/01  10:45:09  mrose
 * touch-up
 * 
 * Revision 7.1  89/11/27  10:32:24  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:09:01  mrose
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
#include "fred.h"

/*    DATA */

int	area_quantum = 0;

struct area_guide areas[] = {
    W_ORGANIZATION,
	"organization",	"-singlelevel",	"organization",		"o",	NULL,

    W_UNIT,
	"unit",		"-subtree",	"organizationalUnit",	"ou",	NULL,

    W_LOCALITY,
	"locality",	"-singlelevel", "locality",		"l",	NULL,

    W_PERSON,
	"person",	"-subtree",	"person",		"cn",	NULL,

    W_DSA,
	"dsa",		"-singlelevel",	"dsa",			"cn",	NULL,

    W_ROLE,
	"role",		"-subtree",	"organizationalRole",	"cn",	NULL,

    NULL
};

/*    ALIAS */

/* ARGSUSED */

int	f_alias (vec)
char  **vec;
{
    char   *cp,
	    buffer[BUFSIZ];

    if ((cp = *++vec) == NULL)
	return dish ("squid -fred -sequence default", 0);

    if (strcmp (cp, "-help") == 0) {
	fprintf (stdfp, "alias [name]\n");
	fprintf (stdfp, "    with no arguments, reports on active aliases\n");
	fprintf (stdfp,
		 "    with one argument, defines an alias for the given name\n");

	return OK;
    }

    (void) sprintf (buffer, "squid -fred -alias \"%s\"", cp);
    return dish (buffer, runcom);
}

/*    AREA */

int	f_area (vec)
char  **vec;
{
    int	    status;
    char   *cp,
	   *dp,
	    buffer[BUFSIZ];
    register struct area_guide *ag;

    if ((cp = *++vec) == NULL) {
	if (myarea == NULL) {
	    if (dish ("moveto -pwd", 0) == NOTOK)
		return NOTOK;
	}
	else
	    fprintf (stdfp, "                     default area %s\n", myarea);

	for (ag = areas; ag -> ag_record; ag++)
	    if (ag -> ag_area)
		fprintf (stdfp, "area for record-type %-12.12s %s\n",
			 ag -> ag_key, ag -> ag_area);

	return OK;
    }

    if (strcmp (cp, "-help") == 0) {
	fprintf (stdfp, "area [[record] location]\n");
	fprintf (stdfp,
		 "    with no arguments, lists areas current defined for various searches\n");
	fprintf (stdfp,
		 "    with one argument, sets the default area for general searches\n");
	fprintf (stdfp,
		 "    with two arguments, sets the default area for the given record type\n");

	return OK;
    }

    if ((dp = *++vec) == NULL) {
	(void) sprintf (buffer, "moveto -pwd \"%s\"", cp);
	if (dish (buffer, 1) == NOTOK) {
	    advise (NULLCP, "bad area: \"%s\"", cp);
	    return NOTOK;
	}
	if (!runcom)
	    fprintf (stdfp, "%s\n", myarea);

	area_quantum++;
	return OK;
    }

    for (ag = areas; ag -> ag_record; ag++)
	if (strcmp (ag -> ag_key, cp) == 0)
	    break;
    if (!ag -> ag_record) {
	advise (NULLCP, "invalid record-type: \"%s\"", cp);
	return NOTOK;
    }

    if (cp = myarea)
	myarea = NULL;

    (void) sprintf (buffer, "moveto -pwd \"%s\"", dp);
    if ((status = dish (buffer, 1)) == OK) {
	if (ag -> ag_area)
	    free (ag -> ag_area);
	ag -> ag_area = myarea;

	if (!runcom)
	    fprintf (stdfp, "area for record-type %s: %s\n",
		     ag -> ag_key, ag -> ag_area);
    }
    else {
	advise (NULLCP, "bad area: \"%s\"", dp);
	if (myarea)
	    free (myarea), myarea = NULL;
    }

    if (myarea = cp) {
	(void) sprintf (buffer, "moveto -pwd \"%s\"", myarea);
	(void) dish (buffer, 1);
    }

    area_quantum++;
    return status;
}

/*    DISH */

int	f_dish (vec)
char  **vec;
{
    register char *bp,
		  *cp;
    char    buffer[BUFSIZ];

    if ((cp = *++vec) == NULL)
	return dish ("squid -fred", 0);
    if (strcmp (cp, "-help") == 0) {
	fprintf (stdfp, "dish [command [arguments ...]]\n");
	fprintf (stdfp, "    with no arguments, reports on status of dish\n");
	fprintf (stdfp, "    with arguments, passes those directly to dish\n");

	return OK;
    }

    (void) strcpy (bp = buffer, cp);
    bp += strlen (bp);

    while (cp = *++vec) {
	(void) sprintf (bp, " \"%s\"", cp);
	bp += strlen (bp);
    }

    return dish (buffer, runcom);
}

/*    EDIT */

int	f_edit (vec)
char  **vec;
{
    int	    result;
    char    buffer[BUFSIZ];

    if (*++vec != NULL && strcmp (*vec, "-help") == 0) {
	fprintf (stdfp, "edit\n");
	fprintf (stdfp, "    edit entry in the white pages\n");

	return OK;
    }

    if (mydn == NULL) {
	advise (NULLCP, "who are you?  use the \"thisis\" command first...");
	return NOTOK;
    }

    (void) sprintf (buffer, "modify -nocache -dontusecopy -newdraft \"%s\"",
		    mydn);
    dontpage = 1;
    result = dish (buffer, 0);
    dontpage = 0;

    if (result != OK)
	return result;

    (void) sprintf (buffer, "showentry \"%s\" -fred -nocache -dontusecopy",
		    mydn);
    (void) dish (buffer, 0);
    return OK;
}

/*    MANUAL */

int	f_manual (vec)
char  **vec;
{
    char   buffer[BUFSIZ];
    FILE  *fp;

    if (*++vec != NULL && strcmp (*vec, "-help") == 0) {
	fprintf (stdfp, "manual\n");
	fprintf (stdfp, "    print detailed information\n");

	return OK;
    }

    (void) strcpy (buffer, isodefile ("fred.0", 0));
    if (fp = fopen (buffer, "r")) {
	while (fgets (buffer, sizeof buffer, fp))
	    paginate (stdfp, buffer, strlen (buffer));
	paginate (stdfp, NULLCP, 0);

	(void) fclose (fp);
    }
    else
	advise (buffer, "unable to open");

    return OK;
}

/*    REPORT */

int	f_report (vec)
char  **vec;
{
    register char   *bp;
    char   *cp,
	    buffer[BUFSIZ];

    if (*++vec != NULL && strcmp (*vec, "-help") == 0) {
	fprintf (stdfp, "report [subject]\n");
	fprintf (stdfp, "    send report to white pages manager\n");

	return OK;
    }

    if (readonly)
	(void) strcpy (buffer, _isodefile (isodebinpath, "mhmail"));
    bp = buffer;
    cp = strcmp (manager, "internal") ? manager : "wpp-manager@psi.com";

    if (!readonly || access (buffer, 0x01) == NOTOK) {
	(void) strcpy (bp, "/usr/ucb/Mail ");
	bp += strlen (bp);

	if (debug) {
	    (void) sprintf (bp, "-v ");
	    bp += strlen (bp);
	}

	if (readonly) {
	    (void) sprintf (bp, "-r \"%s\" ", cp);
	    bp += strlen (bp);
	}

	(void) sprintf (bp, "-s");
    }
    else {
	bp += strlen (bp);

	(void) sprintf (bp, " -subject");
    }
    bp += strlen (bp);

    (void) sprintf (bp, " \"%s\" \"%s\"",
		    *vec ? *vec : "White Pages report", cp);
    bp += strlen (bp);
			

    fprintf (stdfp, "End report with CTRL-D%s.\n",
	readonly ? ", it will then take 30 seconds to post message" : "");
    if (readonly)
	fprintf (stdfp,
		 "In the message please specify your e-mail address, so a reply may be made.\n");
    (void) fflush (stdfp);

    if (watch) {
	fprintf (stderr, "%s\n", buffer);
	(void) fflush (stderr);
    }
    if (system (buffer))
	advise (NULLCP, "problem sending report");

    return OK;
}

/*    THISIS */

int	f_thisis (vec)
char  **vec;
{
    register char   *bp;
    char   *cp,
	    buffer[BUFSIZ];

again: ;
    if ((cp = *++vec) == NULL) {
	if (mydn == NULL) {
	    advise (NULLCP, "who are you?");
	    return NOTOK;
	}

	printf ("you are \"%s\"\n", mydn);
	return OK;
    }

    if (strcmp (cp, "-help") == 0) {
	fprintf (stdfp, "thisis [name]\n");
	fprintf (stdfp,
		 "    with no arguments, lists your name in the white pages\n");
	fprintf (stdfp,
		 "    with one argument, identifies you in the white pages\n");

	return OK;
    }

    if (strcmp (cp, "is") == 0)
	goto again;

    if (*cp == '!')
	cp++;
    for (bp = cp; isdigit (*bp); bp++)
	continue;
    if (*bp && (index (cp, '@') == NULL || index (cp, '=') == NULL)) {
	advise (NULLCP,
	"expecting a distinguished name (if you don't know what this is, punt)"
		);
	return NOTOK;
    }

    bp = buffer;

    (void) sprintf (bp, "bind -simple -user \"%s\"", cp);
    bp += strlen (bp);

    if (*++vec) {
	if (runcom && (rcmode & 077))
	    adios (NULLCP,
		   "incorrect mode for runcom file -- use \"chmod 0600 $HOME/.fredrc\"");

	(void) sprintf (bp, " -password \"%s\"", *vec);
	bp += strlen (bp);
    }

    if (dish (buffer, 0) != OK) {
	(void) f_quit (NULLVP);
	exit (1);	/* NOT REACHED */
    }

    if (runcom)
	didbind = 1;

    (void) dish ("squid -user", 1);

    return OK;
}
