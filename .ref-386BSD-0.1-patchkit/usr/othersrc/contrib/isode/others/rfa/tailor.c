/*
 * RFA - Remote File Access
 *
 * Access and Management for a partial file system tree that exists
 * at two sites either as master files or slave files
 *
 * tailor.c - read rfatailor and .rfarc file
 *
 * Contributed by Oliver Wenzel, GMD Berlin, 1990
 *
 * $Header: /f/osi/others/rfa/RCS/tailor.c,v 7.3 91/02/22 09:28:37 mrose Interim $
 *
 * $Log:	tailor.c,v $
 * Revision 7.3  91/02/22  09:28:37  mrose
 * Interim 6.8
 * 
 * Revision 7.2  91/01/14  13:55:13  mrose
 * update
 * 
 * Revision 1.1  91/01/04  16:08:47  ow
 * Initial revision
 * 
 */

#ifndef       lint
static char *rcsid = "$Header: /f/osi/others/rfa/RCS/tailor.c,v 7.3 91/02/22 09:28:37 mrose Interim $";
#endif

/*
 *                              NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#include <stdio.h>
#include <ctype.h>
#include <strings.h>
#include "logger.h"
#include "rfa.h"
#include "rfainfo.h"

extern char *fsBase;
extern char *user, *host, *passwd, *strdup();
extern LLog *pgm_log;

int default_transfer = RI_TR_REQ;
int doChown = 0;
int doChgrp = 1;
int doChmod = 1;
int doClearSUID = 1;
int doRmWidows = 0;
int doRfaExec = 1;
int timeSlave = 0;
int compLimit = COMPRESSLIMIT;
char *passwd = "rfa";
char *user = "rfa";
char *host = "localhost";
int backup = 0;

#define VERROR(v)	{ sprintf(errp,"\n\tinvalid tailor value '%s'",(v));\
			  errp += strlen(errp); \
			  err++; }

char *cut(s)
    char *s;
{
    register char *s1;
 
    for (; *s && isspace(*s); s++)
        ;
    for (; *s && (*s == '"'); s++)
        ;
    for (s1 = s + strlen(s) - 1; (s1 != s) && isspace(*s1); s1--)
        ;
    for (; *s1 && (*s1 == '"'); s1--)
        ;
    if (*s1)
        *(s1+1) = '\0';
    return s;
}      



int tailor(fn)
    char *fn;
{
    FILE *f;
    char buf[BUFSIZ];
    char *errp, *v, *o, *index();
    int err = 0;

    if ((f = fopen(fn, "r")) == NULL)  
	return OK;

    errp = rfaErrStr;
    while (fgets(buf, BUFSIZ, f)) {
	for (v = buf; isspace(*v); v++)
	    ;
	if (*v == '\0')
	    continue;
	if (*buf == '#')
	    continue;
	if ((v = index(buf,'=')) == NULL) {
	    sprintf(rfaErrStr, "\n\tinvalid tailor string '%s'", cut(buf));
	    fclose(f);
	    return NOTOK_LOCAL_ERROR;
    	}
	*v = '\0';
	v = cut(v+1);
	o = cut(buf);

	/*--- USER ---*/
	if (!strcasecmp(o, "user")) {
	    user = strdup(v);
	} else 

	/*--- PASSWORD ---*/
	if (!strcasecmp(o, "password")) {
	    passwd = strdup(v);
	} else

	/*--- HOST ---*/
	if (!strcasecmp(o, "host")) {
	    host = strdup(v);
	} else 

	/*--- ROOT ---*/
	if (!strcasecmp(o, "root")) {
	    char buf[BUFSIZ];

	    if (realpath(v, buf, sizeof buf) == NULL) {
		sprintf(errp,"\n\tinvalid local root '%s'", cut(v));
		errp += strlen(errp);
		err++;
	    }
	    fsBase = strdup(buf);
	} else 

	/*--- COMPRESS ---*/
	if (!strcasecmp(o, "compress")) {
	    if((compLimit = atoi(v)) == 0)
		VERROR(v);
	} else 

	/*--- BACKUP ---*/
	if (!strcasecmp(o, "backup")) {

	    if (!strcasecmp(v, "on"))
		backup = 1;
	    else 
		if (!strcasecmp(v, "off"))
		    backup = 0;
		else
		    VERROR(v);
	} else 

	/*--- CHGRP ---*/
	if (!strcasecmp(o, "chgrp")) {

	    if (!strcasecmp(v, "on"))
		doChgrp = 1;
	    else 
		if (!strcasecmp(v, "off"))
		    doChgrp = 0;
		else
		    VERROR(v);
	} else 

	/*--- TIME ---*/
	if (!strcasecmp(o, "time")) {

	    if (!strcasecmp(v, "slave"))
		timeSlave = 1;
	    else 
		if (!strcasecmp(v, "master"))
		    timeSlave = 0;
		else
		    VERROR(v);
	} else 

	/*--- CHOWN ---*/
	if (!strcasecmp(o, "chown")) {
	    if (!strcasecmp(v, "on")) {
		if (geteuid() != 0) {
		    sprintf(errp,
			"\n\tWARNING: must run as root for <chown> option");
		    errp += strlen(errp);
		    doChown = 0;
		} else
		    doChown = 1;
	    } else 
		if (!strcasecmp(v, "off"))
		    doChown = 0;
		else
		    VERROR(v);
	} else 

	/*--- CHMOD ---*/
	if (!strcasecmp(o, "chmod")) {
	    if (!strcasecmp(v, "on"))
		doChmod = 1;
	    else 
		if (!strcasecmp(v, "off"))
		    doChmod = 0;
		else
		    VERROR(v);
	} else 
	
	/*--- STRIP SUID ---*/
	if (!strcasecmp(o, "clearsuid")) {
	    if (!strcasecmp(v, "on"))
		doClearSUID = 1;
	    else 
		if (!strcasecmp(v, "off"))
		    doClearSUID = 0;
		else
		    VERROR(v);
	} else 

	/*--- REMOVE SLAVES ---*/
	if (!strcasecmp(o, "rmslaves")) {
	    if (!strcasecmp(v, "on"))
		doRmWidows = 1;
	    else 
		if (!strcasecmp(v, "off"))
		    doRmWidows = 0;
		else
		    VERROR(v);
	} else 

	/*--- LOGDEBUG ---*/
	if (!strcasecmp(o, "debuglog"))  {
	    if (!strcasecmp(v, "on"))
		pgm_log->ll_events |= LLOG_DEBUG | LLOG_TRACE | LLOG_NOTICE;
	    else 
		if (!strcasecmp(v, "off"))
		    pgm_log->ll_events = LLOG_EXCEPTIONS | LLOG_FATAL;
		else
		    VERROR(v);

	} else 

	/*--- RFA EXEC ---*/
	if (!strcasecmp(o, "rfaexec"))  {
	    if (!strcasecmp(v, "on"))
		doRfaExec = 1;
	    else 
		if (!strcasecmp(v, "off"))
		    doRfaExec = 0;
		else
		    VERROR(v);
	} else 

	/*--- TRANSFER ---*/
	if (!strcasecmp(o, "transfer"))  {
	    if (!strcasecmp(v, "request"))
		default_transfer = RI_TR_REQ;
	    else 
		if (!strcasecmp(v, "auto"))
		    default_transfer = RI_TR_AUTO;
		else
		    VERROR(v);

	} else  {
	    sprintf(errp, "\n\tinvalid tailor option '%s'",o);
	    errp += strlen(errp);
	    err++;
	}
    }
    fclose(f);
    if (err)
	return NOTOK;
    return OK;
}

