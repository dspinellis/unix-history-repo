/* audio.c - play a sound */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/audio.c,v 7.3 91/02/22 09:18:33 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/common/RCS/audio.c,v 7.3 91/02/22 09:18:33 mrose Interim $
 *
 *
 * $Log:	audio.c,v $
 * Revision 7.3  91/02/22  09:18:33  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/11/20  15:29:13  mrose
 * cjr
 * 
 * Revision 7.1  90/10/17  11:41:14  mrose
 * sync
 * 
 * Revision 7.0  90/08/08  08:35:36  mrose
 * *** empty log message ***
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


/* LINTLIBRARY */

#include <signal.h>
#include "quipu/util.h"
#include "quipu/attr.h"
#include "psap.h"
#include <errno.h>
#ifdef	BSD42
#include <sys/wait.h>
#endif

#ifndef BINPATH
#define BINPATH "/usr/local/bin/"
#endif

/* Assumes the 'play' routine from Mike (bauer@cns.ucalgary.ca) is
   installed in BINPATH directory.
*/

/* Use the {FILE} mechanism to pull in the sound files */

extern r_octprint();
extern PE r_octenc();
extern struct qbuf * r_octsdec();
extern struct qbuf * r_octparse();
extern struct qbuf * qb_cpy();
extern qb_cmp();
extern int file_attr_length;

audio_print (ps,qb,format)
PS ps;
struct qbuf * qb;
int format;
{
int     pd[2];
char    buffer [LINESIZE];
char	execvector [LINESIZE];
struct qbuf   *p;

int	pid;
int	childpid;
#ifndef	BSD42
int	status;
#else
union wait status;
#endif

SFP	pstat;


	if (format != READOUT) {
		for (p = qb -> qb_forw; p != qb; p = p -> qb_forw)
			(void) ps_write (ps,(PElementData)p->qb_data,p->qb_len);
		return;
	}

#ifndef	sparc
	ps_print (ps, "(No display process defined)");
#else
	if (pipe(pd) == -1) {
		ps_print (ps,"ERROR: could not create pipe");
		return;
	}

	pstat = signal (SIGPIPE, SIG_IGN);

	switch (childpid = fork()) {

	case -1:
		(void) close (pd[1]);
		(void) close (pd[0]);
		(void) signal (SIGPIPE, pstat);
		ps_print (ps,"ERROR: could not fork");
		return;

	case 0:
		(void) signal (SIGPIPE, pstat);

		if (dup2(pd[0], 0) == -1)
			_exit (-1);
		(void) close (pd[0]);
		(void) close (pd[1]);

		(void) sprintf (execvector,"%splay",BINPATH);

		(void) execl (execvector,execvector,NULLCP);

		while (read (0, buffer, sizeof buffer) > 0)
		    continue;
		(void) printf ("ERROR: can't execute '%s'",execvector);

		(void) fflush (stdout);
		/* safety catch */
		_exit (-1);
		/* NOTREACHED */

	default:
		(void) close (pd[0]);
		for (p = qb -> qb_forw; p != qb; p = p -> qb_forw) {
			if (write (pd[1],p->qb_data,p->qb_len) != p->qb_len) {
				(void) close (pd[1]);
				(void) signal (SIGPIPE, pstat);
				ps_print (ps,"ERROR: write error");
				return;
			}
		}
		(void) close (pd[1]);
		ps_printf (ps,"%splay invoked",BINPATH);

		while ((pid = wait (&status)) != NOTOK && childpid != pid)
			continue;

		(void) signal (SIGPIPE, pstat);
		
		return;
	} 
#endif
}

static struct qbuf *audio_parse (str)
char   *str;
{
    if (file_attr_length)
	    return str2qb (str, file_attr_length, 1);
    else
	    return str2qb (str, strlen (str), 1);
}

audio_syntax ()
{
    (void) add_attribute_syntax ("audio",
				 (IFP)r_octenc,		(IFP) r_octsdec,
				 (IFP)audio_parse,	audio_print,
				 (IFP)qb_cpy,		qb_cmp,
				 qb_free,		NULLCP,
				 NULLIFP,		TRUE);

}
