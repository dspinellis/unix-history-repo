/* edit.c - */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/dish/RCS/edit.c,v 7.2 91/02/22 09:40:31 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/quipu/dish/RCS/edit.c,v 7.2 91/02/22 09:40:31 mrose Interim $
 *
 *
 * $Log:	edit.c,v $
 * Revision 7.2  91/02/22  09:40:31  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/09  14:47:08  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:20:04  mrose
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


#include "manifest.h"
#include "quipu/util.h"
#include "psap.h"
#include "tailor.h"
#ifdef	BSD42
#include <sys/file.h>
#endif
#ifdef	SYS5
#include <fcntl.h>
#endif
#include <sys/stat.h>

extern char     fname[];

#define	OPT	(!frompipe || rps -> ps_byteno == 0 ? opt : rps)
#define	RPS	(!frompipe || opt -> ps_byteno == 0 ? rps : opt)
extern	char	frompipe;
extern	PS	opt, rps;

editentry (argc, argv)
int             argc;
char          **argv;
{
	char            str[LINESIZE];
	char            prog[LINESIZE];
	int             res;
	extern char     inbuf[];
	extern int      fd;
	extern char	remote_prob;
	extern char	dad_flag;

	if (argc != 1) {
		Usage (argv[0]);
		return (NOTOK);
	}

	(void) sprintf (str, "%s %s",
			_isodefile (isodebinpath, "editentry"), fname);

	if (!frompipe)
	    return (system (str) ? NOTOK : OK);
	
	if (!dad_flag) {
		(void) sprintf (prog, "e%s\n", str);

		send_pipe_aux (prog);

		if ((res = read_pipe_aux (prog,sizeof prog)) < 1) {
			(void) fprintf (stderr, "read failure\n");
			remote_prob = TRUE;
			return (NOTOK);
		} else {
			if ((res == 1) && (*prog == 'e')) {
				remote_prob = FALSE;
				return (NOTOK);	 /* remote error - abandon ! */
			} 
			if (*fname != '/') {
				char            tempbuf[LINESIZE];

				/* relative path... prefix cwd */
				*(prog + res) = 0;
				(void) sprintf (tempbuf, "%s/%s", prog, fname);
				(void) strcpy (fname, tempbuf);
			}
		}
	} else {
#ifndef	SOCKETS
		ps_printf (OPT,
			   "operation not allowed when using directory assistance server!\n");
		return NOTOK;
#else
		int	cc, i, j;
		char *cp, *dp;
		FILE *fp;
		struct stat st;
		extern int errno;

		if ((fp = fopen (fname, "r+")) == NULL) {
		    ps_printf (OPT, "unable to open %s for rw: %s\n",
			       fname, sys_errname (errno));
		    return NOTOK;
		}
		if (fstat (fileno (fp), &st) == NOTOK
		        || (st.st_mode & S_IFMT) != S_IFREG
		        || (cc = st.st_size) == 0) {
		    ps_printf (OPT, "%s: not a regular file\n", fname);
out: ;
		    (void) fclose (fp);
		    return NOTOK;
		}

		(void) sprintf (prog, "e%d\n", cc);
		send_pipe_aux (prog);

		if ((res = read_pipe_aux (prog, sizeof prog)) < 1) {
		    (void) fprintf (stderr, "read failure\n");
		    remote_prob = TRUE;
		    goto out;
		}
		else
		    if ((res == 1) && (*prog == 'e')) {
			remote_prob = FALSE;
			goto out;
		    }
		
		if ((cp = malloc ((unsigned) (cc))) == NULL) {
		    ps_printf (OPT, "out of memory\n");
		    goto out;
		}
		for (dp = cp, j = cc; j > 0; dp += i, j -= i)
		    switch (i = fread (dp, sizeof *dp, j, fp)) {
			 case NOTOK:
			     ps_printf (OPT, "error reading %s: %s\n",
					fname, sys_errname (errno));
			     goto out2;

			 case OK:
			     ps_printf (OPT, "premature eof reading %s\n",
					fname);
out2: ;
			     free (cp);
			     goto out;

			 default:
			     break;
		    }

		send_pipe_aux2 (cp, cc);
		free (cp), cp = NULL;
		
		if ((res = read_pipe_aux2 (&cp, &cc)) < 1) {
		    (void) ps_printf (OPT, "read failure\n");
		    remote_prob = TRUE;
		    goto out;
		}
		if (res == 1) {
		    if (*cp != 'e')
			(void) ps_printf (OPT, "remote protocol error: %s\n",
					  cp);
		    goto out;
		}

		(void) fclose (fp);
		if ((fp = fopen (fname, "w")) == NULL) {
		    ps_printf (OPT, "unable to re-open %s for writing: %s\n",
			       fname, sys_errname (errno));
		    free (cp);
		    return NOTOK;
		}

		if (fwrite (cp, sizeof *cp, cc, fp) == 0) {
		    ps_printf (OPT, "error writing %s: %s\n",
			       fname, sys_errname (errno));
		    goto out2;
		}

		free (cp);
		(void) fclose (fp);
#endif
	}

	return (OK);
}


get_password (str,buffer)
char * str;
char * buffer;
{

	char            prog[LINESIZE];
	int             res;
	extern char     inbuf[];
	extern int      fd;
	extern char	remote_prob;
	char * 		getpassword ();

	if (frompipe) {
		(void) sprintf (prog, "p%s\n", str);

		send_pipe_aux (prog);

		if ((res = read_pipe_aux (prog,sizeof prog)) < 1) {
			(void) fprintf (stderr, "read failure\n");
			remote_prob = TRUE;
			return;
		} else {
			*(prog+res) = 0;
			(void) strcpy (buffer, prog + 1);
		}
	} else {
		(void) sprintf (buffer,"Enter password for \"%s\": ",str);
		(void) strcpy (buffer,getpassword (buffer));
	}
}

yesno (str)
char * str;
{
	char            prog[LINESIZE];
	extern char     inbuf[];
	extern int      fd;
	extern char	remote_prob;
	char * 		getpassword ();

	if (frompipe) {
		(void) sprintf (prog, "y%s\n", str);

		send_pipe_aux (prog);

		if (read_pipe_aux (prog,sizeof prog) < 1) {
			(void) fprintf (stderr, "read failure\n");
			remote_prob = TRUE;
			return FALSE;
		}
	} else {
		ps_printf (OPT,"%s",str);
		(void) fgets (prog, sizeof prog, stdin);
	}

	switch (prog[0]) {
	    case 'y':
	        return OK;

	    case 'n':
	    default:
		return NOTOK;

	    case 'N':
		return DONE;
	}
}
