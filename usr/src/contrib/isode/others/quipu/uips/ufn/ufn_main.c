/* ufn_main.c - stand-alone UFN user-interface */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/uips/ufn/RCS/ufn_main.c,v 7.3 91/02/22 09:33:15 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/quipu/uips/ufn/RCS/ufn_main.c,v 7.3 91/02/22 09:33:15 mrose Interim $
 *
 *
 * $Log:	ufn_main.c,v $
 * Revision 7.3  91/02/22  09:33:15  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/17  11:50:43  mrose
 * sync
 * 
 * Revision 7.1  90/07/09  14:42:35  mrose
 * sync
 * 
 * Revision 7.0  90/06/13  18:52:43  mrose
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


#include "quipu/ufn.h"
#include "quipu/bind.h"
#include "quipu/dsp.h"
#include "quipu/ds_error.h"

extern char * dn2str();
extern char * dn2ufn();
static PS ps;

/* ARGSUSED */

DNS interact (dns,dn,s)
DNS dns;
DN dn;
char * s;
{
char    buf[LINESIZE];
DNS tmp;
DNS result = NULLDNS;

	if (dns == NULLDNS)
		return NULLDNS;

	ps_printf (ps,"Please select from the following (matching '%s'):\n",s);

	while (dns != NULLDNS) {
		ps_printf (ps,"  %s [y/n] ? ",dn2ufn(dns->dns_dn,FALSE));
		(void) ps_flush (ps);

again:;
		if (gets (buf) == NULL) {
			clearerr (stdin);
			ps_print (ps,"\n");
			return result;
		}

		if ((buf[0] == NULL) 
			|| (strlen(buf) != 1)
			|| ((buf[0] != 'y') && (buf[0] != 'n'))) {
				ps_print (ps,"Please type 'y' or 'n': ");
				(void) ps_flush (ps);
				goto again;
			}

		if (buf[0] == 'y') {
		    tmp = dns -> dns_next;
		    dns -> dns_next = result;
		    result = dns;
		    dns = tmp;
		} else {
		    tmp = dns;
		    dns = dns -> dns_next;
		    tmp -> dns_next = NULL;
		    dn_seq_free (tmp);
		}
	}
	return result;
}

bind_to_dsa ()
{
  struct ds_bind_arg bindarg;
  struct ds_bind_arg bindresult;
  struct ds_bind_error binderr;

  bindarg.dba_version = DBA_VERSION_V1988;

  bindarg.dba_passwd_len = 0;
  bindarg.dba_passwd [0] = '\0';
  bindarg.dba_dn = NULLDN;

  if (ds_bind (&bindarg,&binderr,&bindresult) != DS_OK)
    	return FALSE;
  else
	return TRUE;
}



main (argc,argv)
int argc;
char ** argv;
{
char buffer [1024];
char use_dish = FALSE;
char full_dn = FALSE;
char got_arg = FALSE;
int n;
envlist	el;

	buffer[0] = NULL;

	for (n=1; n<argc; n++) {
		if (*argv[n] == '-') {
			if ((strlen (argv[n]) != 2) 
			   || ((buffer[0] != NULL) && (n != argc - 1))
			   || got_arg) {
usage:;
				(void) fprintf (stderr,"Usage: %s [-df] [name]\n",argv[0]);
				(void) fprintf (stderr,"   -d:  send results to dish for processing (assumes dish is running in the background)\n");
				(void) fprintf (stderr,"   -f:  print the full DN in results\n");
				exit (-4);
			   }

			got_arg = TRUE;
			if (argv[n][1] == 'd') 
				use_dish = TRUE;
			else if (argv[n][1] == 'f')
				full_dn = TRUE;
			else
				goto usage;
			continue;
		}
		(void) strcat (buffer," ");
		(void) strcat (buffer, argv[n]);
	}

	ufn_notify = TRUE;

	if ((ps = ps_alloc (std_open)) == NULLPS) {
	      (void) fprintf (stderr,"Can't allocate output PS\n");
	      exit (-1);
	}
	if (std_setup (ps, stdout) == NOTOK) {
	      (void) fprintf (stderr,"Can't set up output PS\n");
	      exit (-2);
	}

	isodetailor ("ufn",1);

	quipu_syntaxes ();
	dsap_init ((int *)0,(char ***)0);
	check_known_oids();
	(void) ufn_init ();
	if ((el = read_envlist ()) == NULLEL) {
	    (void) fprintf (stderr, "%s\n", PY_pepy);
	    exit (-5);
	}

	if (buffer[0] != NULL) 
		do_name (ps,buffer,use_dish,full_dn,el);
	else 
		for (;;) {
			ps_print (ps,"\nufn> ");
			(void) ps_flush (ps);
			if (gets (buffer) == NULL) 
				break;
			if (buffer[0] != NULL) 
				do_name (ps,buffer,use_dish,full_dn,el);
		}

	(void) ds_unbind();
}

do_name (opt,buffer,use_dish,full_dn,el)
PS opt;
char * buffer;
char use_dish;
char full_dn;
envlist	el;
{
char command [1024];
int n;
char * v[20];
extern int print_parse_errors, parse_status;
extern char PY_pepy[];
int old;
DNS dns = NULLDNS, tdns;
static char bound = FALSE;
DN marker;
extern char * local_dit;

	if ((n = sstr2arg (buffer,20,v,",")) == NOTOK) {
		(void) fprintf (stderr, "Can't parse input !!!\n");
		return;
	}

	if (n == 1) {
		DN dn;
		char * ptr;

		if (*v[0] == '@')
			ptr = strdup (v[0]);
		else if ((ptr = alias2name (v[0])) != NULLCP)
			ptr = strdup (ptr);
		else
			goto use_ufn;

		old = print_parse_errors;
		print_parse_errors = FALSE;
		if ((dn = str2dn (ptr)) != NULLDN) {
			if (use_dish) {
				(void) sprintf (command,"showentry -fred \"@%s\"\n",dn2str(dn));
				dish (command);	
			} else {
				ps_print (opt,"\nThat's an easy one:\n   ");
				if (full_dn)
					dn_print (opt,dn,EDBOUT);
				else 
					ufn_dn_print (opt,dn,TRUE);
			}
			ps_print (opt,"\n");
			return;
		}
		free (ptr);
		parse_status--;		/* ignore error */
		print_parse_errors = old;
	}

use_ufn:;
	if (!bound) {
		(void) fprintf (stderr,"Connecting to DSA...\n");
		if (! bind_to_dsa()) {
			(void) fprintf (stderr,"Can't contact DSA\n");
			exit (-3);
		}
		bound = TRUE;
	}

	if ( ! ufn_match (n,v,interact,&dns,el))
	    (void) fprintf (stderr, "Try again later !!!\n");
	else {
		if (dns == NULLDNS) {
			ps_print (opt, "Nothing found\n");
		} else if (dns->dns_next == NULLDNS) {
			if (use_dish) {
				(void) sprintf (command,"showentry -fred \"@%s\"\n",dn2str(dns->dns_dn));
				dish (command);	
			} else {
				ps_print (opt,"\nThe following name was matched:\n   ");
				if (full_dn)
					dn_print (opt,dns->dns_dn,EDBOUT);
				else 
					ufn_dn_print (opt,dns->dns_dn,TRUE);
			}
			ps_print (opt,"\n");
			return;
		} else {
			DN next, trail = NULLDN;

			(void) printf ("\nThe following names were matched...\n");

			if (!full_dn) {
				DN tdn;
				marker = str2dn (local_dit);
				for (tdns=dns; (tdns!= NULLDNS) && (marker != NULLDN); tdns=tdns->dns_next) {
					tdn = tdns->dns_dn;
					for (next=marker; 
							(tdn!= NULLDN) && (next != NULLDN); 
							next=next->dn_parent, tdn=tdn->dn_parent) {
						if (dn_comp_cmp(tdn,next) != 0) {
							if (trail == NULLDN) 
								marker = NULLDN;
							else 
								trail->dn_parent = NULLDN;
							dn_free (next);
							break;
						}
						trail = next;
					}
					if (tdn == NULLDN) {
						if (trail == NULLDN) 
							marker = NULLDN;
						else 
							trail->dn_parent = NULLDN;
						dn_free (next);
					}
				}
			}

			for (; dns!= NULLDNS; dns=dns->dns_next) {
				if (use_dish) {
					(void) sprintf (command,"squid -alias \"@%s\"\n",dn2str(dns->dns_dn));
					dish (command);	
				} else {
					ps_print (opt,"   ");
					if (full_dn)
						dn_print (opt,dns->dns_dn,EDBOUT);
					else 
						ufn_dn_print_aux (opt,dns->dns_dn,marker,TRUE);
					ps_print (opt,"\n");
				}
			}
			ps_print (opt,"\n");
		}
	}
}

#include <varargs.h>

#ifndef lint

void    advise (va_alist)
va_dcl
{
    int     code;
    va_list ap;

    va_start (ap);

    code = va_arg (ap, int);

    (void) fprintf (stderr, ap);

    va_end (ap);
}

#else
/* VARARGS */

void    advise (code, what, fmt)
char    *what,
        *fmt;
int      code;
{
    advise (code, what, fmt);
}
#endif
