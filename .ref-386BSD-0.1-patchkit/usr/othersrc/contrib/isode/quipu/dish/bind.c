/* bind.c - */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/dish/RCS/bind.c,v 7.8 91/03/09 11:55:55 mrose Exp $";
#endif

/* 
 * $Header: /f/osi/quipu/dish/RCS/bind.c,v 7.8 91/03/09 11:55:55 mrose Exp $
 *
 *
 * $Log:	bind.c,v $
 * Revision 7.8  91/03/09  11:55:55  mrose
 * update
 * 
 * Revision 7.7  91/02/22  09:40:20  mrose
 * Interim 6.8
 * 
 * Revision 7.6  90/10/17  11:55:10  mrose
 * sync
 * 
 * Revision 7.5  90/07/09  14:46:57  mrose
 * sync
 * 
 * Revision 7.4  90/04/18  08:49:30  mrose
 * 6.2
 * 
 * Revision 7.3  90/03/15  11:18:16  mrose
 * quipu-sync
 * 
 * Revision 7.2  90/01/11  18:37:33  mrose
 * real-sync
 * 
 * Revision 7.1  89/12/19  16:20:58  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:19:56  mrose
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


#include <signal.h>
#include "manifest.h"
#include "quipu/util.h"
#include <fcntl.h>
#include <sys/stat.h>
#include "quipu/dua.h"
#include "quipu/name.h"
#include "quipu/bind.h"
#include "quipu/dsp.h"
#include "quipu/ds_error.h"
#include "tailor.h"
#include "pepsy.h"
#include "quipu/DAS_pre_defs.h"

extern DN       fixed_pos;
DN	        user_name;

#define	OPT	(!frompipe || rps -> ps_byteno == 0 ? opt : rps)
#define	RPS	(!frompipe || opt -> ps_byteno == 0 ? rps : opt)
extern	char	frompipe;
extern	PS	opt, rps;

extern char	retpipe[],
		inbuf [],
		bound;

extern char	dad_flag;

extern int	dsap_ad;
extern unsigned	connect_time,
		cache_time;

static char	username [LINESIZE];
static char	password [LINESIZE];
static int	auth_type = DBA_AUTH_SIMPLE; 
static char	first_bind = TRUE;

char		neverefer = FALSE;

static struct ds_bind_arg bindarg;
static struct ds_bind_arg bindresult;
static struct ds_bind_error binderr;

static int 	main_dsa_id;
static int	referral_dsa;

#ifndef NO_STATS
extern LLog * log_stat;
#endif 
extern LLog * log_dsap;

extern int parent_pid;
static unsigned waiting = 0;

extern char *new_version();
extern long hash_passwd();
extern char *hash2str();
extern struct signature *sign_operation();
extern struct certificate *my_certificate;
extern struct certificate *cert_cpy();
extern int encode_DAS_TokenToSign();

SFD alarm_sig ()
{
SFD dish_quit ();

	if (frompipe && (parent_pid != 0))
		if (kill (parent_pid,0) == -1) {
			/* invoking shell gone - exit */
			dish_quit (SIGHUP);
		}
		

	if (bound) {
		(void) dap_unbind (main_dsa_id);
#ifndef NO_STATS
	LLOG (log_stat,LLOG_NOTICE,("Connection closed"));
#endif
		bound = FALSE;
		if (referral_dsa != 0) {
			(void) dap_unbind (referral_dsa);
			referral_dsa = 0;
		}
	}

	waiting += connect_time;
	if (frompipe && (waiting >= cache_time))
		dish_quit (SIGHUP);
		
	(void) signal (SIGALRM, alarm_sig);
	(void) alarm (connect_time);
}

set_alarm ()
{
	waiting = 0;
	(void) signal (SIGALRM, alarm_sig);
	(void) alarm (connect_time);
}

SFD bind_sig ()
{
extern jmp_buf  dish_env;

	ps_print (OPT,"Bind timeout\n");
	if (referral_dsa != 0) {
		referral_dsa = 0;
		dsap_ad = main_dsa_id;
	}
	longjmp (dish_env,1);
}

bind_alarm ()
{
	(void) signal (SIGALRM, bind_sig);
	(void) alarm (connect_time);
}

call_bind (argc,argv)
int argc;
char ** argv;
{
int 	x;
char    noconnect = FALSE;
static  char    bdsa  [LINESIZE], save_bdsa[LINESIZE];
char   *save_address;
extern  char * dsa_address,
	     * myname;
FILE    *fp;
char    buf[BUFSIZ];
DN	newdn;
extern  char * tailfile;
extern DN sequence_dn ();
char got_name = FALSE;
char got_pass = FALSE;

	bdsa[0] = 0;
	save_address = dsa_address;

	for (x=1; x<argc; x++) {
		if (test_arg (argv[x], "-noconnect",3))
			noconnect = TRUE;
#ifdef PDU_DUMP
		else if (test_arg (argv[x], "-pdus",2)) {
			if (++x == argc) {
				ps_print (OPT,"PDU file name missing\n");
				Usage (argv[0]);
				return (NOTOK);
			}
			ps_printf (RPS,"Dumping PDUs in directory %s\n",argv[x]);
			pdu_dump_init (argv[x]);
		}
#endif		
 		else if (test_arg (argv[x], "-user",1)) {
			got_name = TRUE;
			if ((++x == argc) || (*argv[x] == '-')) {
				x--;
				*username = 0;
			} else
				(void) strcpy (username,argv[x]);
		} else if (test_arg (argv[x], "-pipe",2)) {
			if (strcmp (argv[0],"dish") == 0)
				ps_print (OPT,"Sorry... '-pipe' must be the first argument to dish.\n");
			else 
				Usage (argv[0]);
			return (NOTOK);
		} else if (test_arg (argv[x], "-norefer",3))
			neverefer = TRUE;
		else if (test_arg (argv[x], "-refer",1))
			neverefer = FALSE;
		/* these flags select the mode of authentication only */
		else if (test_arg (argv[x],"-noauthentication",3)) 
			auth_type = DBA_AUTH_NONE;
		else if (test_arg (argv[x], "-protected", 3))
			auth_type = DBA_AUTH_PROTECTED;
		else if (test_arg (argv[x], "-simple", 3))
			auth_type = DBA_AUTH_SIMPLE;
		else if (test_arg (argv[x], "-strong", 3))
			auth_type = DBA_AUTH_STRONG;
		/* -password sets the `key', whatever the mode  */
		else if (test_arg (argv[x], "-password",2)) {
			got_pass = TRUE;
			if ((++x == argc)  || (*argv[x] == '-')) {
				x--;
				*password = 0;
			} else {
				int i;
				(void) strcpy (password,argv[x]);
				for (i=0; i< strlen(password) ; i++)
					if ( i < 4 )
						argv[x][i] = 'X';
					else
						argv[x][i] = 0;

			}
		} else if (test_arg (argv[x], "-call",1)) {
			if (++x == argc) {
				ps_print (OPT,"dsa name missing\n");
				Usage (argv[0]);
				return (NOTOK);
			}
			(void) strcpy (bdsa,argv[x]);
		} else {
			/* assume its the user name */
			if (got_name) {
				ps_print (OPT,"One user name only please!\n");
				Usage (argv[0]);
				return (NOTOK);
			} 
			got_name = TRUE;
			(void) strcpy (username,argv[x]);
			if (*username == '-') {
				ps_printf (OPT,"Unknown option %s\n",username);
				Usage (argv[0]);
				return (NOTOK);
			}
		}
	}


	if (noconnect)
		return (OK);

	if (isdigit (*username)) {
	    PS	    ps;

	    if ((newdn = sequence_dn (atoi (username))) == NULLDN) {
		ps_printf (OPT,"Invalid sequence in username %s\n",username);
		Usage (argv[0]);
		return (NOTOK);
	    }
	    if ((ps = ps_alloc (str_open)) == NULLPS) {
		ps_printf (OPT, "Unable to expand sequence: out of memory\n");
		return NOTOK;
	    }
	    if (str_setup (ps, username, sizeof username - 2, 1) == NOTOK) {
		ps_printf (OPT, "Unable to expand sequence: %s\n",
			   ps_error (ps -> ps_errno));
		ps_free (ps);
		return NOTOK;
	    }
	    dn_print (ps, newdn, EDBOUT);
	    ps_print (ps, " ");
	    *--ps -> ps_ptr = NULL, ps -> ps_cnt++;

	    ps_free (ps);
	}
	else
	    newdn = NULLDN;

	if ((got_name && ! got_pass) || (*password == 0)) {
		bindarg.dba_passwd_len = 0;
		bindarg.dba_passwd[0]  = 0;
		if ((*username != 0) && (auth_type != DBA_AUTH_NONE)) {
			get_password (username, password);
			(void) strcpy(&bindarg.dba_passwd[0], password);
			bindarg.dba_passwd_len = strlen	(&bindarg.dba_passwd[0]);
		}
	} else {
		bindarg.dba_passwd_len = strlen (password);
		(void) strcpy (bindarg.dba_passwd, password);
	}

	if ((bindarg.dba_passwd_len == 0) && (auth_type != DBA_AUTH_STRONG))
		auth_type = DBA_AUTH_NONE;

	if (*username == 0) {
		bindarg.dba_dn = NULLDN;
		/* Don't need credentials to bind as NULLDN! */
		auth_type = DBA_AUTH_NONE;
	}
	else
		if (newdn)
		    bindarg.dba_dn = dn_cpy (newdn);
	    	else {
		    if ((bindarg.dba_dn = str2dn (username[0] != '@' ? username
								  : username + 1))
			    == NULLDN) {
			ps_printf (OPT,"Invalid DN for username: %s\n",username);
			Usage (argv[0]);
			return (NOTOK);
		    }
		}

	/* prepare credentials */
	bindarg.dba_auth_type = auth_type;
	switch (auth_type) {
	case DBA_AUTH_NONE:
	case DBA_AUTH_SIMPLE:
		break;
	case DBA_AUTH_PROTECTED: 
		protect_password();
		break;
	case DBA_AUTH_STRONG:
		sign_bindarg();
		break;
	}

	/* now set dsa_address */
	if (bdsa[0] != 0) {
	        (void) strcpy (myname = save_bdsa, bdsa);
		dsa_address = NULLCP;

		/* read tailor file to get address */
		
		if( (fp = fopen(isodefile(tailfile, 0), "r")) == (FILE *)NULL) {
			LLOG (log_dsap,LLOG_FATAL,("can't open %s",tailfile));
			fatal (-72, "Cannot open tailor file");
		}

		while(fgets(buf, sizeof(buf), fp) != NULLCP)
			if ( (*buf != '#') && (*buf != '\n') )
				/* not a comment or blank */
				if (tai_string (buf) == NOTOK)
					DLOG (log_dsap,LLOG_DEBUG,("tai_string failed %s",buf));

		(void) fclose(fp);
		
		if (dsa_address == NULLCP)
			dsa_address = myname;
	}

	if (bound) 
		(void) ds_unbind ();
		
	bound = FALSE;
	first_bind = FALSE;

	binderr.dbe_value = 0;
	bind_alarm ();
	if (secure_ds_bind (&bindarg, &binderr, &bindresult) != OK) {
		(void) signal (SIGALRM, SIG_IGN);
		if (binderr.dbe_value == 0)
			ps_print (OPT, "*** Service error : Unable to contact DSA ***\n");
		else
			ds_bind_error(OPT, &binderr);
		dsa_address = save_address;
		return (NOTOK);
	} 
	(void) signal (SIGALRM, SIG_IGN);
	main_dsa_id = dsap_ad;

#ifndef NO_STATS
	LLOG (log_stat,LLOG_NOTICE,("Bound '%s' to '%s'",username,myname));
#endif

	bound = TRUE;
	user_name = bindarg.dba_dn;

 	return (OK);
	
}

rebind () {

	if (referral_dsa != 0) {
		(void) dap_unbind (referral_dsa);
		referral_dsa = 0;
		dsap_ad = main_dsa_id;
	}

	if (first_bind) {
		char * buff = "bind";
		return (call_bind (1,&buff));
	}

	if (bound)
		return (OK);
		
	/* prepare credentials */
	bindarg.dba_auth_type = auth_type;
	switch (auth_type) {
	case DBA_AUTH_NONE:
	case DBA_AUTH_SIMPLE:
		break;
	case DBA_AUTH_PROTECTED: 
		protect_password();
		break;
	case DBA_AUTH_STRONG:
		sign_bindarg();
		break;
	}

	binderr.dbe_value = 0;
	bind_alarm ();
	if (secure_ds_bind (&bindarg, &binderr, &bindresult) != OK) {
		(void) signal (SIGALRM, SIG_IGN);
		if (binderr.dbe_value == 0)
			ps_print (OPT, "*** Service error: Unable to contact DSA ***\n");
		else
			ds_bind_error(OPT, &binderr);
		return (NOTOK);
	} 
	(void) signal (SIGALRM, SIG_IGN);
	main_dsa_id = dsap_ad;
#ifndef NO_STATS
	LLOG (log_stat,LLOG_NOTICE,("re-connect"));
#endif

	bound = TRUE;
	user_name = bindarg.dba_dn;
	return (OK);
}

referral_bind (addr) 
struct PSAPaddr * addr;
{
	if (referral_dsa != 0) 
		(void) dap_unbind (referral_dsa++);
	else
		referral_dsa = dsap_ad + 1;

	dsap_ad = referral_dsa;

	/* prepare credentials */
	bindarg.dba_auth_type = auth_type;
	switch (auth_type) {
	case DBA_AUTH_NONE:
	case DBA_AUTH_SIMPLE:
		break;
	case DBA_AUTH_PROTECTED: 
		protect_password();
		break;
	case DBA_AUTH_STRONG:
		sign_bindarg();
		break;
	}

	binderr.dbe_value = 0;
	bind_alarm ();
	if (dap_bind (&dsap_ad, &bindarg, &binderr, &bindresult, addr) != OK) {
		(void) signal (SIGALRM, SIG_IGN);
		if (binderr.dbe_value == 0)
			ps_print (OPT, "*** Service error : Unable to contact DSA ***\n");
		else
			ds_bind_error(OPT, &binderr);
		referral_dsa = 0;
		dsap_ad = main_dsa_id;
		return (0);
	}
	(void) signal (SIGALRM, SIG_IGN);
	referral_dsa = dsap_ad;

#ifndef NO_STATS
	LLOG (log_stat,LLOG_NOTICE,("referral connect"));
#endif

	return (1);
}

call_unbind (argc,argv)
int argc;
char ** argv;
{
	int		x;
	char		noquit = FALSE;
	extern char	resbuf [];
	static int very_first_time = 1;

	for (x=1; x<argc; x++) {
		if (test_arg (argv[x], "-noquit",3))
			noquit = TRUE;
		else {
			Usage (argv[0]);
			return;
		}
	}
	if (!noquit) 
		(void) signal (SIGINT, SIG_DFL);

	if (bound) {
		(void) dap_unbind (main_dsa_id);
		if (referral_dsa != 0) {
			(void) dap_unbind (referral_dsa);
			referral_dsa = 0;
		}
	}
		
	bound = FALSE;
		
	if (! noquit) {
		if (frompipe)
			exit_pipe ();
		ps_free (opt);
		ps_free (rps);
		exit (0);
	}
	else
	    if (dad_flag && very_first_time) {
		very_first_time = 0;
		cache_time = 24 * 60 * 60;
	    }
}

extern char no_rcfile;
static time_t rc_mod_time;
extern time_t time ();
static char Dish_Home[LINESIZE];

user_tailor ()
{
	int		isenv;
	char           *part1;
	char           *part2;
	char           *getenv ();
	char           *home;

	FILE           *file;
	char            Read_in_Stuff[LINESIZE];
	char           *p, 
		       *TidyString();
	extern char    *local_dit;
	extern char	dishinit;
	struct	 stat	sbuf;

	*password = 0;
	*username = 0;

	set_sequence ("default");

	(void) set_cmd_default ("modify","-dontusecopy");	
		/* we dont want to make templates with copies */


	bzero ((char *)&bindarg, sizeof bindarg);
	bindarg.dba_version = DBA_VERSION_V1988;

	isenv = 0;
	if (home = getenv ("QUIPURC"))
	    (void) strcpy (Dish_Home, home), isenv = 1;
	else
	    if (home = getenv ("HOME"))
		(void) sprintf (Dish_Home, "%s/.quipurc", home);
	    else
		(void) strcpy (Dish_Home, "./.quipurc");

	if (no_rcfile)
	    goto out;

	if (stat (Dish_Home,&sbuf) != 0) {
	        if (isenv)
		    goto no_dice;

		if (dishinit && !frompipe) {
			char cmd_buf [LINESIZE];
			int msk;

			ps_print (OPT,"Please wait whilst I initialise everything...\n");
			msk = umask (0111);
			(void) strcpy (cmd_buf, isodefile ("new_quipurc", 1));
			if ((file = fopen (Dish_Home, "w")) == 0) 
				return (OK);	/* cant make one */
			(void) umask (msk);
			(void) fclose (file);
			if (system (cmd_buf) == 0) {
				(void) chmod (Dish_Home,0600);
				if ((file = fopen (Dish_Home, "r")) == 0) {
					(void) fprintf (stderr,"Cant open %s - BUT I just created it!!!\n", Dish_Home);
					return (NOTOK);
				}
			} else {
				(void) unlink (Dish_Home);
				return (NOTOK);
			}
			rc_mod_time = time ((time_t *)0);
		} else {
			rc_mod_time = time ((time_t *)0);
			goto out;
		}
	} else 
		rc_mod_time = sbuf.st_mtime;

	if ((file = fopen (Dish_Home, "r")) == 0) {
no_dice: ;
		(void) fprintf (stderr,"Cant open ");
		perror (Dish_Home);
		return NOTOK;
	}

	while (fgets (Read_in_Stuff, LINESIZE, file) != 0) {
		p = SkipSpace (Read_in_Stuff);
		if (( *p == '#') || (*p == '\0'))
			continue;  /* ignore comments and blanks */
			
		part1 = p;
		if ((part2 = index (p,':')) == NULLCP) {
			ps_printf (OPT,"Seperator missing '%s'\n",p);
			return (NOTOK);
		} 
		
		*part2++ = '\0';
		part2 = TidyString (part2);

		if (lexequ (part1, "username") == 0) {
			if ((user_name = str2dn (*part2 != '@' ? part2
						 	       : part2 + 1))
			    == NULLDN) {
				ps_printf (OPT,"Invalid DN for username: %s\n",part2);
				return (NOTOK);
			}
			(void) strcpy (username, part2);
			bindarg.dba_dn = user_name;
		}
		else if (lexequ (part1, "password") == 0) {
			(void) strcpy (bindarg.dba_passwd,part2);
			(void) strcpy (password, part2);
			bindarg.dba_passwd_len = strlen (part2);
		} 
		else if (lexequ (part1, "certificate") == 0) {
			struct certificate *str2cert();
			my_certificate = str2cert(part2);
		} else if (lexequ (part1, "secret_key") == 0) 
			(void) set_secret_key(part2);
		else if (lexequ (part1, "cache_time") == 0) 
			cache_time = MIN (atoi(part2) * 60, 5 * 60 * 60);
					/* enforce 5 hour maximum */
		else if (lexequ (part1, "connect_time") == 0) 
			connect_time = MIN (atoi(part2) * 60, 5 * 60);
					/* enforce 5 minute maximum */
		else if (lexequ (part1, "service") == 0) 
			new_service (part2);
		else if (lexequ (part1, "type") == 0) {
			if (lexequ (part2,"unknown") == 0)
				show_unknown();
		} else if (lexequ (part1, "notype") == 0)
			new_ignore (part2);
		else if (lexequ (part1, "sequence") == 0) {
			DN sdn;
			if ( (sdn = str2dn (*part2 != '@' ? part2 : part2 + 1))
			    == NULLDN) {
                                ps_printf (OPT,"Invalid DN for sequence: %s\n",part2);
                                return (NOTOK);
			}
			(void) add_sequence (sdn);
		} else if (lexequ (part1, "dsap") == 0)
			(void) tai_string (part2);
		else if (lexequ (part1, "isode") == 0) {
			char * split;
			if ((split = index (part2,' ')) != NULLCP) {
				*split++ = 0;
				(void)isodesetvar (part2,strdup(split),0);
			}
		} else if (set_cmd_default (part1,part2) != OK) {
			if (*part2 == '@')
				part2++;
			if (add_alias (part1,part2) != OK) {
				ps_printf (OPT,"Unknown parameter %s\n",part1);
				return (NOTOK);
			}
		}
	}
	(void) fclose (file);

out:;
	if ((local_dit != NULLCP) && (strcmp ("@", TidyString(local_dit)) != 0))
		if ((fixed_pos = str2dn (*local_dit != '@' ? local_dit
					 		   : local_dit + 1))
		    == NULLDN) {
			ps_printf (OPT,"Invalid DN for dsaptailor default position: %s\n",local_dit);
			return (NOTOK);
		}

	(void) strcpy (bindarg.dba_passwd,password);
	bindarg.dba_passwd_len = strlen (password);

	isodexport (NULLCP);
	
	return (OK);
}

test_rc_file (ps)
PS  ps;
{
	struct	 stat	sbuf;

	if (stat (Dish_Home,&sbuf) != 0)
		return;

	if (rc_mod_time < sbuf.st_mtime) {
		ps_printf (ps,"WARNING: %s has changed - but not re-read!!!\n",
			  Dish_Home);
		rc_mod_time = sbuf.st_mtime;
	}

}

SFD dish_quit (sig)
int sig;
{
	if (bound) {
		(void) dap_unbind (main_dsa_id);
		if (referral_dsa != 0) {
			(void) dap_unbind (referral_dsa);
			referral_dsa = 0;
		}
	}

	if (frompipe)
		exit_pipe ();
	else
		(void) fprintf (stderr,"Dish Problem\n");

	hide_picture();

	switch (sig) {
		case SIGALRM:
			LLOG (log_dsap, LLOG_EXCEPTIONS, ("Timer expired :- Dish quitting"));
			exit (0);
		case SIGHUP:
		case SIGINT:
		case SIGTERM:
			LLOG (log_dsap, LLOG_EXCEPTIONS, ("Dish quitting - signal %d",sig));
			exit (0);
		default:
			LLOG (log_dsap, LLOG_FATAL, ("Dish problem - signal %d",sig));
			(void) signal (sig, SIG_DFL); /* to stop recursion */
			abort ();
	}
			
}

static int protect_password()
{
long hash;
char *cp;
int len;

	bindarg.dba_time1 = new_version();
	bindarg.dba_time2 = NULLCP;
	bindarg.dba_r1.n_bits = 0;
	bindarg.dba_r1.value = NULLCP;
	bindarg.dba_r2.n_bits = 0;
	bindarg.dba_r2.value = NULLCP;
	hash = 0;
	hash = hash_passwd(hash, password, strlen(password));
	hash = hash_passwd(hash, bindarg.dba_time1, strlen(bindarg.dba_time1));
	cp = hash2str(hash, &len);
	bcopy(cp, bindarg.dba_passwd, len);
	bindarg.dba_passwd_len = len;
}


static int sign_bindarg()
{
struct signature sig;

	bindarg.dba_time1 = new_version();
	bindarg.dba_time2 = NULLCP;

	/* Have to send a random number, but don't care what it is */
	bindarg.dba_r1.n_bits = 8;
	bindarg.dba_r1.value = strdup("*");
	bindarg.dba_r2.n_bits = 8;
	bindarg.dba_r2.value = strdup("*");

        bindarg.dba_sig = &sig;
	/* Have to build a signature alg_id now, so can sign it. This will
	 * subsequently be replaced with a new alg_id by sign_operation.
	 */
        sig.alg.algorithm = oid_cpy(name2oid("sq_mod_n_with_rsa"));
        sig.alg.p_type = ALG_PARM_NUMERIC;
        sig.alg.un.numeric = 512;
	sig.alg.asn = NULLPE;

	bindarg.dba_sig = sign_operation((caddr_t) &bindarg, 
			_ZTokenToSignDAS, &_ZDAS_mod);
	if (my_certificate)
	{
 	bindarg.dba_cpath = (struct certificate_list *) 
		calloc(1, sizeof(struct certificate_list));
	bindarg.dba_cpath->cert = cert_cpy(my_certificate);
	}
}
