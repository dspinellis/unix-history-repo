/* service.c - */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/service.c,v 7.3 91/02/22 09:20:15 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/common/RCS/service.c,v 7.3 91/02/22 09:20:15 mrose Interim $
 *
 *
 * $Log:	service.c,v $
 * Revision 7.3  91/02/22  09:20:15  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/17  11:42:50  mrose
 * sync
 * 
 * Revision 7.1  90/03/15  11:19:10  mrose
 * quipu-sync
 * 
 * Revision 7.0  89/11/23  22:18:06  mrose
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


#include "quipu/util.h"
#include "quipu/commonarg.h"
#include "quipu/dua.h"
#include "quipu/sequence.h"

#define MAXSERV 20
static char default_service [BUFSIZ] = { 0 };
static char * serv_vec [MAXSERV];
static int  serv_argc;
static int default_serv_set = 0;
extern DN user_name;
extern int chase_flag;
static char do_shuffle;
int    copy_flag;
extern struct certificate *my_certificate;
char * result_sequence = NULLCP;
struct dua_sequence * current_sequence = NULL_DS;
struct dua_sequence * top_sequence = NULL_DS;


set_default_service (opt)
PS opt;
{
	if (default_service [0] == 0) {
		serv_argc = 0;
		return (OK);
	}

	if ((serv_argc = sstr2arg (default_service, MAXSERV, serv_vec, " \t")) == -1) {
		ps_print (opt,"Invalid default service controls");
		return (NOTOK);
	} else {
		default_serv_set = 1;
		return (OK);
	}
}

int get_default_service (ca)
CommonArgs * ca;
{
PS opt;
char buffer [LINESIZE];
extern int sizelimit;
extern int timelimit;
static CommonArgs sca = default_common_args;

	*ca = sca;  /* struct copy */
	ca->ca_servicecontrol.svc_sizelimit = sizelimit;
	ca->ca_servicecontrol.svc_timelimit = timelimit;
	
	if ((opt = ps_alloc (str_open)) == NULLPS) {
		(void) fprintf (stderr,"ps_alloc error\n");
		return (NOTOK);
	}
		
	if (str_setup (opt, buffer, LINESIZE, 1) == NOTOK) {
		(void) fprintf (stderr,"ps_setup error\n");
		return (NOTOK);
	}

	if (! default_serv_set)
		if (set_default_service (opt) != OK) {
			(void) fprintf (stderr,"error (1) - %s\n",buffer);
			ps_free (opt);
			return (NOTOK);

		}

	do_shuffle = FALSE;

	if (do_service_control (opt,serv_argc, serv_vec, ca) < 0) {
		(void) fprintf (stderr,"error (2) - %s\n",buffer);
		ps_free (opt);
		return (NOTOK);
	}

	ps_free (opt);
	return (OK);
	
}

int service_control (opt,argc, argv, ca)
PS 		opt;
int             argc;
char          **argv;
CommonArgs     *ca;
{

	if (get_default_service (ca) != OK) {
		ps_print (opt,"default service error - check quipurc\n");
		return (-1);
	}
	do_shuffle = TRUE;
	return (do_service_control (opt,argc, argv, ca));
}

int do_service_control (opt,argc, argv, ca)
PS 		opt;
int             argc;
char          **argv;
CommonArgs     *ca;
{

	ServiceControl  *sc;
	int             shuffle_up ();
	int             x;
	char            shuffle;

	sc = &(ca->ca_servicecontrol);
	copy_flag = TRUE;

	for (x = 0; x < argc; x++) {
		shuffle = do_shuffle;
		
		if (test_arg (argv[x], "-preferchain",3)) 
			sc->svc_options |= SVC_OPT_PREFERCHAIN;
		else if (test_arg (argv[x], "-nopreferchain",5)) 
			sc->svc_options &= ~SVC_OPT_PREFERCHAIN;
			
		else if (test_arg (argv[x], "-localscope",3)) 
			sc->svc_options |= SVC_OPT_LOCALSCOPE;
		else if (test_arg (argv[x], "-nolocalscope",3)) 
			sc->svc_options &= (~SVC_OPT_LOCALSCOPE);
			
		else if (test_arg (argv[x], "-dontusecopy",5)) {
			copy_flag = FALSE;
			sc->svc_options |= SVC_OPT_DONTUSECOPY;
		} else if (test_arg (argv[x], "-usecopy",1)) {
			copy_flag = TRUE;
			sc->svc_options &= (~SVC_OPT_DONTUSECOPY);
		}

		else if (test_arg (argv[x], "-dontdereferencealias",5)) 
			sc->svc_options |= SVC_OPT_DONTDEREFERENCEALIAS;
		else if (test_arg (argv[x], "-dereferencealias",3)) 
			sc->svc_options &= (~SVC_OPT_DONTDEREFERENCEALIAS);

		else if (test_arg (argv[x], "-low",3)) 
			sc->svc_prio = SVC_PRIO_LOW;
		else if (test_arg (argv[x], "-medium",2)) 
			sc->svc_prio = SVC_PRIO_MED;
		else if (test_arg (argv[x], "-high",1)) 
			sc->svc_prio = SVC_PRIO_HIGH;
			
		else if (test_arg (argv[x], "-timelimit",2)) {
			if (x + 1 == argc) {	
				ps_printf (opt, "We need a number for timelimit.\n");
				return (-1);
			} else {
				if (shuffle)
					shuffle_up (argc--, argv, x);
				else
				        x++;

				if ((sc->svc_timelimit = atoi (argv[x])) < -1) {
					ps_printf (opt, "We need a posative number for timelimit.\n");
					return (-1);
				}
			}
		} else if (test_arg (argv[x], "-notimelimit",4)) 
			sc->svc_timelimit = -1;
			
		else if (test_arg (argv[x], "-sizelimit",3)) {
			if (x + 1 == argc) {
				ps_printf (opt, "We need a number for sizelimit.\n");
				return (-1);
			} else {
				if (shuffle)
					shuffle_up (argc--, argv, x);
				else
					x++;

				if ((sc->svc_sizelimit = atoi (argv[x])) < -1) {
					ps_printf (opt, "We need a posative number for sizelimit.\n");
					return (-1);
				}
			}
		} else if (test_arg (argv[x], "-nosizelimit",4)) 
			sc->svc_sizelimit = -1;
			
		else if (test_arg (argv[x], "-nochaining",4)) 
			sc->svc_options |= SVC_OPT_CHAININGPROHIBIT;
		else if (test_arg (argv[x], "-chaining",3)) 
			sc->svc_options &= (~SVC_OPT_CHAININGPROHIBIT);
			
		else if (test_arg (argv[x], "-norefer",5)) 
			chase_flag = 1;
		else if (test_arg (argv[x], "-refer",3)) 
			chase_flag = 2;

		else if (test_arg(argv[x], "-strong", 3)) {
			char *new_version();
			struct certificate *cert_cpy();

			ca->ca_security = (struct security_parms *)
				calloc(1, sizeof(struct security_parms));
			ca->ca_security->sp_name = NULLDN;
			ca->ca_security->sp_time = new_version();
			ca->ca_security->sp_target = '\0';
			if (my_certificate != (struct certificate *) 0) {
			  ca->ca_security->sp_path =
				(struct certificate_list *) 
				   calloc(1, sizeof(struct certificate_list));
			  if (ca->ca_security->sp_path != 
				(struct certificate_list *) 0) {
			    ca->ca_security->sp_path->next =
				   (struct certificate_list *) 0;
			    ca->ca_security->sp_path->superior =
				   (struct certificate_list *) 0;
			    ca->ca_security->sp_path->cert =
				   cert_cpy(my_certificate);
			    ca->ca_security->sp_path->reverse =
				   (struct certificate *) 0;
			    }
			  }
			}

		else if (test_arg (argv[x], "-sequence",3)) {
			if (x + 1 == argc) {	
				ps_printf (opt, "We need a sequence name.\n");
				return (-1);
			} else {
				if (shuffle)
					shuffle_up (argc--, argv, x);
				else
					x++;
				if (lexequ (argv[x],"result") == 0) {
					if (x + 1 == argc) {	
						ps_printf (opt, "We need a result sequence name.\n");
						return (-1);
					}
					if (shuffle)
						shuffle_up (argc--, argv, x);
					else
						x++;
					result_sequence = strdup (argv[x]);
				} else
					set_sequence (argv[x]);
			}
		} else if (test_arg (argv[x], "-nosequence",5)) 
			unset_sequence();

		else if (do_shuffle)
			shuffle = FALSE;
		else {
			ps_printf (opt,"unknown service option %s\n",argv[x]);
			return (-1);
		}

		if (shuffle) 
			shuffle_up (argc--, argv, x--);
	}

	return (argc);
}

shuffle_up (argc, argv, start)
register int    argc;
char          **argv;
register int    start;
{
	register int    x;

	for (x = start; x < argc; x++)
		if (x == argc - 1)	/* if it is the last one, then stick
					 * a 0 in */
			argv[x] = 0;
		else		/* oterwise put the next one in it's place. */
			argv[x] = argv[x + 1];
}

new_service (ptr)
char * ptr;
{

	if (ptr != 0) {
		if (*default_service != 0)
			(void) strcat (default_service," ");
		(void) strcat (default_service,ptr);
	}
}

set_sequence (str)
char * str;
{
struct dua_sequence *ptr;

	if (lexequ (str,"reset") == 0) {
                if (current_sequence == NULL_DS)
                        return;
                current_sequence->ds_data = NULL_DE;
                current_sequence->ds_last = NULL_DE;
		return;
	}

	for (ptr = top_sequence; ptr != NULL_DS; ptr=ptr->ds_next)
		if (lexequ (ptr->ds_name,str) == 0) {
			current_sequence = ptr;
			return;
		}

	ptr = ds_alloc();
	ptr->ds_name = strdup (str);
	ptr->ds_data = NULL_DE;
	ptr->ds_last = NULL_DE;
	ptr->ds_next = top_sequence;
	top_sequence = ptr;
	current_sequence = ptr;
}

unset_sequence ()
{
	current_sequence = NULL_DS;
}

add_sequence (adn)
DN adn;
{
struct dua_seq_entry * ptr;
register int x=1;

	if (current_sequence == NULL_DS)
		return (0);
	
	for (ptr=current_sequence->ds_data; ptr != NULL_DE; ptr=ptr->de_next,x++) 
		if (dn_cmp (adn,ptr->de_name) == 0)
			return (x);

	ptr = de_alloc();
	ptr->de_name = dn_cpy (adn);
	ptr->de_next = NULL_DE;
	if (current_sequence->ds_data == NULL_DE) 
		current_sequence->ds_data = ptr;
	else
		current_sequence->ds_last->de_next = ptr;
	current_sequence->ds_last = ptr;
		
	return (x);
}

DN sequence_dn(y)
int y;
{
struct dua_seq_entry * ptr;
register int x = 1;

	if (current_sequence == NULL_DS)
		return (NULLDN);

	for (ptr=current_sequence->ds_data; 
		(ptr != NULL_DE) && (x<y); 
		ptr=ptr->de_next,x++) 
			;		

	if (ptr == NULL_DE)
		return (NULLDN);
	if ( x == y )
		return (ptr->de_name);
	return (NULLDN);
			
}


show_sequence (RPS,str,ufn)
PS RPS;
char * str;
char	ufn;
{
struct dua_seq_entry * ptr;
register int x = 1;

	if (str != NULLCP)
		set_sequence (str);

	if (current_sequence == NULL_DS) {
		ps_print (RPS,"No sequence set!\n");
		return;
	}

	if (strcmp (current_sequence -> ds_name, "default"))
	    ps_printf (RPS,"Sequence %s contains:-\n",
		       current_sequence->ds_name);

	for (ptr=current_sequence->ds_data; ptr != NULL_DE; ptr=ptr->de_next, x++) {
		ps_printf (RPS,"%-3d%s",x,ufn ? " " : " @");
		if (ufn)
		    (void) ufn_dn_print_aux (RPS, ptr -> de_name, NULLDN, 0);
		else
		    dn_print (RPS,ptr->de_name,EDBOUT);
		ps_print (RPS,"\n");
	}
}

