/* A 'C' version of the shellscript dishinit 
 * By Steve Titcombe
 *
 * Most of this has fixed calls to other functions, and will require going 
 * through again to strip out all unnecessary error trapping, etc.
 * (Utterly Horrible Hack.)
 */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/uips/dish/RCS/quipurc.c,v 7.5 91/02/22 09:30:27 mrose Interim $";
#endif

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */

#include <fcntl.h>
#include "manifest.h"
#include <sys/stat.h>
#include <signal.h>
#include <pwd.h>
#include "quipu/read.h"
#include "quipu/sequence.h"
#include "quipu/name.h"
#include "quipu/bind.h"
#include "quipu/dsp.h"
#include "quipu/ds_error.h"
#include "tailor.h"
#include "quipu/util.h"
#include "quipu/dua.h"
#include "quipu/ds_search.h"
#include "quipu/list.h"
#include "quipu/entry.h"
#include "quipu/modify.h"

#define ORG_PERSON "thornPerson & quipuObject"
	/* this should probably go elsewhere !!! */

	LLog    *log_dua;
	DN	sequence_dn() ;
	DN      dn, moddn ;
	DN	fixed_pos = NULLDN ;
	PS      opt;
	PS      rps;
	PS	fileps ;

	FILE	*fp_quipurc ;
	FILE	*fp_draft ;
	FILE	*fp_tailor ;

	Filter	get_filter ();
struct	entrymod	*ems_append() ;
struct	entrymod	*modify_avs() ;

static	struct	ds_bind_arg	bindarg ;
static	struct	ds_bind_arg	bindresult ;
static	struct	ds_bind_error	binderr ;

	struct	ds_read_arg	read_arg;
	struct	DSError		read_error;

	struct	ds_read_result	read_result;
	struct	ds_modifyentry_arg	mod_arg ;
	struct	DSError			mod_error ;

	struct	DSError			search_error;
	struct	ds_search_result	search_result;
static	struct	ds_search_arg search_arg =
	{
		default_common_args,
		NULLDN,
		SRA_ONELEVEL,
		NULLFILTER, /* filter */
		FALSE,
		{       /* eis */
			FALSE,
			NULLATTR,
			EIS_ATTRIBUTESANDVALUES
		}
	};

static	struct	dua_sequence	*current_sequence = NULL_DS;
	struct	entrymod	*emnew ;

AV_Sequence	avst = NULLAV ;
Attr_Sequence	as_flag = NULLATTR ;
Attr_Sequence	trail = NULLATTR ;
Attr_Sequence	eptr ;
Attr_Sequence	temp ;
Attr_Sequence	as ;
AttributeType	at;

#ifdef TURBO_DISK
extern	Attr_Sequence	fget_attributes() ;
#else
extern	Attr_Sequence	get_attributes() ;
#endif
extern	char		*TidyString() ;

	Entry	current_entry ;
	Entry	entry_ptr ;

	char	Manager[LINESIZE] ;
	char	Password[LINESIZE] ;
	char	Local[LINESIZE] ;
	char	filterstring[LINESIZE] ;	

main()
{
	struct  passwd  *pw_entry ;
	struct  passwd  *getpwuid() ;
	struct	stat	buf ;

	int	i = 1 ;
	int     uid ;
	int	um ;
	char	pass1[LINESIZE] ;
	char	pass2[LINESIZE] ;
	char	Read_in_Stuff[LINESIZE] ;
	char	**vecptr ;
	char 	*tmpdraft ;
	char	home_dir[LINESIZE] ;
	char	*p, *part1, *part2 ;
	char	quipurc_file[100] ;
	char	tailor_file[100] ;
	char	user_name[9] ;
	char	*localptr = Local ;
	char	print_format = EDBOUT ;
	EntryInfo	*ptr ;
	static  CommonArgs ca = default_common_args;
	
	vecptr = (char **) malloc(100) ;
	vecptr[0] = malloc (LINESIZE) ;
	(void) strcpy(vecptr[0], "showentry") ;
	(void) strcpy(pass1, "x") ;
	(void) strcpy(pass2, "y") ;
	tmpdraft = malloc (LINESIZE) ;
	(void) strcpy(tmpdraft, "/tmp/dish-") ;

	if ((opt = ps_alloc (std_open)) == NULLPS)
		fatal (-62, "ps_alloc failed");
	if (std_setup (opt, stderr) == NOTOK)
		fatal (-63, "std_setup failed");
	if ((rps = ps_alloc (std_open)) == NULLPS)
		fatal (-64, "ps_alloc 2 failed");
	if (std_setup (rps, stdout) == NOTOK)
		fatal (-65, "std_setup 2 failed");
	(void) strcpy(filterstring, "userid=") ;

	/* Sort out files, userids etc. */
	uid=getuid() ;
	if ((pw_entry=getpwuid(uid)) == 0)
	{
		ps_printf(rps, "Who are you? (no name for your uid number)\n") ;
		exit(1) ;
	}
	(void) strcpy(user_name, pw_entry->pw_name) ;
	(void) strcat(tmpdraft, user_name) ;

	if (getenv("HOME") == 0) 
	{
		ps_printf(rps, "No home directory?!!") ;
		(void) strcpy(home_dir, pw_entry->pw_dir) ;
	}
	else
	{
		(void) strcpy(home_dir, getenv("HOME")) ;
	}

	(void) strcpy(quipurc_file, home_dir) ;
	(void) strcat(quipurc_file, "/.quipurc") ;

	(void) strcpy(tailor_file, isodefile ("dishinit", 1));

	Manager[0] = 0;
	Password[0] = 0;
	Local[0] = 0;

	(void) stat(tailor_file, &buf) ;
	(void) seteuid(buf.st_uid) ;	/* set effective to enable */
					/* us to read protected file */

	if ((fp_tailor = fopen(tailor_file, "r")) == 0)
	{
		ps_print(rps, "Can't open Tailor File. Abort.\n") ;
		exit(1) ;
	}

	while (fgets (Read_in_Stuff, LINESIZE, fp_tailor) != 0)
	{
		if (!strcmp(Read_in_Stuff, "##Anything after this line is copied into the users ~/.quipurc file\n"))
		{
			break ;
		}

		p = SkipSpace (Read_in_Stuff);
		if (( *p == '#') || (*p == '\0'))
			continue;  		/* ignore comments and blanks */

		part1 = p;
		if ((part2 = index (p,':')) == NULLCP) {
			ps_printf (opt,"Seperator missing '%s'. Ignoring..\n",p);
		}

		*part2++ = '\0';
		part2 = TidyString (part2);

		if (lexequ(part1, "manager") == 0)
		{
			(void) strcpy(Manager, part2) ;
		}
		else
		if (lexequ(part1, "password") == 0)
		{
			(void) strcpy(Password, part2) ;
		}
		else
		if (lexequ(part1, "local") == 0)
		{
			(void) strcpy(Local, part2) ;
		}
		else
		{
			ps_printf(rps, "Error in tailor. What's a %s?\n", part1) ;
		}

	}
	(void) setuid(uid) ;			/* Restore Userid to original user. */

/* create ~/.quipurc file. NB this does eradicate anything in there.
 * 			   (Theoretically nothing.) 
 */

	if (Manager[0] == 0) {
		ps_print(rps, "Can't find out the managers name\n") ;
		exit(1) ;
	}
	if (Password[0] == 0) {
		ps_print(rps, "Can't find out the managers password\n") ;
		exit(1) ;
	}
	if (Local[0] == 0) {
		ps_print(rps, "Can't find out where to search\n") ;
		exit(1) ;
	}

	um = umask(0177) ;
	if ((fp_quipurc = fopen(quipurc_file, "w")) == 0)
	{
		ps_printf(rps, "Can't open ~/.quipurc. Aborting..\n") ;
		exit(1) ;
	}
	(void) umask(um) ;	

	if ((fileps = ps_alloc(std_open)) == NULLPS)
	{
		fatal (-66, "ps_alloc 2 failed");
	}
	if (std_setup (fileps, fp_quipurc) == NOTOK)
	{
		fatal (-67, "std_setup 2 failed");
	}


	/* Sorting out the bind section */
	quipu_syntaxes() ;		/* set up the needed function pointers */
	dsap_init(&i, &vecptr) ;

	(void) strcpy(bindarg.dba_passwd, Password) ;
	bindarg.dba_version = DBA_VERSION_V1988;
	bindarg.dba_passwd_len = strlen(bindarg.dba_passwd) ;

	if ((bindarg.dba_dn = str2dn (Manager)) == NULLDN) 
	{
		ps_printf (opt,"Invalid Manager name %s (???!)\n",Manager) ;
		exit(1) ;
	}

	if (ds_bind (&bindarg, &binderr, &bindresult) != OK)
	{
		ps_printf(rps, "Can't bind as the manager.\n") ;
		exit(1);
	}
	/* Hopefully, should be successfully bound */

/*
 * We now call the search stuff with the right bits, to see if we can get a
 * match of uid='user_name'. Once there, we echo lots of information from
 * their entry out to the .quipurc file.
 * Hopefully there should only be one match. This assumes that ALL dir info
 * up to date, and that SG do not allow multiple users with the same login.
 */

/* set up the appropriate structures and defaults. */

	search_arg.sra_common = ca; /* struct copy */
	search_arg.sra_common.ca_servicecontrol.svc_sizelimit = 2 ;
	search_arg.sra_eis.eis_allattributes = FALSE ;
	search_arg.sra_searchaliases = FALSE;
	search_arg.sra_subset = SRA_ONELEVEL;
	search_arg.sra_eis.eis_infotypes = EIS_ATTRIBUTESANDVALUES ;
	search_arg.sra_eis.eis_select = NULLATTR ;
	search_arg.sra_eis.eis_allattributes = TRUE ;
	search_arg.sra_filter = filter_alloc() ;
		/* Default filter. */
		search_arg.sra_filter->flt_next = NULLFILTER;
		search_arg.sra_filter->flt_type = FILTER_ITEM;
		search_arg.sra_filter->FUFILT = NULLFILTER;
		

	if (*localptr == '@')
	{
		localptr++;
	}
	if ((search_arg.sra_baseobject = str2dn(localptr)) == NULLDN)
	{
		ps_printf (opt,"Invalid sequence in username %s.\n", localptr);
		exit(1) ;
	}

	(void) strcat(filterstring, user_name) ;

	search_arg.sra_filter->flt_un.flt_un_item.fi_type = FILTERITEM_EQUALITY ;

	if ((search_arg.sra_filter->flt_un.flt_un_item.fi_un.fi_un_ava.ava_type = AttrT_new ("userid")) == NULLAttrT)
	{
		ps_printf(rps, "Oops, userid is not a valid attr type. ABORT!!\n") ;
		exit(1) ;
	}
	if ((search_arg.sra_filter->flt_un.flt_un_item.fi_un.fi_un_ava.ava_value = str2AttrV (user_name, search_arg.sra_filter->flt_un.flt_un_item.fi_un.fi_un_ava.ava_type->oa_syntax)) == NULLAttrV)
	{
		ps_printf(rps, "%s is not a valid attribute value.\n", user_name) ;
	}

/* call search */
/* We now ought to be in the right place, and with the search stuff set,
 * ready to call search, and receive one (or no) entry back, which then 
 * gets processed accordingly.
 */

	if (ds_search (&search_arg, &search_error, &search_result) != DS_OK)
	{
		ps_printf(rps, "Search failed...\n") ;
		exit (1) ;
		/* This is not the same as coming back with */
		/* message "search failed to find anything. */
	}

/* If the user does not exist in the DIT, print out the limited .quipurc
 * and the warning message, and allow the user to play DISH.
 */

	if (search_result.CSR_entries == NULLENTRYINFO)
	{
		ps_printf(opt, "Unfortunately, you seem to have no entry in\n") ;
		ps_printf(opt, "the directory. Contact '%s' who should be able to help.\n", Manager) ;
		ps_printf(opt, "In the mean time, you can read, but not write.\n") ;
	}
	else
	{
		ptr = search_result.CSR_entries ;
		dn = dn_cpy(ptr->ent_dn) ;	/* Essence of move user_name. */

		/* collect the info and put it into current_entry */

		/* Set up the desired attribute type to be read*/
		/* from read.c */
		if ((at = AttrT_new ("userPassword")) != NULLAttrT) 
		{
			as_flag = as_merge (as_flag, as_comp_new (AttrT_cpy (at), NULLAV, NULLACL_INFO));
		}
		else
		{
			ps_printf(rps, "Oops, Serious error. unknown attribute type 'userPassword'.\n") ;
			exit(1) ;
		}

		if ((current_entry = local_find_entry (dn, FALSE)) == NULLENTRY)
		{	
			read_arg.rda_common = ca; /* struct copy */
			read_arg.rda_object = dn;
			read_arg.rda_eis.eis_infotypes = EIS_ATTRIBUTESANDVALUES;
			read_arg.rda_eis.eis_allattributes = TRUE ;
			read_arg.rda_eis.eis_select = NULLATTR ;

			if (ds_read (&read_arg, &read_error, &read_result) != DS_OK)
			{
				ps_printf(rps, "We even seem to be having problems reading\n" ) ;
				ps_printf(rps, "an entry we searched and found!! HELP!!\n") ;
				exit(1) ;
			}
			if (read_result.rdr_entry.ent_attr == NULLATTR)
			{
				ps_printf(rps, "No attributes present. Even though\n") ;
				ps_printf(rps, "we found you by userid attribute!!! HELP!!\n") ;
				exit (1) ;
			}
			cache_entry (&(read_result.rdr_entry), read_arg.rda_eis.eis_allattributes, TRUE) ;
		}

		if ((current_entry = local_find_entry (dn, FALSE)) == NULLENTRY)
		{
			ps_printf(rps, "We still have nothing.Even after reading? Abort.\n") ;
			exit(1) ;
		}

		ps_printf(fileps, "username: ") ;
		dn_print(fileps, dn, EDBOUT) ;
		ps_printf(fileps, "\n") ;

		ps_printf(fileps, "me: ") ;
		dn_print(fileps, dn, EDBOUT) ;
		ps_printf(fileps, "\n") ;

		/* now showattribute -nokey to display it. */

		ps_printf(fileps, "password: ") ;
		for (eptr = current_entry->e_attributes; eptr != NULLATTR;
							 eptr = eptr->attr_link) 
		{
			/* Tiptoe through the list of types until one matches, and then print value. */
			if (AttrT_cmp (eptr->attr_type, at) == 0) 
			{
				avs_print (fileps, eptr->attr_value,print_format);
				break;
			}
		}

		if (eptr == NULLATTR)
		{
			while( strcmp(pass1, pass2))
			{
				ps_printf(opt, "You need a password...\n") ;
				(void) strcpy(pass1, getpassword("Enter Password: ")) ;
				(void) strcpy(pass2, getpassword("Re-enter password: ")) ;
				if (strcmp(pass1, pass2))
				{
					ps_printf(opt, "\nMismatch - Try again.\n") ;
				}
			}
			ps_printf(fileps, "%s\n", pass1) ;

			um = umask(0177) ;
			if ((fp_draft = fopen(tmpdraft, "w")) == 0)
			{
				ps_print(rps, "Can't open draft file... Abort.\n") ;
				exit(1) ;
			}
			(void) umask(um) ;

			(void) fprintf(fp_draft, "UserPassword = %s\n", pass1) ;
		 	(void) fprintf(fp_draft, "acl = self # write # attributes # acl $ userPassword\n") ;
			(void) fprintf(fp_draft, "acl = others # compare # attributes # acl $ userPassword\n\n") ;
			(void) fclose(fp_draft) ;

			if ((fp_draft = fopen (tmpdraft, "r")) == NULL) {
				ps_printf (opt, "Can't open draft entry %s\n", tmpdraft);
				exit(1) ;
			}

			entry_ptr = get_default_entry (NULLENTRY);
#ifdef TURBO_DISK
			entry_ptr->e_attributes = fget_attributes (fp_draft);
#else
			entry_ptr->e_attributes = get_attributes (fp_draft);
#endif

			(void) fclose (fp_draft);

			mod_arg.mea_common = ca; /* struct copy */
			mod_arg.mea_object = dn;
			for (moddn = dn ; moddn->dn_parent != NULLDN; moddn=moddn->dn_parent)
				;
			entry_ptr->e_name = rdn_cpy (moddn->dn_rdn);
	
			/* add rdn as attribute */
			avst = avs_comp_new (AttrV_cpy (&entry_ptr->e_name->rdn_av));
			temp = as_comp_new (AttrT_cpy (entry_ptr->e_name->rdn_at), avst, NULLACL_INFO);
			entry_ptr->e_attributes = as_merge (entry_ptr->e_attributes, temp);

			for (as = entry_ptr->e_attributes; as != NULLATTR; as = as->attr_link)
			{
				emnew = NULLMOD;
				trail = as->attr_link;
				as->attr_link = NULLATTR;
				temp = current_entry->e_attributes;
				for (; temp != NULLATTR; temp = temp->attr_link) 
					if (AttrT_cmp (as->attr_type, temp->attr_type) == 0)
					{
						/* found it - does it need changing ? */
						if (avs_cmp (as->attr_value, temp->attr_value) != 0) 
							emnew = modify_avs (as->attr_value, temp->attr_value,as->attr_type);
						break;
					}

				if (temp == NULLATTR) 
				{
					emnew = em_alloc ();
					emnew->em_type = EM_ADDATTRIBUTE;
					emnew->em_what = as_cpy(as);
					emnew->em_next = NULLMOD;
				}
				if (emnew != NULLMOD)
				{
					mod_arg.mea_changes = ems_append (mod_arg.mea_changes,emnew);
				}
				as->attr_link = trail;
			}

			while (ds_modifyentry (&mod_arg, &mod_error) != DS_OK)
			{
				if (dish_error (opt, &mod_error) == 0)
				{
					ps_printf(rps,"We have a dish error. Bye.\n") ;
					entry_free (entry_ptr);
					exit(1) ;
				}
				mod_arg.mea_object = mod_error.ERR_REFERRAL.DSE_ref_candidates->cr_name;
			}
			ps_print (rps, "Modified ");
			dn_print (rps, dn, EDBOUT);
			ps_print (rps, "\n");
			delete_cache (dn);	/* re-cache when next read */

			entry_free (entry_ptr);
			ems_part_free (mod_arg.mea_changes);
		}
	}

	while(fgets(Read_in_Stuff, LINESIZE, fp_tailor) != 0)
	{
		fputs(Read_in_Stuff, fp_quipurc) ;
	}
		
	(void) fclose(fp_quipurc) ;	
	(void) fclose(fp_tailor) ;

/*	(void) fprintf(fp_quipurc, "dsap: local_dit \"%s\"\n", Local) ;
	(void) fprintf(fp_quipurc, "notype: acl\n") ;
	(void) fprintf(fp_quipurc, "notype: treestructure\n") ;
	(void) fprintf(fp_quipurc, "notype: masterdsa\n") ;
	(void) fprintf(fp_quipurc, "notype: slavedsa\n") ;
	(void) fprintf(fp_quipurc, "notype: objectclass\n") ;
	(void) fprintf(fp_quipurc, "cache_time: 30\n") ;
	(void) fprintf(fp_quipurc, "connect_time: 2\n") ;
 */
	(void) ds_unbind() ;
	(void) unlink(tmpdraft) ;
}

void
advise()
{
}

void
set_sequence()
{
}

void
unset_sequence()
{
}

dish_error (ps,error)
PS ps;
struct DSError * error;
{

	if (error->dse_type == DSE_ABANDONED) {
		ps_printf (ps,"(DAP call interrupted - abandon successful)\n");
		return (0);
	}

	if (error->dse_type == DSE_ABANDON_FAILED) {
		ps_printf (ps,"(DAP call interrupted - abandon unsuccessful)\n");
		return (0);
	}

	if (error->dse_type == DSE_INTRERROR) {
		ps_printf (ps,"(DAP call interrupted)\n");
		return (0);
	}

	ds_error (ps,error);

	return (0);
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

struct entrymod * ems_append (a,b)
struct entrymod *a;
struct entrymod *b;
{
struct entrymod *ptr;

	if ((ptr = a) == NULLMOD)
		return b;

	for ( ; ptr->em_next != NULLMOD; ptr = ptr->em_next)
		;

	ptr->em_next = b;
	return a;
}

struct entrymod * modify_avs (a,b,ent_mod_at)
AV_Sequence a;
AV_Sequence b;
AttributeType ent_mod_at;
{
AV_Sequence x;
AV_Sequence y;
struct entrymod *em = NULLMOD, *em_new;
int removed_all = TRUE;

	for (x=b; x != NULLAV; x=x->avseq_next) {
		em_new = NULLMOD;
		for (y=a; y != NULLAV; y=y->avseq_next) 
			if (AttrV_cmp (&x->avseq_av,&y->avseq_av) == 0)
				break;
		if (y == NULLAV) {
			em_new = em_alloc ();
			em_new->em_type = EM_REMOVEVALUES;
			em_new->em_what = as_comp_new (ent_mod_at,avs_comp_new(&x->avseq_av),NULLACL_INFO);
			em_new->em_next = NULLMOD;
		} else
			removed_all = FALSE;
		if (em_new != NULLMOD)
			em = ems_append (em,em_new);
	}

	if (removed_all) {
		ems_part_free (em);
		em_new = em_alloc ();
		em_new->em_type = EM_REMOVEATTRIBUTE;
		em_new->em_what = as_comp_new (ent_mod_at,b,NULLACL_INFO);
		em_new->em_next = em_alloc();
		em_new->em_next->em_type = EM_ADDATTRIBUTE;
		em_new->em_next->em_what = as_comp_new (ent_mod_at,avs_cpy(a),NULLACL_INFO);
		em_new->em_next->em_next = NULLMOD;
		return (em_new);
	}

	for (x=a; x != NULLAV; x=x->avseq_next) {
		em_new = NULLMOD;
		for (y=b; y != NULLAV; y=y->avseq_next) 
			if (AttrV_cmp (&x->avseq_av,&y->avseq_av) == 0)
				break;
		if (y == NULLAV) {
			em_new = em_alloc ();
			em_new->em_type = EM_ADDVALUES;
			em_new->em_what = as_comp_new (ent_mod_at,avs_comp_new(&x->avseq_av),NULLACL_INFO);
			em_new->em_next = NULLMOD;
		}
		if (em_new != NULLMOD)
			em = ems_append (em,em_new);
	}

		
	return (em);
}

ems_part_free(emp)
struct entrymod *emp;
{
	if(emp == NULLMOD)
		return;
	ems_part_free(emp->em_next);
	free((char *)emp);
}

