/* dishhelp.c - */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/dish/RCS/dishhelp.c,v 7.6 91/02/22 09:40:27 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/quipu/dish/RCS/dishhelp.c,v 7.6 91/02/22 09:40:27 mrose Interim $
 *
 *
 * $Log:	dishhelp.c,v $
 * Revision 7.6  91/02/22  09:40:27  mrose
 * Interim 6.8
 * 
 * Revision 7.5  90/10/17  11:55:17  mrose
 * sync
 * 
 * Revision 7.4  90/07/09  14:47:05  mrose
 * sync
 * 
 * Revision 7.3  90/04/18  08:49:34  mrose
 * 6.2
 * 
 * Revision 7.2  90/03/15  11:18:21  mrose
 * quipu-sync
 * 
 * Revision 7.1  90/01/11  18:37:37  mrose
 * real-sync
 * 
 * Revision 7.0  89/11/23  22:20:03  mrose
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
#include "quipu/name.h"

#define	OPT	(!frompipe || rps -> ps_byteno == 0 ? opt : rps)
#define	RPS	(!frompipe || opt -> ps_byteno == 0 ? rps : opt)
extern	char	frompipe;
extern	PS	opt, rps;
#define MAXARG 50

struct {
	char           *command;
	char           *args;
	char            serv;
	char	        other;
	char 	       *use;
	
} help_info[MAXARG];
int num_help = 0;

add_dish_help (command,args,serv,other,use)
char           *command;
char           *args;
char            serv;
char	        other;
char 	       *use;
{
	help_info[num_help].command = command;
	help_info[num_help].args = args;
	help_info[num_help].serv = serv;
	help_info[num_help].other = other;
	help_info[num_help].use = use;
	num_help++;
}

dish_help_init () {
	add_dish_help (
	"dish",		"[-pipe] [-noconnect] [-user <name>]\n[-password [<password>]] [-call <dsa name>] [-fast]\n[-simple] [-protected] [-strong] [-noauthentication]", FALSE, FALSE,
			"Directory Shell," );
	add_dish_help (
	"showentry", 	"[-[no]cache] [-[no]name] [-[no]move]", TRUE,TRUE,
			"show an entry, read it if not cached," );
	add_dish_help (
	"list", 	"[-nocache] [-noshow] [-[no]move]", TRUE,FALSE,
			"list children of the current node" );
	add_dish_help (
	"search", 	"[-baseobject] [-singlelevel] [-subtree]\n[-filter <filter>]\n[-[no]relative] [-[no]searchaliases] [-[no]partial] [-hitone]\n[-fred [-expand] [-full] [-summary] [-nofredseq] [-subdisplay]]", TRUE, TRUE,
			"search the tree for an object," );
	add_dish_help (
	"moveto", 	"[-[no]pwd] [-[no]check] [-sequence <name>] [-nosequence] <position>", FALSE, FALSE,
			"move to position in the DIT" );
	add_dish_help (
	"modify", 	"[-draft <draft> [-noedit]] [-newdraft]\n[-add <attr type>=<attr value>] [-remove <attr type>=<attr value>] ", TRUE, FALSE,
			"modify an existing node," );
	add_dish_help (
	"showname", 	"[-[no]compact] [-[no]ufn] [-[no]cache]", TRUE,TRUE,
			"show the name of an entry," );
	add_dish_help (
	"compare", 	"-attribute <attributeType '=' attributeValue>\n[-[no]print]", TRUE,FALSE,
			"compare attribute with the supplied value," );
	add_dish_help (
	"add", 		"[-draft <draft> [-noedit]] [-template <draft>] [-newdraft]\n [-objectclass <objectclass>]", TRUE,FALSE,
			"add a new node," );
	add_dish_help (
	"delete", 	"", TRUE,FALSE,
			"delete an object," );
	add_dish_help (
	"modifyrdn", 	"-name <attributeType '=' attributeValue> [-[no]delete]", TRUE, FALSE,
			"modify the name of a node," );
	add_dish_help (
	"squid",	"[-sequence <name>] [-alias <position>] [-version]\n[-user] [-syntax] [-fred]",FALSE, FALSE,
			"status of dish," );
	add_dish_help (
	"bind",		"[-noconnect] [[-user] <name>]\n[-password [<password>]] [-[no]refer]\n[-call <dsa name>]\n[-simple] [-protected] [-strong] [-noauthentication]", FALSE, FALSE,
			"connect to the directory," );
	add_dish_help (
	"unbind", 	"[-noquit]", FALSE, FALSE,
			"disconnect from the directory," );
	add_dish_help (
	"fred",		"[-display <name>]\n[-dm2dn [-list] [-phone] [-photo] <domain-or-mailbox>]\n[-expand [-full] <DN>]\n[-ufn [-list,][-mailbox,][-phone,][-photo,]<name...>]\n[-ufnrc <list...>]", FALSE, FALSE,
 			"back-end to fred," );
	add_dish_help (
	"dsacontrol", 	"[-[un]lock <entry>] [-dump <directory>]\n[-tailor <string>] [-abort] [-restart] [-info]\n[-refresh <entry>] [-resync <entry>] [-slave [<entry>]]", FALSE, FALSE,
			"control the operation of the DSA (managers only)," );
};

Usage (rtn)
char           *rtn;
{
	extern DN       dn,
	                savename;
	int             i;

	dn_free (dn);
	dn = savename;
	savename = NULLDN;

	if (print_arg_error (OPT) == OK)
		return;

	for (i = 0; help_info[i].command != 0; i++)
		if (strcmp (help_info[i].command, rtn) == 0) {
			if (help_info[i].serv) {
				ps_printf (OPT, "Usage %s [-help] [<object>] %s \n", rtn, help_info[i].args);
				print_other(OPT, help_info[i].other);
				ps_printf (OPT, "\n[<service controls>]\n");
			} else
				ps_printf (OPT, "Usage %s [-help] %s\n", rtn, help_info[i].args);
			return;
		}
	ps_print (OPT, "Usage...\n");
}

help_arg (rtn)
char           *rtn;
{
	int             i;

	for (i = 0; help_info[i].command != 0; i++)
		if (strcmp (help_info[i].command, rtn) == 0) {
			if (help_info[i].serv) {
				ps_printf (RPS, "%-10s - %s\n[<object>] %s ", rtn, help_info[i].use,help_info[i].args);
				print_other(RPS,help_info[i].other);
				ps_print (RPS,"\n");
				print_service();
			} else
				ps_printf (RPS, "%-10s - %s\n%s\n", rtn, help_info[i].use,help_info[i].args);
			return;
		}
	ps_print (OPT,"Sorry - No help available\n");
}

print_other (aps,x) 
PS aps;
char x;
{
	if (x == FALSE)
		return;
		
	ps_print (aps,"\n[-[no]types <attribute-type> *] [-[no]all]\n[-[no]value] [-[no]show] \n[-[no]key] [-edb]\n[-proc <syntax> <process>]");
}

print_service ()
{
	ps_print (RPS,"[-sequence <name>] [-nosequence]\n");
	ps_print (RPS,"[-[no]preferchain] [-[no]chaining]\n");
	ps_print (RPS,"[-[dont]usecopy] [-[dont]dereferencealias]\n");
	ps_print (RPS,"[-low] [-medium] [-high]\n");
	ps_print (RPS,"[-timelimit n] [-notimelimit]\n");
	ps_print (RPS,"[-sizelimit n] [-nosizelimit]\n");
	ps_print (RPS,"[-strong] [-[no]refer]\n");
	ps_print (RPS,"[-[no]localscope] [-help]\n");

}

call_help ()
{
	int             i;

	ps_print (RPS, "The following commands are recognised...\n\n");

	for (i = 0; help_info[i].command != 0; i++)
		ps_printf (RPS, "%-10s - %s\n", help_info[i].command, help_info[i].use);

	ps_print (RPS,"\nEnter <command> -help for help on that command\n");
	ps_print (RPS, "See the DISH manual for full details\n\n");
}
