%token CPU IDENT CONFIG ANY DEVICE UBA MBA NEXUS CSR DRIVE VECTOR
%token CONTROLLER PSEUDO_DEVICE FLAGS ID SEMICOLON NUMBER OPTIONS TRACE
%token DISK SLAVE AT
%{
/*	config.y	1.1	81/02/24	*/
#include "config.h"
#include <stdio.h>
	struct device cur;
	char *temp_id;
%}
%%
Configuration:
	Many_specs
	;

Many_specs:
	Many_specs Spec
	|
	;

Spec:
	Device_spec SEMICOLON  = { newdev(&cur); } |
	Config_spec SEMICOLON |
	TRACE SEMICOLON = { do_trace = ! do_trace; } |
	SEMICOLON |
	error SEMICOLON
	;

Config_spec:
	CPU ID NUMBER = { cpu_type = ns(sprintf(errbuf, "%s%d", $2, $3)); } |
	IDENT ID { ident = ns($2); } |
	CONFIG Save_id ID = { mkconf(temp_id, $3); free(temp_id); }
	;

Save_id:
	ID = { $$ = temp_id = ns($1); }
	;

Dev:
	UBA  = { $$ = ns("uba"); } |
	MBA  = { $$ = ns("mba"); } |
	ID = { $$ = ns($1); }
	;

Device_spec:
	DEVICE Dev_name Dev_info Int_spec = {  cur.d_type = DEVICE; } |
	DISK Dev_name Dev_info Int_spec =
				{  cur.d_dk = 1; cur.d_type = DEVICE; } |
	CONTROLLER Dev_name Dev_info Int_spec = {  cur.d_type = CONTROLLER; } |
	PSEUDO_DEVICE Dev_name = { cur.d_type = PSEUDO_DEVICE ; }
	;

Dev_name:
	Init_dev Dev NUMBER =	{
			cur.d_name = $2;
			if (eq($2, "mba"))
			    seen_mba = TRUE;
			else if (eq($2, "uba"))
			    seen_uba = TRUE;
			cur.d_unit = $3;
		}
	;

Init_dev:
	= {
		cur.d_name = "OHNO!!!";
		cur.d_type = DEVICE;
		cur.d_conn = NULL;
		cur.d_vec1 = cur.d_vec2 = NULL;
		cur.d_addr = cur.d_flags = cur.d_dk = 0;
		cur.d_slave = cur.d_drive = cur.d_unit = -1;
	}
	;

Dev_info:
	Con_info Info_list
	|
	;

Con_info:
	AT Dev NUMBER = { cur.d_conn = connect($2, $3); } |
	AT NEXUS NUMBER = { cur.d_conn = -1; }
	;
    
Info_list:
	Info_list Info
	|
	;

Info:
	CSR NUMBER = { cur.d_addr = $2; } |
	DRIVE NUMBER = { cur.d_drive = $2; } |
	SLAVE NUMBER = { cur.d_slave = $2; } |
	FLAGS NUMBER = { cur.d_flags = $2; }
	;

Int_spec:
	VECTOR Save_id = { cur.d_vec1 = $2; } |
	VECTOR Save_id ID = { cur.d_vec1 = $2; cur.d_vec2 = ns($3); } |
	;
%%

yyerror(s)
char *s;
{
	fprintf(stderr, "config: %s at line %d\n", s, yyline);
}

/*
 * ns:
 *	Return the passed string in a new space
 */

char *
ns(str)
register char *str;
{
	register char *cp;

	cp = malloc(strlen(str)+1);
	strcpy(cp, str);
	return cp;
}

/*
 * newdev
 *	Add a device to the list
 */

newdev(dp)
register struct device *dp;
{
	register struct device *np;

	np = (struct device *) malloc(sizeof *np);
	*np = *dp;
	np->d_next = dtab;
	dtab = np;
}

/*
 * mkconf
 *	Note that a configuration should be made
 */

mkconf(dev, sysname)
char *dev, *sysname;
{
	register struct file_list *fl;

	fl = (struct file_list *) malloc(sizeof *fl);
	fl->f_fn = ns(dev);
	fl->f_needs = ns(sysname);
	if (confp == NULL)
	    conf_list = fl;
	else
	    confp->f_next = fl;
	confp = fl;
}

/*
 * Connect:
 *	Find the pointer to connect to the given device and number.
 *	returns NULL if no such device and prints an error message
 */

struct device *
connect(dev, num)
register char *dev;
register int num;
{
	register struct device *dp;

	for (dp = dtab; dp != NULL; dp = dp->d_next)
		if ((num == dp->d_unit || num == -1)
		    && eq(dev, dp->d_name))
			return dp;
	yyerror(sprintf(errbuf, "%s %d not defined", dev, num));
}
