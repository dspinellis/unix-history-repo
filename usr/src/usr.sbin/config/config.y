%token CPU IDENT CONFIG ANY DEVICE UBA MBA NEXUS CSR DRIVE VECTOR
%token CONTROLLER PSEUDO_DEVICE FLAGS ID SEMICOLON NUMBER OPTIONS TRACE
%token DISK SLAVE AT
%{
/*	config.y	1.3	81/02/26	*/
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
	CPU Save_id NUMBER = {
		    cpu_type = ns(sprintf(errbuf, "%s%d", $2, $3));
		    free(temp_id);
		    } |
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
	= { init_dev(&cur); }
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

struct device *connect(dev, num)
register char *dev;
register int num;
{
	register struct device *dp;
	struct device *huhcon();

	if (num == -1)
	    return huhcon(dev);
	for (dp = dtab; dp != NULL; dp = dp->d_next)
		if ((num == dp->d_unit || num == -1)
		    && eq(dev, dp->d_name))
			return dp;
	yyerror(sprintf(errbuf, "%s %d not defined", dev, num));
}

/*
 * huhcon
 *	Connect to an unspecific thing
 */

struct device *huhcon(dev)
register char *dev;
{
    register struct device *dp, *dcp;
    struct device rdev;

    /*
     * First make certain that there are some of these to wildcard on
     */
    for (dp = dtab; dp != NULL; dp = dp->d_next)
	if (eq(dp->d_name, dev))
	    break;
    if (dp == NULL)
    {
	yyerror(sprintf(errbuf, "no %s's to wildcard", dev));
	return NULL;
    }
    dcp = dp->d_conn;
    /*
     * Now see if there is already a wildcard entry for this device
     */
    for (; dp != NULL; dp = dp->d_next)
	if (eq(dev, dp->d_name) && dp->d_unit == -1)
	    break;
    /*
     * If there isn't, make one
     */
    if (dp == NULL)
    {
	dp = &rdev;
	init_dev(dp);
	dp->d_unit = -1;
	dp->d_name = ns(dev);
	newdev(dp);
	dp = dtab;
	/*
	 * Connect it to the same thing that other similar things are
	 * connected to, but make sure it is a wildcard unit
	 */
	if (dcp == -1 || dcp == NULL)
	    dp->d_conn = dcp;
	else
	    dp->d_conn = connect(dcp->d_name, -1);
    }
    return dp;
}

init_dev(dp)
register struct device *dp;
{
    dp->d_name = "OHNO!!!";
    dp->d_type = DEVICE;
    dp->d_conn = NULL;
    dp->d_vec1 = dp->d_vec2 = NULL;
    dp->d_addr = dp->d_flags = dp->d_dk = 0;
    dp->d_slave = -1;
    dp->d_drive = dp->d_unit = -17;
}
