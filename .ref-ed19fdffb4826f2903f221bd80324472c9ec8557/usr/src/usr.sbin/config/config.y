%token CPU IDENT CONFIG ANY DEVICE UBA MBA NEXUS CSR DRIVE VECTOR OPTIONS
%token CONTROLLER PSEUDO_DEVICE FLAGS ID SEMICOLON NUMBER FPNUMBER TRACE
%token DISK SLAVE AT HZ TIMEZONE DST MAXUSERS MASTER COMMA MINUS
%{
/*	config.y	1.11	81/05/22	*/
#include "config.h"
#include <stdio.h>
	struct device cur;
	struct device *curp = NULL;
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
	CPU Save_id = {
		    struct cputype *cp = malloc(sizeof (struct cputype));
		    cp->cpu_name = ns($2);
		    cp->cpu_next = cputype;
		    cputype = cp;
		    free(temp_id);
		    } |
	OPTIONS Opt_list |
	IDENT ID { ident = ns($2); } |
	CONFIG Save_id ID = { mkconf(temp_id, $3); free(temp_id); } |
	HZ NUMBER = {
		yyerror("HZ specification obsolete; delete");
		hz = 60;
		} |
	TIMEZONE NUMBER = { timezone = 60 * $2; check_tz(); } |
	TIMEZONE NUMBER DST = { timezone = 60 * $2; dst = 1; check_tz(); } |
	TIMEZONE FPNUMBER = { timezone = $2; check_tz(); } |
	TIMEZONE FPNUMBER DST = { timezone = $2; dst = 1; check_tz(); } |
	MINUS TIMEZONE NUMBER =
	    { timezone = -60 * $3; check_tz(); } |
	MINUS TIMEZONE NUMBER DST =
	    { timezone = -60 * $3; dst = 1; check_tz(); } |
	MINUS TIMEZONE FPNUMBER =
	    { timezone = -$3; check_tz(); } |
	MINUS TIMEZONE FPNUMBER DST =
	    { timezone = -$3; dst = 1; check_tz(); } |
	MAXUSERS NUMBER = { maxusers = $2; }
	;

Opt_list:
	Opt_list COMMA Option |
	Option
	;

Option:
	Save_id = {
		    struct opt *op = malloc(sizeof (struct opt));
		    op->op_name = ns($1);
		    op->op_next = opt;
		    opt = op;
		    free(temp_id);
	}
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
	MASTER Dev_name Dev_info Int_spec = {  cur.d_type = MASTER; } |
	DISK Dev_name Dev_info Int_spec =
				{  cur.d_dk = 1; cur.d_type = DEVICE; } |
	CONTROLLER Dev_name Dev_info Int_spec = {  cur.d_type = CONTROLLER; } |
	PSEUDO_DEVICE Init_dev Dev =
			{ cur.d_name = $3; cur.d_type = PSEUDO_DEVICE; }
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
	AT Dev NUMBER = {
		if (eq(cur.d_name, "mba") || eq(cur.d_name, "uba"))
			yyerror(sprintf(errbuf,
				"%s must be connected to a nexus", cur.d_name));
		cur.d_conn = connect($2, $3);
	} |
	AT NEXUS NUMBER = { check_nexus(&cur, $3); cur.d_conn = TO_NEXUS; }
	;
    
Info_list:
	Info_list Info
	|
	;

Info:
	CSR NUMBER = { cur.d_addr = $2; } |
	DRIVE NUMBER = { cur.d_drive = $2; } |
	SLAVE NUMBER =
	{
		if (cur.d_conn != NULL && cur.d_conn != TO_NEXUS
		    && cur.d_conn->d_type == MASTER)
			cur.d_slave = $2;
		else
			yyerror("can't specify slave--not to master");
	} |
	FLAGS NUMBER = { cur.d_flags = $2; }
	;

Int_spec:
	VECTOR Id_list = { cur.d_vec = $2; } | ;

Id_list:
	Save_id =
	    { struct idlst *a = (struct idlst *)malloc(sizeof(struct idlst));
	      a->id = $1; a->id_next = 0; $$ = a; } |
	Save_id Id_list =
	    { struct idlst *a = (struct idlst *)malloc(sizeof(struct idlst));
	      a->id = $1; a->id_next = $2; $$ = a; } ;
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
	if (curp == NULL)
		dtab = np;
	else
		curp->d_next = np;
	curp = np;
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

	if (num == QUES)
	    return huhcon(dev);
	for (dp = dtab; dp != NULL; dp = dp->d_next)
		if ((num == dp->d_unit) && eq(dev, dp->d_name))
		    if (dp->d_type != CONTROLLER && dp->d_type != MASTER)
		    {
			yyerror(sprintf(errbuf,
			    "%s connected to non-controller", dev));
			return NULL;
		    }
		    else
			return dp;
	yyerror(sprintf(errbuf, "%s %d not defined", dev, num));
	return NULL;
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
    int oldtype;

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
    oldtype = dp->d_type;
    dcp = dp->d_conn;
    /*
     * Now see if there is already a wildcard entry for this device
     * (e.g. Search for a "uba ?")
     */
    for (; dp != NULL; dp = dp->d_next)
	if (eq(dev, dp->d_name) && dp->d_unit == -1)
	    break;
    /*
     * If there isn't, make one becuase everything needs to be connected
     * to something.
     */
    if (dp == NULL)
    {
	dp = &rdev;
	init_dev(dp);
	dp->d_unit = QUES;
	dp->d_name = ns(dev);
	dp->d_type = oldtype;
	newdev(dp);
	dp = curp;
	/*
	 * Connect it to the same thing that other similar things are
	 * connected to, but make sure it is a wildcard unit
	 * (e.g. up connected to sc ?, here we make connect sc? to a uba?)
	 * If other things like this are on the NEXUS or if the aren't
	 * connected to anything, then make the same connection, else
	 * call ourself to connect to another unspecific device.
	 */
	if (dcp == TO_NEXUS || dcp == NULL)
	    dp->d_conn = dcp;
	else
	    dp->d_conn = connect(dcp->d_name, QUES);
    }
    return dp;
}

/*
 * init_dev:
 *	Set up the fields in the current device to their
 *	default values.
 */

init_dev(dp)
register struct device *dp;
{
    dp->d_name = "OHNO!!!";
    dp->d_type = DEVICE;
    dp->d_conn = NULL;
    dp->d_vec = NULL;
    dp->d_addr = dp->d_flags = dp->d_dk = 0;
    dp->d_slave = dp->d_drive = dp->d_unit = UNKNOWN;
}

/*
 * Check_nexus:
 *	Make certain that this is a reasonable type of thing to put
 *	on the nexus.
 */

check_nexus(dev, num)
register struct device *dev;
int num;
{
    if (!eq(dev->d_name, "uba") && !eq(dev->d_name, "mba"))
	yyerror("only uba's and mba's should be connected to the nexus");
    if (num != QUES)
	yyerror("can't give specific nexus numbers");
}

/*
 * Check the timezone to make certain it is sensible
 */

check_tz()
{
	if (timezone > 24 * 60)
		yyerror("timezone is unreasonable");
	else
		hadtz = TRUE;
}
