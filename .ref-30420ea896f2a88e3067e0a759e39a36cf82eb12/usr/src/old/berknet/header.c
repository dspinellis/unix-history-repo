static char sccsid[] = "@(#)header.c	4.1	(Berkeley)	%G%";

/* sccs id variable */
static char *header_sid = "@(#)header.c	1.3";
/*
	header.c

	these routines provide a way of transferring the information
	in the "header" structure between machines.
	Programs calling these routines either read or write
	their information into the header structure.
	These procedures format the info in a way that is acceptable.
	Called by net.c, netdaemon.c, and netq.c.
*/
/*
	protocol that is sent (in ASCII):
	code, remote mach, local mach, version stamp (2), remote login name,
	password, -i, -o, -r files,
	local login name, terminal, flag, utmp tty login time,
	cc jobno(variable parameter list), current time,
	command '\n' real command '\n'
	any data
	
	changes:
	1) remove header
	3) use ascii length instead of 4 bytes
	4) encrypt the login name, command, and part of data as well
*/
# include "defs.h"


writehdfd(phd,fd)
register struct header *phd;
FILE *fd;
{
	char *genparmlist();
	char cflag = 'a';

	/* cflag is initially 'a'. Add the flags as needed. */
	if(phd->hd_fnonotify)cflag += F_NONOTIFY;
	if(phd->hd_fquiet)cflag += F_QUIET;

	fprintf(fd,
	"%c :%c :%c :%c :%c :%s :%s :%s :%s :%s :%s :%s :%c :%lo :%s%s :%ld :",
		phd->hd_code,phd->hd_mchto,phd->hd_mchfrom,
		phd->hd_vmajor+'a',phd->hd_vminor+'a',phd->hd_snto,
		phd->hd_spasswd,phd->hd_sinfile,phd->hd_soutfile,
		phd->hd_srespfile,
		phd->hd_snfrom,phd->hd_sttyname,cflag,phd->hd_lttytime,
		phd->hd_ijobno,genparmlist(phd),phd->hd_ltimesent-TIMEBASE);
	fputs(phd->hd_scmdact,fd);
	putc('\n',fd);
	fputs(phd->hd_scmdvirt,fd);
	putc('\n',fd);
	/* not used, but a good idea */
	sprintf(phd->hd_addrfrom,"%c:%s",phd->hd_mchfrom,phd->hd_snfrom);
	sprintf(phd->hd_addrto,  "%c:%s",phd->hd_mchto,  phd->hd_snto);
}
/*
	print out debugging values of a header structure
*/
printhd(phd)
register struct header *phd;
{
	if(debugflg){
		printf("To %s From %s quiet=%c nonotify=%c\n",
			phd->hd_addrto, phd->hd_addrfrom, 
			chfromf(phd->hd_fquiet), chfromf(phd->hd_fnonotify));
		/* don't print out for security reasons
		printf("Password %s\n",phd->hd_spasswd);
		*/
		printf("Code '%c' Version (%d,%d) Infile '%s'\n",
			phd->hd_code, phd->hd_vmajor,phd->hd_vminor,
			phd->hd_sinfile);
		printf("Outfile '%s' Respfile '%s' TTYName '%s'\n",
			phd->hd_soutfile,phd->hd_srespfile, phd->hd_sttyname);
		printf("Jobno %s TimeSent %s", phd->hd_ijobno,
			ctime(&phd->hd_ltimesent));
		if(phd->hd_lttytime != 0)
			printf("TTYTime %s", ctime(&phd->hd_lttytime));
		printf("Virtual Command \"%s\"\n", phd->hd_scmdvirt);
		printf("Actual Command \"%s\"\n", phd->hd_scmdact);
	}
}
/*
	generate a variable parameter list
	the format is:
		(name value, name value, ..., name value)
	where names are unquoted single words and values
	are unquoted if a single alphanumeric word, and are
	surrounded by {} otherwise. \ quotes { and }.
	the values are escape-processed, e.g. \n becomes 012.
	this function returns such a list.
	Returns the null parm list if nothing to give, i.e. "()" 

	Should also default so single keywords can have on/off
	states, and so do not require a value.

	Things this variable protocol should specify:
		EPASSWD 	encrypted passwd
		FILEMODE 	file mode
		FROMUID  	from users' uid
		FROMGID  	from users' gid
		COMPRESS 	use colin's compression
		ACCTPAIR	handle acct pairs
		MESSAGEID	unique number identifying this request.
		FILENAME	when omitted by netcp, will use FILENAME ext.
		REPLYTO		the person the response should be sent to

		 --- possibly ---
		MACHINE2	a second machine (e.g. 3way netcp)
		LOGIN2		a second login name
		PASSWD2		a second passwd

*/
static char *genparmlist(phd)
register struct header *phd;
{
	static char returnstr[PARMLIST];
	strcpy(returnstr,"()");
	return(returnstr);
}
