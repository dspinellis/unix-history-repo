static char sccsid[] = "@(#)sendberkmail.c	4.1	(Berkeley)	%G%";

# include "defs.h"

/*
Usage:
	sendberkmail [-m mach ] [-f addrfrom] [-h hopcnt] -t addrto
	
Archaic Usage:
	sendberkmail mach:user

	Send remote mail to user on mach.
	Only one addrto allowed.

	Sendberkmail uses the network to send an mmail command
	to the remote machine.  It specifies the source, destination,
	and a hop count only.

	Sendberkmail uses the -q option of net, so only error msgs
	and non-zero return codes will be sent back.

	It is best to think of sendberkmail as a transport mechanism:
	It takes mail from one machine to another machine (specified
	using the -m option) and executes the local mail program
	there with a to-address of "addrto", and a from-address
	of "addrfrom".  If the -m option is not given, it parses the
	"addrto" field to get a berkeley network address.
	This extreme generality is necessary when destinations are on
	different networks, consider a command from the Ing70:

		sendberkmail -m csvax -f schmidt@parc -t research!chuck 

	This is clearly a forwarding function- send mail from the Arpanet
	to the Bell Net, which calls our CSVAX.
	Alternatively, executed on the CSVAX,
		sendberkmail -m ing70 -f research!chuck -t schmidt@parc
	sends mail the other way.

	There is duplication in the arguments because of
	a need to convert to labelled parameters.
	See the note in mmail.c to that effect.


	Options:
		-t addrto	mail command on remote machine will be
				fed "addrto" as address
		-f addrfrom	mail will be "From" addrfrom
		-m mach		send this mail to the "mach" machine
		-h hopcnt	if this hopcnt hits a threshold, there
				is presumed to be an infinite loop.
	
*/
main(argc,argv)
  char **argv; {
	char addrto[BUFSIZ], addrfrom[BUFSIZ], *sn;
	char mchto = 0, snto[BUFSIZ], snfrom[BUFSIZ], smchto[20], mchfrom;
	int cmdstr[BUFSIZ], hopcntstr[20];
	char rcmd[BUFSIZ];
	int hopcnt = 0;

	argc[argv] = 0;
	debugflg = DBV;
	addrfrom[0] = 0;
	addrto[0] = 0;

	while(argc > 1 && argv[1][0] == '-'){
		argc--; argv++;
		switch(argv[0][1]){
		case 'f':
			harg(addrfrom);
			break;
		case 'h':
			harg(hopcntstr);
			hopcnt = atoi(hopcntstr);
			break;
		case 'm':
			harg(smchto);
			mchto = lookup(smchto);
			break;
		case 't':
			harg(addrto);
			break;
		/* it is important to ignore unknown flags
		   for compatibility reasons */
		}
	}

	/* handle to address */
	if(argc > 1)strcpy(addrto,argv[1]);
	if(addrto[0] == 0){
		fprintf(stderr,"Usage: sendberkmail mach:user\n");
		exit(EX_USAGE);
	}
	if(mchto == 0)
		mchto = MchSFromAddr(snto,addrto);
	else
		strcpy(snto,addrto);
	if(mchto == 0){
		fprintf(stderr,"Unknown host %s\n",addrto);
		exit(EX_NOHOST);
	};
	if(mchto == local){
		fprintf(stderr,
		"Use mail to send to %s on this machine. Mail not delivered.\n",
			addrto);
		exit(EX_NOUSER);
	}
	sprintf(rcmd,"mail %s",addrto);

	/* handle from address */
	if(addrfrom[0] == 0){
		char name[100];
		SnCurrent(name);
		sprintf(addrfrom,"%s:%s",longname(local),name);
	}
	mchfrom = MchSFromAddr(snfrom,addrfrom);

	/* uses new options of mmail */
	/* X's are for compatibility with mmail */
	sprintf(cmdstr,"%s XXX XXX XXX -f '%s' -t '%s' -h %d", MMAILCMD,
		addrfrom,addrto,hopcnt);
	/* old code:
	sprintf(cmdstr,"%s '%s' %s '%s'", MMAILCMD,snfrom,
		longname(mchfrom),snto);
	*/


	mexecl(netcmd,"net","-m",longname(mchto),"-q","-l","network",
		"-","-c",rcmd,cmdstr,0);
	perror(netcmd);
	fprintf(stderr,"Network is down\n");
	exit(EX_UNAVAILABLE);
}

SnCurrent(name)
  char *name;
	{
	char *sn;
	sn = getlogin();
	if(sn == NULL || *sn == 0 || *sn == ' '){
		struct passwd *pwd;
		pwd = getpwuid(getuid());	/* will read passwd file */
		if(pwd != NULL) sn = pwd->pw_name;
		if(sn == NULL){
			fprintf(stderr,"Who are you?\n");
			exit(EX_OSERR);
		}
	}
	strcpy(name, sn);
}
