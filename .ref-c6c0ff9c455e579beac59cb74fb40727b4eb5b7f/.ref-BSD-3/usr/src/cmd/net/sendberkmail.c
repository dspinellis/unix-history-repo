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
	
	Exit Codes:
		Returns the net commands return code normally.
		101	The net cmd wont exec.
		102	Bad userid/name.
		103	Mail is to the local machine.
		104	Wrong # arguments.
*/
main(argc,argv)
  char **argv; {
	char addrto[BUFSIZ], addrfrom[BUFSIZ];
	char mchto = 0, snto[NS], *snfrom, smchto[20];
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
			harg(addrfrom,&argc,&argv);
			break;
		case 'h':
			harg(hopcntstr,&argc,&argv);
			hopcnt = atoi(hopcntstr);
			break;
		case 'm':
			harg(smchto,&argc,&argv);
			mchto = lookup(smchto);
			break;
		case 't':
			harg(addrto,&argc,&argv);
			break;
		/* it is important to ignore unknown flags
		   for compatibility reasons */
		}
	}

	/* handle to address */
	if(argc > 1)strcpy(addrto,argv[1]);
	if(addrto[0] == 0){
		fprintf(stderr,"Usage: sendberkmail mach:user\n");
		exit(104);
	}
	if(mchto == 0)
		mchto = MchSFromAddr(snto,addrto);
	else
		strcpy(snto,addrto);
	if(mchto == 0 || mchto == local){
		fprintf(stderr,
		"Use mail to send to %s on this machine. Mail not delivered.\n",
			addrto);
		exit(103);
	}
	sprintf(rcmd,"mail %s",addrto);

	/* handle from address */
	snfrom = SnCurrent();
	if(snfrom == NULL){
		fprintf(stderr,"Unknown userid\n");
		exit(102);
	}
	if(addrfrom[0] == 0)
		sprintf(addrfrom,"%s:%s",longname(local),snfrom);

	/* uses both old and new options of mmail */
	/* compatibility problems
	sprintf(cmdstr,"%s %s %s %s -f %s -t %s -h %d", MMAILCMD,snfrom,
		longname(local),snto,addrfrom,addrto,hopcnt);
	*/
	sprintf(cmdstr,"%s %s %s %s", MMAILCMD,snfrom,
		longname(local),snto);


	mexecl(netcmd,"net","-m",longname(mchto),"-q","-l","network",
		"-","-c",rcmd,cmdstr,0);
	fprintf(stderr,"Network is down\n");
	exit(101);
	}
