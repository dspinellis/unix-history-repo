#include	<stdio.h>

/*
 *  lpr -- on line print to line printer
 */
#ifdef VPR
/*  if VPR defined, uses versatek printer via /usr/spool/vpd. */
#endif

#define	DPR	0
#define	FGET	0
#define	FSEND	0
#define	GCAT	0
#define	LPR	1

#ifdef VPR
#define	NAME	"vpr"
#else
#define	NAME	"lpr"
#endif

#define	BF	'F'
#define	MAXCOPY	204800L
#define FIRSTCHAR 'A'-1

#ifdef VPR
char	cfname[]	= "/usr/spool/vpd/cf@XXXXX";
char	dfname[]	= "/usr/spool/vpd/df@XXXXX";
char	lfname[]	= "/usr/spool/vpd/lf@XXXXX";
char	tfname[]	= "/usr/spool/vpd/tf@XXXXX";
char	zfname[]	= "/usr/spool/vpd/zf@XXXXX";
#else
char	cfname[]	= "/usr/spool/lpd/cf@XXXXX";
char	dfname[]	= "/usr/spool/lpd/df@XXXXX";
char	lfname[]	= "/usr/spool/lpd/lf@XXXXX";
char	tfname[]	= "/usr/spool/lpd/tf@XXXXX";
char	zfname[]	= "/usr/spool/lpd/zf@XXXXX";
#endif

#include	"spool.c"

main(agc, agv)
int agc;
char *agv[];
{
	argc = agc;    argv = agv;
	pidfn();

	while (argc>1 && (arg = argv[1])[0]=='-') {
	    if(!comopt(arg[1]))
		switch (arg[1]) {

		default:
			fprintf(stderr, "%s: Unrecognized option: %s\n", NAME, arg);
			break;
		}
		argc--;
		argv++;
	}

	if(debug)
		tff = stdout;
	else
		if((tff = nfile(tfname)) == NULL){
			fprintf(stderr, "%s: Can't create %s.\n", NAME, tfname);
			out();
		}
	if(ident())
		out();

	filargs();		/*process file arguments.*/

	if(debug)
		out();
	fclose(tff);
	if(nact) {
		dfname[INCHAR]++;
		if(link(tfname, dfname) < 0){
			fprintf(stderr, "%s: Cannot rename %s\n", NAME, tfname);
			out();
		}
		unlink(tfname);
#ifdef VPR
		execl("/usr/lib/vpd", "vpd", 0);
		execl("/etc/vpd", "vpd", 0);
#else
		execl("/usr/lib/lpd", "lpd", 0);
		execl("/etc/lpd", "lpd", 0);
#endif
		fprintf(stderr, "%s: Can't find daemon.\nFiles left in spooling dir.\n", NAME);
		exit(1);
	}
	out();
}


archive()
{
}


nuact()
{
}
