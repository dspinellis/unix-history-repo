/*
 *  daemon.c -- main control routines for spider and phone dpd daemons,
 *			and for line printer daemon.
 */

long	snsum = 0;		/*number of characters written*/

#include	"daemon0.c"

#include	<setjmp.h>
#include	<sys/dir.h>

struct	direct	dbuf;
int	LDIRNAM	= 0;		/*length of directory name. MRW*/
int	LCHAR	= 0;		/*length of file name thru 'df'.*/
int	LPID	= 0;		/*length of file name to process-id.*/
int	retcode	= 1;
int	nwait	= 0;
char	*fowner;		/*RBB*/
char	baddf[30];		/*name for unsendable df-files.*/
FILE	*popen();

#if LPD
int	waittm	= 10;
#else
int	waittm	= 60;
#endif

main()
{
	register char *p1, *p2;
	FILE *df;

	dem_setup();
	LDIRNAM = 0;		/*calculate length of directory name. MRW*/
	while(dfname[LDIRNAM])  LDIRNAM++;
	LCHAR = LDIRNAM + 2;
	LPID = LDIRNAM + 3;

again:
	snsum = 0;
	if(access(lock, 0) < 0){
		logerr("Lock has disappeared.");
		dem_dis();
		exit(1);
	}
	df = fopen(dpd, "r");
	if (df) {
		do {
			if(fread((char *)&dbuf, sizeof dbuf, 1, df) < 1){
				dem_dis();	/*disconnect phone line. MRW*/
				unlink(lock);
				exit(0);
			}
		} while (dbuf.d_ino==0 || dbuf.d_name[0]!='d' || dbuf.d_name[1]!='f');
		fclose(df);
	}
	p1 = dbuf.d_name;
	p2 = &dfname[LDIRNAM];
	while (p1 < &dbuf.d_name[DIRSIZ])
		*p2++ = *p1++;
	if (trysend() == 0) {
		waittm = 60;
		nwait = 0;
		goto again;
		}
#if PHONE
	if(nwait > 10){		/*after 3 hours try fresh daemon. MRW*/
		unlink(lock);
		execl("/usr/lib/dpd", "dpd", (char *)0);
		execl("/etc/dpd", "dpd", (char *)0);
		logerr("Can't find dpd.");
		exit(1);
	}
#endif
	sleep(waittm);
#if PHONE || SPIDER
#ifndef DEBUG
	if (waittm <= 8*60)
		waittm *= 2;
	else
		nwait++;
#endif
#endif
	goto again;
}

/*
 * The remaining part is the device interface, to spider, the dataphone,
 *    or the line printer.
 */

#define	MXLINE	128
#define	MXMESS	500

char	*snumb;
char	line[MXLINE];
char	message[MXMESS+1];
char	*mesp;
char	mailfname[64];
int	linel;
FILE	*dfb = NULL;
jmp_buf	env;
char	ff	= '\014';		/*formfeed*/
char	*copline();

trysend()
{
	if(retcode != 0)
		if(retcode = dem_con())		/*connect phone line.*/
			return(retcode);
	retcode = 1;
	if(setjmp(env))
		return(retcode);
	xtrysend();
	return(0);
}

xtrysend()
{
	int i;

	dem_open(dfname);		/*open spider connection.*/
	if((dfb = fopen(dfname, "r")) == NULL){
		if(LDIRNAM < (i = sizeof(baddf)-1)){
			strncpy(baddf, dfname, i);
			baddf[i] = '\0';
			baddf[LDIRNAM] = 'b';
			link(dfname, baddf);
		}
		unlink(dfname);
		retcode = 0;
		trouble("Can't read %s.", dfname);
	}
	getowner(dfname);		/*RBB*/
	mesp = message;
	*mesp = 0;
	while (getline()) switch (line[0]) {

	case 'S':
		get_snumb();		/*get snumb for GCOS.*/
		continue;

	case 'B':
		if(sascii(0))
			trouble("Can't send %s.", &line[1]);
		continue;

	case 'F':
		if(sascii(1))
			trouble("Can't send %s.", &line[1]);
		continue;

	case 'I':			/*mail back $IDENT card. MRW*/
		mesp = copline(&line[1], linel-1, mesp);

	case 'L':
		lwrite();		/*write a literal line.*/
		continue;

	case 'M':
		continue;

	case 'N':			/*mail back file name. MRW*/
		copline(&line[1], linel-1, mailfname);
		continue;

	case 'Q':			/*additional text to mail back*/
		if(mesp+linel <= message+MXMESS)
			mesp = copline(&line[1], linel-1, mesp);

	case 'U':
		continue;
	}
/*
 * Second pass.
 * Unlink files and send mail.
 */
	alarm(0);
	fseek(dfb, (long)0, 0);
	while (getline()) switch (line[0]) {

	default:
		continue;

	case 'U':
		unlink(&line[1]);
		continue;

	case 'M':
		sendmail();
		continue;
	}
	FCLOSE(dfb);
	dem_close();		/*close connection to spider.*/
	unlink(dfname);
	retcode = 0;
	trouble("OK: %-5s %-7s %-8s", snumb, fowner, dfname+LDIRNAM);	/*RBB*/
}

#define	LUBM	30	/*length of mail command line.*/
#define	IARG1	5	/*start of arg to mail command.*/
char	mail[LUBM+1]	= "mail ";
FILE	*pmail;

sendmail()
{
	register i;

	alarm(0);
	i = 0;
	while((mail[IARG1+i] = line[i+1]) && (++i < LUBM-IARG1));
	mail[IARG1+i] = 0;
	if((pmail = popen(mail, "w")) == NULL){
		logerr("Can't mail: %-5s %-8s",snumb,dfname+LDIRNAM);  /*MRW*/
		return;
	}
	maildname();
	pclose(pmail);
}

#if LPD == 0

maildname()		/*break up dfname into command name, and process-id
			    to send back in the mail. MRW*/
{
	char c;
	char *command;

	fprintf(pmail,"Sent %-5s:  file  %s%s",snumb,mailfname,message);
/*
	c = dfname[LCHAR];
	switch (c){
	case 'A':
	case 'a':
		command = "dpr";
		break;

	case 'G':
	case 'g':
		command = "gcat";
		break;

	case 'J':
	case 'j':
		command = "ibm";
		break;

	case 'N':
	case 'n':
		command = "fsend";
		break;

	case 'T':
	case 't':
		command = "fget";
		break;

	default:
		command = &dfname[LDIRNAM];
		break;
	}
	fprintf(pmail, "%s process-id was %s\n", command, &dfname[LPID]);
*/
}

#endif


getline()
{
	register char *lp;
	register c;

	lp = line;
	linel = 0;
	while ((c = getc(dfb)) != '\n' && linel < MXLINE-2) {
		if (c == EOF)
			return(0);
		if (c=='\t') {
			do {
				*lp++ = ' ';
				linel++;
			} while ((linel & 07) != 0);
			continue;
		}
		*lp++ = c;
		linel++;
	}
	*lp++ = 0;
	return(1);
}

char *
copline(ab, n, ml)
char	*ab;
int	n;
char	ml[64];
{
	register char *b, *p, *eb;

	b = ab;
	eb = b+n;
	p = ml;
	while (b<eb && p < &ml[63])
		*p++ = *b++;
	*p++ = '\n';
	*p = 0;			/*RBB*/
	return(p);
}

#if LPD == 0

/*
 * the following code determines who the file's owner is.
 *			/*RBB*/

struct passwd *getpwuid();

getowner(file)
char	*file;
{
	struct passwd *f;

	if(stat(file, &statbuf) < 0)
		return;
	if((f = getpwuid(statbuf.st_uid)) == NULL)
		fowner = "";
	else
		fowner = f->pw_name;
}

#endif
