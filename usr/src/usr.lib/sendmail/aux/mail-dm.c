#include <arpa/netopen.h>
#include "srvrftp.h"
#include <statbuf.h>
#include <arpa/hostnames.h>
#include <io_buf.h>
#include <arpa/mail.h>
#include <ident.h>
#include <signal.h>
#include <log.h>
extern int fout;

static char SccsId[] = "@(#)mail-dm.c	4.1	7/25/83";

/*
Name:
	mail

Function:
	handle the MAIL <user> command over the command connection

Algorithm:
	see if we have a known user

	if mailbox file can't be gotten
		return
	tell him it is ok to go ahead with mail

	while he doesn't type a period
		read and write data
	say completed

Parameters:
	username in arg

Returns:
	nothing

Globals:
	arg
	username=

Calls:
	strmove
	getuser
	loguser
	openmail
	closemail
	getline
	chown (sys)
	time (sys)
	printf (sys)
	getch	(util)
	putch	(util)

Called by:
	main thru command array

History:
	initial coding 		Mark Kampe UCLA-ATS
	modified 4/13/76 by S. F. Holmgren for Illinois version
	modified 6/30/76 by S. F. Holmgren to call getmbox
	modified 10/18/76 by J. S. Kravitz to improve net mail header
	chown removed by R. Balocca @ CAC, Sunday 1977 February 20
	getline removed and limit on line length removed by using
	getch and putch added by R. Balocca @ CAC, 1977 March 8 Tuesday
	Fixed oversight in above (forgot to translate <crlf> to <lf>)
		1977 March 10 Thursday by Rick Balocca @ CAC
	Added openmail & closemail, added logging, and fixed several
		bugs on or about 12/21/79 by Eric Allman, UCB/INGRES.
	Changed to always accept mail -- bad mail will be sent back --
		1/9/80 by Eric Allman, UCB/INGRES.
	Don't print out 350 enter mail or 256 mail accepted messages --
		sendmail will do that.  8/19/81 Eric Allman UCB/INGRES.
*/
#define gt (c = getch())
mail()
{
	register char *p;	/* general use */
	register int c;
	int i;

	/* extern struct io_buf obuf; */

	/* get to open mailbox file descriptor */
	fflush(&fout);
	if( (fout = openmail(arg, 0)) < 0 )
		return;

	for(;;)				/* while no error or <crlf>.<crlf> */
	{
		/* we are at beginning of line */

		if(gt=='.')			/*"."*/
		{
			if(gt=='\r')			/*".\r"*/
			{
				if(gt=='\n')			/*".\r\n"*/
				{
					/* end of message */
					break;
				}
				else
				{				/*".\r"c*/
					putch('.');
					putch('\r');
				}
			}
			else				/*"."c"*/
				putch('.');
		}
								/*"-"*/
					/* c */
		for(;;)
		{
			for(; c != '\r'; gt)
			{
				if( c < 0 )
				{
					fflush(&fout);
					write(fout, "\n***** Sender aborted connection *****\n", 39);
					goto out;
				}
				else
					putch(c);
			}
	
						/*"\r"*/
			if( gt == '\n' )
			{			/*"\r\n"*/
crlf:
				putch('\n');
				break;
			}
			else
			{			/*"\r"c*/
crc:
				putch('\r');
				if(c=='\0')
					gt;	/* "\r\0" */
						/* is arpa escape for "\r" */
			}
		}
	}

out:
	fflush(&fout);
	closemail(fout);
}

/*
Name:
	datamail

Function:
	handle the MLFL command

Algorithm:
	fork
		make sure we have a valid user
			say bad user and exit
		send sock command
		open data connection
		get open mailbox file descriptor
		call rcvdata to receive mail

Parameters:
	username in arg

Returns:
	nothing

Globals:
	arg

Calls:
	fork (sys)
	strmove
	netreply
	sendsock
	dataconnection
	getmbox
	rcvdata
	printf (sys)
	time (sys)

Called by:
	main thru command array 

History:
	initial coding 4/13/76 by S. F. Holmgren
	modified 10/18/76 by J. S. Kravitz to put net mail header
	chown removed by R. Balocca @ CAC, Sunday 1977 February 20
*/
datamail()
{
	register netdata;
	/* register mboxfid; */
	register int i;

	i = fork();
	if (i < 0)
	{
		netreply("455 Mail server temporarily unavailable\r\n");
		return;
	}
	else if (i == 0)
	{
		fflush(&fout);
		if ((fout = openmail(arg, 1)) < 0)
			exit(3);

		/* send sock command */
		sendsock( U4 );

		/* open data connection */
		netdata = dataconnection( U4 );

		/* say its ok to proceed */
		numreply( NUM250 );

		/* get data from net connection and copy to mail file */
		/* rcvdata( netdata,mboxfid ); */
		if (rcvdata(netdata, fout) < 0)
			exit(1);

		/* close the mail, see if ok; if so say ok */
		fflush(&fout);
		closemail(fout);

		exit( 0 );
	}
}
/*
**  OPENMAIL -- Open a channel to the mail server
**
**	Gets the mail server started up ready to handle our
**	mail.
**
**	Algorithm:
**		See if the user is specified.
**			If not, send to user "root".
**		See if the user exists.
**			If not, signal error 450 and return.
**		Fork.
**		Create a pipe
**			Signal "unavailable" and exit on failure.
**		Fork.
**			Signal "unavailable" and exit on failure
**			In child:
**				Call mailer: /etc/delivermail is preferred.
**			In parent:
**				Avoid pipe signals in case delivermail dies.
**				Save the childs pid.
**				Return file descriptor.
**
**	Notes:
**		The check to see if the user actually exists should
**		go away so that we can do real mail forwarding.
**
**	Parameters:
**		who -- the user to send the mail to.
**		mode -- 0 -- called from mail
**			1 -- called from mlfl
**
**	Returns:
**		File descriptor to send mail to.
**		-1 on failure.
**
**	Side Effects:
**		Forks /etc/delivermail or /bin/mail or /usr/bin/mail.
**		Becomes "network" in the child.
**
**	Requires:
**		strmove
**		getuser
**		netreply
**		pipe (sys)
**		fork (sys)
**		close (sys)
**		dup (sys)
**		execl (sys)
**		signal (sys)
**		exit (sys)
**
**	Called By:
**		mail
**		datamail
**
**	History:
**		1/9/80 -- Added 050 & 455 reply messages if execl's
**			fail.  Eric Allman UCB/INGRES.
**		11/26/79 -- Modified to map upper case to lower
**			case.  Eric Allman UCB/INGRES.
**		11/10/79 -- Written by Eric Allman UCB/INGRES
**		3/6/80 -- Dropped case mapping; delivermail does
**			that now.  EPA UCB/INGRES.
**		8/19/81 -- Added "mode" parameter; call sendmail
**			instead of delivermail.  EPA
*/

int Mail_pid;
char *Mail_user;

openmail(who, mode)
	char *who;
	int mode;
{
	register char *w;
	register int i;
	int pvect[2];
	register char *p;

	w = who;
	if (w == 0)
		w = "root";
	Mail_user = w;

	/* see if the user exists */
	strmove(w, username);

	/* try to get a pipe to the mailer */
	if (pipe(pvect) < 0)
	{
	  unavailable:
		netreply("455 Mail server temporarily unavailable\r\n");
		return (-1);
	}

	/* fork */
	i = fork();
	if (i < 0)
	{
		/* failure */
		close(pvect[0]);
		close(pvect[1]);
		goto unavailable;
	}
	else if (i == 0)
	{
		/* child */
		close(pvect[1]);
		close(0);
		dup(pvect[0]);
		close(pvect[0]);
		setuid(NETUID);

		/* try to call something to deliver the mail */
		execl("/etc/sendmail", "sendmail", "-v", mode == 1 ? "-af" : "-am", w, 0);

		/* doesn't seem to be anything around */
		netreply("455 Mail server unavailable\r\n");
		exit(3);
	}

	/* else parent */
	signal(SIGPIPE, SIG_IGN);
	Mail_pid = i;
	close(pvect[0]);
	return (pvect[1]);
}
/*
**  CLOSEMAIL -- Close the mail file and get actual status
**
**	The mail file is closed.
**
**	Algorithm:
**		Wait for the mailer to die.
**			If it wasn't there, be non-comittal.
**		If it died a violent death, give error.
**
**	Parameters:
**		fd -- the file descriptor of the mail file.
**
**	Returns:
**		none.
**
**	Side Effects:
**		mailer is soaked up.
**
**	Requires:
**		close (sys)
**		wait (sys)
**
**	Called By:
**		mail
**		datamail
**
**	History:
**		1/9/80 -- Changed to not check for errors in mailing,
**			since these will be mailed back.
**		11/10/79 -- Written by Eric Allman UCB/INGRES.
*/

closemail(fd)
	int fd;
{
	auto int st;
	register int i;

	/* close the pipe -- mail should go away */
	close(fd);

	/* wait for its body */
	while ((i = wait(&st)) != Mail_pid)
	{
		if (i < 0)
		{
			/* how did this happen? */
			logmsg(LOG_ERR, "mail from host %d to %s: no child",
			    openparams.o_frnhost & 0377, Mail_user);
			goto unavailable;
		}
	}

	/* 'st' is now the status of the mailer */
	if ((st & 0377) != 0)
	{
		logmsg(LOG_ERR, "mail from host %d to %s: status %o",
		    openparams.o_frnhost & 0377, Mail_user, st);
unavailable:
		netreply("455 Mail not delivered -- local system error\r\n");
		return (-1);
	}

	return (0);
}
