/*	v3451.c	4.1	83/06/15	*/

#if VADIC
/*
 * Routines for calling up on a Vadic 3451 Modem
 */
#include "tip.h"
#include <setjmp.h>
#include <errno.h>
#include <signal.h>

static char *sccsid = "@(#)v3451.c	4.1 %G%";

int	va_delay;
static	int	fudge=0;	/* for sleep in vawrite */
jmp_buf Sjbuf;

vadic_dialer(num, acu)
	register char *num;
	char *acu;
{
	int lt;
	int ok;
	char phone[50];
#ifdef ACULOG
	char line[80];
#endif
	int (*func) ();

	if(number(value(BAUDRATE)) < 1200)
		fudge = 1;
	/*
	 * Get in synch
	 */
	lt = strlen(num);
	va_delay = 15 + 3*lt;
	vawrite("I\r",1);
	vawrite("I\r",1);
	vawrite("I\r",1);
	vawrite("\005\r",2);
	ok = expect("READY");

	if ( ok ) {
		printf("can't synchronize with vadic 3451\n");
#ifdef ACULOG
		logent(value(HOST), num, "vadic", "can't synch up");
#endif
		return (0);
	}
	ioctl(FD, TIOCHPCL, 0);
	sleep(1);
	vawrite("D\r",2);
	ok = expect("NUMBER?");
	if ( ok ) {
		printf("Vadic will not accept dial command\n");
#ifdef ACULOG
		logent(value(HOST), num, "vadic", "will not accept dial");
#endif
		return (0);
	}
	strcpy(phone,num);
	strcat(phone,"\r");
	vawrite(phone,1);
	ok = expect(phone);
	if ( ok ) {
		printf("Vadic will not accept phone number\n");
#ifdef ACULOG
		logent(value(HOST), num, "vadic", "will not accept number");
#endif
		return (0);
	}
	func = signal(SIGINT,SIG_IGN);
	/* You cannot interrupt the Vadic when its dialing */
	/* Even dropping DTR does not work /*
	/* Definitely a Brain Damaged Design */
	vawrite("\r",1);
	vawrite("\r",1);
	ok = expect("DIALING:");
	if ( ok ) {
		printf("Vadic failed to dial\n");
#ifdef ACULOG
		logent(value(HOST), num, "vadic", "failed to dial");
#endif
		return (0);
	} else
		printf("dialing...\n");
	ok = expect("ON LINE");
	signal(SIGINT,func);
	if ( ok ) {
		printf("call failed\n");
#ifdef ACULOG
		logent(value(HOST), num, "vadic", "call failed");
#endif
		return (0);
	}
	ioctl(FD, TIOCFLUSH);
	return (1);
}

vadic_disconnect()
{
	char string[100];
	close(FD);
	sleep(5); /* insure that the phone line is dropped */
	sprintf(string,"/usr/lib/uucp/enable %s\n",rindex(DV,'/')+1);
	system(string);
}

vadic_abort()
{
	vadic_disconnect();
}

vawrite(str,delay)
char *str;
int delay;
{
	while(*str)
	{
		write(FD,str,1);
		sleep(delay+fudge);
		str++;
	}
	return;
}


#define MR 300

int Error = 0;

/***
 *	expect(str)	look for expected string
 *	char *str;
 *
 *	return codes:
 *		0  -  found
 *		FAIL  -  lost line or too many characters read
 *		some character  -  timed out
 */

expect(str)
char *str;
{
	static char rdvec[MR];
	extern alarmtr();
	char *rp = rdvec;
	int nextch = 0, kr;
	int alarm_tm;
	int expect_online = 0;

	if (strcmp(str, "\"\"") == 0)
		return(0);
	*rp = 0;
	/*
	 * If we are waiting for the Vadic to complete
	 * dialing and get a connection, allow more time
	 * Unfortunately, the Vadic times out 24 seconds after
	 * the last digit is dialed
	 */
	if(strcmp(str, "ON LINE") == 0){
		alarm_tm = number(value(DIALTIMEOUT));
		expect_online++;
	}
	else
		alarm_tm = 30;
	if (setjmp(Sjbuf)) {
		return(1);
	}
	signal(SIGALRM, alarmtr);
	alarm(alarm_tm);
	while (notin(str, rdvec)) {
		if(expect_online)
			if(notin("FAILED CALL", rdvec) == 0)
				return(1);
		kr = read(FD, &nextch, 1);
		if (kr <= 0) {
			alarm(0);
			return(1);
		}
		{
		int c;
		c = nextch & 0177;
		}
		if ((*rp = nextch & 0177) != '\0')
			rp++;
		*rp = '\0';
		if (rp >= rdvec + MR)
			return(1);
	}
	alarm(0);
	return(0);
}

/***
 *	alarmtr()  -  catch alarm routine for "expect".
 */

alarmtr()
{
	longjmp(Sjbuf, 1);
}

/***
 *	notin(sh, lg)	check for occurrence of substring "sh"
 *	char *sh, *lg;
 *
 *	return codes:
 *		0  -  found the string
 *		1  -  not in the string
 */

notin(sh, lg)
char *sh, *lg;
{
	while (*lg != '\0') {
		if (prefix(sh, lg))
			return(0);
		else
			lg++;
	}
	return(1);
}

/*******
 *	prefix(s1, s2)	check s2 for prefix s1
 *	char *s1, *s2;
 *
 *	return 0 - !=
 *	return 1 - == 
 */

prefix(s1, s2)
char *s1, *s2;
{
	char c;

	while ((c = *s1++) == *s2++)
		if (c == '\0')
			return(1);
	return(c == '\0');
}
