/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)utilities.c	1.19 (Berkeley) 6/28/90";
#endif /* not lint */

#define	TELOPTS
#define	TELCMDS
#include <arpa/telnet.h>
#include <sys/types.h>
#include <sys/time.h>

#include <ctype.h>

#include "general.h"

#include "fdset.h"

#include "ring.h"

#include "defines.h"

#include "externs.h"

FILE	*NetTrace = 0;		/* Not in bss, since needs to stay */
int	prettydump;

/*
 * upcase()
 *
 *	Upcase (in place) the argument.
 */

void
upcase(argument)
register char *argument;
{
    register int c;

    while ((c = *argument) != 0) {
	if (islower(c)) {
	    *argument = toupper(c);
	}
	argument++;
    }
}

/*
 * SetSockOpt()
 *
 * Compensate for differences in 4.2 and 4.3 systems.
 */

int
SetSockOpt(fd, level, option, yesno)
int
	fd,
	level,
	option,
	yesno;
{
#ifndef	NOT43
    return setsockopt(fd, level, option,
				(char *)&yesno, sizeof yesno);
#else	/* NOT43 */
    if (yesno == 0) {		/* Can't do that in 4.2! */
	fprintf(stderr, "Error: attempt to turn off an option 0x%x.\n",
				option);
	return -1;
    }
    return setsockopt(fd, level, option, 0, 0);
#endif	/* NOT43 */
}

/*
 * The following are routines used to print out debugging information.
 */

unsigned char NetTraceFile[256] = "(standard output)";

void
SetNetTrace(file)
register char *file;
{
    if (NetTrace && NetTrace != stdout)
	fclose(NetTrace);
    if (file  && (strcmp(file, "-") != 0)) {
	NetTrace = fopen(file, "w");
	if (NetTrace) {
	    strcpy(NetTraceFile, file);
	    return;
	}
	fprintf(stderr, "Cannot open %s.\n", file);
    }
    NetTrace = stdout;
    strcpy(NetTraceFile, "(standard output)");
}

void
Dump(direction, buffer, length)
char	direction;
char	*buffer;
int	length;
{
#   define BYTES_PER_LINE	32
#   define min(x,y)	((x<y)? x:y)
    char *pThis;
    int offset;
    extern pettydump;

    offset = 0;

    while (length) {
	/* print one line */
	fprintf(NetTrace, "%c 0x%x\t", direction, offset);
	pThis = buffer;
	if (prettydump) {
	    buffer = buffer + min(length, BYTES_PER_LINE/2);
	    while (pThis < buffer) {
		fprintf(NetTrace, "%c%.2x",
		    (((*pThis)&0xff) == 0xff) ? '*' : ' ',
		    (*pThis)&0xff);
		pThis++;
	    }
	    length -= BYTES_PER_LINE/2;
	    offset += BYTES_PER_LINE/2;
	} else {
	    buffer = buffer + min(length, BYTES_PER_LINE);
	    while (pThis < buffer) {
		fprintf(NetTrace, "%.2x", (*pThis)&0xff);
		pThis++;
	    }
	    length -= BYTES_PER_LINE;
	    offset += BYTES_PER_LINE;
	}
	if (NetTrace == stdout) {
	    fprintf(NetTrace, "\r\n");
	} else {
	    fprintf(NetTrace, "\n");
	}
	if (length < 0) {
	    fflush(NetTrace);
	    return;
	}
	/* find next unique line */
    }
    fflush(NetTrace);
}


void
printoption(direction, fmt, option)
	char *direction, *fmt;
	int option;
{
	if (!showoptions)
		return;
	fprintf(NetTrace, "%s ", direction);
	if (TELOPT_OK(option))
		fprintf(NetTrace, "%s %s", fmt, TELOPT(option));
	else if (TELCMD_OK(option))
		fprintf(NetTrace, "%s %s", fmt, TELCMD(option));
	else
		fprintf(NetTrace, "%s %d", fmt, option);
	if (NetTrace == stdout)
	    fprintf(NetTrace, "\r\n");
	else
	    fprintf(NetTrace, "\n");
	return;
}

optionstatus()
{
    register int i;
    extern char will_wont_resp[], do_dont_resp[];

    for (i = 0; i < 256; i++) {
	if (do_dont_resp[i]) {
	    if (TELOPT_OK(i))
		printf("resp DO_DONT %s: %d\n", TELOPT(i), do_dont_resp[i]);
	    else if (TELCMD_OK(i))
		printf("resp DO_DONT %s: %d\n", TELCMD(i), do_dont_resp[i]);
	    else
		printf("resp DO_DONT %d: %d\n", i,
				do_dont_resp[i]);
	    if (my_want_state_is_do(i)) {
		if (TELOPT_OK(i))
		    printf("want DO   %s\n", TELOPT(i));
		else if (TELCMD_OK(i))
		    printf("want DO   %s\n", TELCMD(i));
		else
		    printf("want DO   %d\n", i);
	    } else {
		if (TELOPT_OK(i))
		    printf("want DONT %s\n", TELOPT(i));
		else if (TELCMD_OK(i))
		    printf("want DONT %s\n", TELCMD(i));
		else
		    printf("want DONT %d\n", i);
	    }
	} else {
	    if (my_state_is_do(i)) {
		if (TELOPT_OK(i))
		    printf("     DO   %s\n", TELOPT(i));
		else if (TELCMD_OK(i))
		    printf("     DO   %s\n", TELCMD(i));
		else
		    printf("     DO   %d\n", i);
	    }
	}
	if (will_wont_resp[i]) {
	    if (TELOPT_OK(i))
		printf("resp WILL_WONT %s: %d\n", TELOPT(i), will_wont_resp[i]);
	    else if (TELCMD_OK(i))
		printf("resp WILL_WONT %s: %d\n", TELCMD(i), will_wont_resp[i]);
	    else
		printf("resp WILL_WONT %d: %d\n",
				i, will_wont_resp[i]);
	    if (my_want_state_is_will(i)) {
		if (TELOPT_OK(i))
		    printf("want WILL %s\n", TELOPT(i));
		else if (TELCMD_OK(i))
		    printf("want WILL %s\n", TELCMD(i));
		else
		    printf("want WILL %d\n", i);
	    } else {
		if (TELOPT_OK(i))
		    printf("want WONT %s\n", TELOPT(i));
		else if (TELCMD_OK(i))
		    printf("want WONT %s\n", TELCMD(i));
		else
		    printf("want WONT %d\n", i);
	    }
	} else {
	    if (my_state_is_will(i)) {
		if (TELOPT_OK(i))
		    printf("     WILL %s\n", TELOPT(i));
		else if (TELCMD_OK(i))
		    printf("     WILL %s\n", TELCMD(i));
		else
		    printf("     WILL %d\n", i);
	    }
	}
    }

}

char *slcnames[] = { SLC_NAMES };

#ifdef	KERBEROS
static char *authtypes[3] = { "NONE", "PRIVATE", "KERBEROS" };
#endif

void
printsub(direction, pointer, length)
char	direction;		/* '<' or '>' */
unsigned char	*pointer;	/* where suboption data sits */
int	length;			/* length of suboption data */
{
    register int i;

    if (showoptions) {
	if (direction) {
	    fprintf(NetTrace, "%s suboption ",
				(direction == '<')? "Received":"Sent");
	    if (length >= 3) {
		register int j;

		i = pointer[length-2];
		j = pointer[length-1];

		if (i != IAC || j != SE) {
		    fprintf(NetTrace, "(terminated by ");
		    if (TELOPT_OK(i))
			fprintf(NetTrace, "%s ", TELOPT(i));
		    else if (TELCMD_OK(i))
			fprintf(NetTrace, "%s ", TELCMD(i));
		    else
			fprintf(NetTrace, "%d ", i);
		    if (TELOPT_OK(j))
			fprintf(NetTrace, "%s", TELOPT(j));
		    else if (TELCMD_OK(j))
			fprintf(NetTrace, "%s", TELCMD(j));
		    else
			fprintf(NetTrace, "%d", j);
		    fprintf(NetTrace, ", not IAC SE!) ");
		}
	    }
	    length -= 2;
	}
	if (length < 1) {
	    fprintf(NetTrace, "(Empty suboption???)");
	    return;
	}
	switch (pointer[0]) {
	case TELOPT_TTYPE:
	    fprintf(NetTrace, "TERMINAL-TYPE ");
	    switch (pointer[1]) {
	    case TELQUAL_IS:
		fprintf(NetTrace, "IS \"%.*s\"", length-2, (char *)pointer+2);
		break;
	    case TELQUAL_SEND:
		fprintf(NetTrace, "SEND");
		break;
	    default:
		fprintf(NetTrace,
				"- unknown qualifier %d (0x%x).",
				pointer[1], pointer[1]);
	    }
	    break;
	case TELOPT_TSPEED:
	    fprintf(NetTrace, "TERMINAL-SPEED");
	    if (length < 2) {
		fprintf(NetTrace, " (empty suboption???)");
		break;
	    }
	    switch (pointer[1]) {
	    case TELQUAL_IS:
		fprintf(NetTrace, " IS ");
		fprintf(NetTrace, "%.*s", length-2, (char *)pointer+2);
		break;
	    default:
		if (pointer[1] == 1)
		    fprintf(NetTrace, " SEND");
		else
		    fprintf(NetTrace, " %d (unknown)", pointer[1]);
		for (i = 2; i < length; i++)
		    fprintf(NetTrace, " ?%d?", pointer[i]);
		break;
	    }
	    break;

	case TELOPT_LFLOW:
	    fprintf(NetTrace, "TOGGLE-FLOW-CONTROL");
	    if (length < 2) {
		fprintf(NetTrace, " (empty suboption???)");
		break;
	    }
	    switch (pointer[1]) {
	    case 0:
		fprintf(NetTrace, " OFF"); break;
	    case 1:
		fprintf(NetTrace, " ON"); break;
	    default:
		fprintf(NetTrace, " %d (unknown)", pointer[1]);
	    }
	    for (i = 2; i < length; i++)
		fprintf(NetTrace, " ?%d?", pointer[i]);
	    break;

	case TELOPT_NAWS:
	    fprintf(NetTrace, "NAWS");
	    if (length < 2) {
		fprintf(NetTrace, " (empty suboption???)");
		break;
	    }
	    if (length == 2) {
		fprintf(NetTrace, " ?%d?", pointer[1]);
		break;
	    }
	    fprintf(NetTrace, " %d %d (%d)",
		pointer[1], pointer[2],
		(int)((((unsigned int)pointer[1])<<8)|((unsigned int)pointer[2])));
	    if (length == 4) {
		fprintf(NetTrace, " ?%d?", pointer[3]);
		break;
	    }
	    fprintf(NetTrace, " %d %d (%d)",
		pointer[3], pointer[4],
		(int)((((unsigned int)pointer[3])<<8)|((unsigned int)pointer[4])));
	    for (i = 5; i < length; i++)
		fprintf(NetTrace, " ?%d?", pointer[i]);
	    break;

#ifdef	KERBEROS
	case TELOPT_AUTHENTICATION:
	    fprintf(NetTrace, "Authentication information ");
	    switch (pointer[1]) {
	    case TELQUAL_IS:
		switch (pointer[2]) {
		case TELQUAL_AUTHTYPE_NONE:
		case TELQUAL_AUTHTYPE_PRIVATE:
		case TELQUAL_AUTHTYPE_KERBEROS:

			fprintf(NetTrace, "is type %s\r\n", authtypes[pointer[2]]);
			break;
		default:
			fprintf(NetTrace, "is type unknown\r\n");
			break;
		}

	    case TELQUAL_SEND:
	    {
		int	idx = 2;
		fprintf(NetTrace, "- request to send, types");
		for (idx = 2; idx < length - 1; idx++)
			switch (pointer[idx]) {
			case TELQUAL_AUTHTYPE_NONE:
			case TELQUAL_AUTHTYPE_PRIVATE:
			case TELQUAL_AUTHTYPE_KERBEROS:
				fprintf(NetTrace, " %s",
					authtypes[pointer[idx]]);
					break;
			default:
				fprintf(NetTrace, " <unknown %u>",
					pointer[idx]);
				break;
			}
		fprintf(NetTrace, "\r\n");
	    }
		break;

	    default:
		fprintf(NetTrace, " - unknown qualifier %d (0x%x).\r\n",
			pointer[1], pointer[1]);
	    }
	    break;
#endif /* KERBEROS */

	case TELOPT_LINEMODE:
	    fprintf(NetTrace, "LINEMODE ");
	    if (length < 2) {
		fprintf(NetTrace, " (empty suboption???)");
		break;
	    }
	    switch (pointer[1]) {
	    case WILL:
		fprintf(NetTrace, "WILL ");
		goto common;
	    case WONT:
		fprintf(NetTrace, "WONT ");
		goto common;
	    case DO:
		fprintf(NetTrace, "DO ");
		goto common;
	    case DONT:
		fprintf(NetTrace, "DONT ");
	    common:
		if (length < 3) {
		    fprintf(NetTrace, "(no option???)");
		    break;
		}
		switch (pointer[2]) {
		case LM_FORWARDMASK:
		    fprintf(NetTrace, "Forward Mask");
		    for (i = 3; i < length; i++)
			fprintf(NetTrace, " %x", pointer[i]);
		    break;
		default:
		    fprintf(NetTrace, "%d (unknown)", pointer[2]);
		    for (i = 3; i < length; i++)
			fprintf(NetTrace, " %d", pointer[i]);
		    break;
		}
		break;
		
	    case LM_SLC:
		fprintf(NetTrace, "SLC");
		for (i = 2; i < length - 2; i += 3) {
		    if (pointer[i+SLC_FUNC] <= NSLC)
			fprintf(NetTrace, " %s", slcnames[pointer[i+SLC_FUNC]]);
		    else
			fprintf(NetTrace, " %d", pointer[i+SLC_FUNC]);
		    switch (pointer[i+SLC_FLAGS]&SLC_LEVELBITS) {
		    case SLC_NOSUPPORT:
			fprintf(NetTrace, " NOSUPPORT"); break;
		    case SLC_CANTCHANGE:
			fprintf(NetTrace, " CANTCHANGE"); break;
		    case SLC_VARIABLE:
			fprintf(NetTrace, " VARIABLE"); break;
		    case SLC_DEFAULT:
			fprintf(NetTrace, " DEFAULT"); break;
		    }
		    fprintf(NetTrace, "%s%s%s",
			pointer[i+SLC_FLAGS]&SLC_ACK ? "|ACK" : "",
			pointer[i+SLC_FLAGS]&SLC_FLUSHIN ? "|FLUSHIN" : "",
			pointer[i+SLC_FLAGS]&SLC_FLUSHOUT ? "|FLUSHOUT" : "");
		    if (pointer[i+SLC_FLAGS]& ~(SLC_ACK|SLC_FLUSHIN|
						SLC_FLUSHOUT| SLC_LEVELBITS))
			fprintf(NetTrace, "(0x%x)", pointer[i+SLC_FLAGS]);
		    fprintf(NetTrace, " %d;", pointer[i+SLC_VALUE]);
		    if ((pointer[i+SLC_VALUE] == IAC) &&
			(pointer[i+SLC_VALUE+1] == IAC))
				i++;
		}
		for (; i < length; i++)
		    fprintf(NetTrace, " ?%d?", pointer[i]);
		break;

	    case LM_MODE:
		fprintf(NetTrace, "MODE ");
		if (length < 3) {
		    fprintf(NetTrace, "(no mode???)");
		    break;
		}
		{
		    char tbuf[64];
		    sprintf(tbuf, "%s%s%s%s%s",
			pointer[2]&MODE_EDIT ? "|EDIT" : "",
			pointer[2]&MODE_TRAPSIG ? "|TRAPSIG" : "",
			pointer[2]&MODE_SOFT_TAB ? "|SOFT_TAB" : "",
			pointer[2]&MODE_LIT_ECHO ? "|LIT_ECHO" : "",
			pointer[2]&MODE_ACK ? "|ACK" : "");
		    fprintf(NetTrace, "%s", tbuf[1] ? &tbuf[1] : "0");
		}
		if (pointer[2]&~(MODE_EDIT|MODE_TRAPSIG|MODE_ACK))
		    fprintf(NetTrace, " (0x%x)", pointer[2]);
		for (i = 3; i < length; i++)
		    fprintf(NetTrace, " ?0x%x?", pointer[i]);
		break;
	    default:
		fprintf(NetTrace, "%d (unknown)", pointer[1]);
		for (i = 2; i < length; i++)
		    fprintf(NetTrace, " %d", pointer[i]);
	    }
	    break;

	case TELOPT_STATUS: {
	    register char *cp;
	    register int j, k;

	    fprintf(NetTrace, "STATUS");

	    switch (pointer[1]) {
	    default:
		if (pointer[1] == TELQUAL_SEND)
		    fprintf(NetTrace, " SEND");
		else
		    fprintf(NetTrace, " %d (unknown)", pointer[1]);
		for (i = 2; i < length; i++)
		    fprintf(NetTrace, " ?%d?", pointer[i]);
		break;
	    case TELQUAL_IS:
		if (NetTrace == stdout)
		    fprintf(NetTrace, " IS\r\n");
		else
		    fprintf(NetTrace, " IS\n");

		for (i = 2; i < length; i++) {
		    switch(pointer[i]) {
		    case DO:	cp = "DO"; goto common2;
		    case DONT:	cp = "DONT"; goto common2;
		    case WILL:	cp = "WILL"; goto common2;
		    case WONT:	cp = "WONT"; goto common2;
		    common2:
			i++;
			if (TELOPT_OK((int)pointer[i]))
			    fprintf(NetTrace, " %s %s", cp, TELOPT(pointer[i]));
			else
			    fprintf(NetTrace, " %s %d", cp, pointer[i]);

			if (NetTrace == stdout)
			    fprintf(NetTrace, "\r\n");
			else
			    fprintf(NetTrace, "\n");
			break;

		    case SB:
			fprintf(NetTrace, " SB ");
			i++;
			j = k = i;
			while (j < length) {
			    if (pointer[j] == SE) {
				if (j+1 == length)
				    break;
				if (pointer[j+1] == SE)
				    j++;
				else
				    break;
			    }
			    pointer[k++] = pointer[j++];
			}
			printsub(0, &pointer[i], k - i);
			if (i < length) {
			    fprintf(NetTrace, " SE");
			    i = j;
			} else
			    i = j - 1;

			if (NetTrace == stdout)
			    fprintf(NetTrace, "\r\n");
			else
			    fprintf(NetTrace, "\n");

			break;
				
		    default:
			fprintf(NetTrace, " %d", pointer[i]);
			break;
		    }
		}
		break;
	    }
	    break;
	  }

	case TELOPT_XDISPLOC:
	    fprintf(NetTrace, "X-DISPLAY-LOCATION ");
	    switch (pointer[1]) {
	    case TELQUAL_IS:
		fprintf(NetTrace, "IS \"%.*s\"", length-2, (char *)pointer+2);
		break;
	    case TELQUAL_SEND:
		fprintf(NetTrace, "SEND");
		break;
	    default:
		fprintf(NetTrace, "- unknown qualifier %d (0x%x).",
				pointer[1], pointer[1]);
	    }
	    break;

	case TELOPT_ENVIRON:
	    fprintf(NetTrace, "ENVIRON ");
	    switch (pointer[1]) {
	    case TELQUAL_IS:
		fprintf(NetTrace, "IS ");
		goto env_common;
	    case TELQUAL_SEND:
		fprintf(NetTrace, "SEND ");
		goto env_common;
	    case TELQUAL_INFO:
		fprintf(NetTrace, "INFO ");
	    env_common:
		{
		    register int noquote = 2;
		    for (i = 2; i < length; i++ ) {
			switch (pointer[i]) {
			case ENV_VAR:
			    if (pointer[1] == TELQUAL_SEND)
				goto def_case;
			    fprintf(NetTrace, "\" VAR " + noquote);
			    noquote = 2;
			    break;

			case ENV_VALUE:
			    fprintf(NetTrace, "\" VALUE " + noquote);
			    noquote = 2;
			    break;

			case ENV_ESC:
			    fprintf(NetTrace, "\" ESC " + noquote);
			    noquote = 2;
			    break;

			default:
			def_case:
			    if (isprint(pointer[i]) && pointer[i] != '"') {
				if (noquote) {
				    putc('"', NetTrace);
				    noquote = 0;
				}
				putc(pointer[i], NetTrace);
			    } else {
				fprintf(NetTrace, "\" %03o " + noquote,
							pointer[i]);
				noquote = 2;
			    }
			    break;
			}
		    }
		    if (!noquote)
			putc('"', NetTrace);
		    break;
		}
	    }
	    break;

	default:
	    fprintf(NetTrace, "Unknown option ");
	    for (i = 0; i < length; i++)
		fprintf(NetTrace, " %d", pointer[i]);
	    break;
	}
	if (direction) {
	    if (NetTrace == stdout)
		fprintf(NetTrace, "\r\n");
	    else
		fprintf(NetTrace, "\n");
	}
    }
}

/* EmptyTerminal - called to make sure that the terminal buffer is empty.
 *			Note that we consider the buffer to run all the
 *			way to the kernel (thus the select).
 */

void
EmptyTerminal()
{
#if	defined(unix)
    fd_set	o;

    FD_ZERO(&o);
#endif	/* defined(unix) */

    if (TTYBYTES() == 0) {
#if	defined(unix)
	FD_SET(tout, &o);
	(void) select(tout+1, (fd_set *) 0, &o, (fd_set *) 0,
			(struct timeval *) 0);	/* wait for TTLOWAT */
#endif	/* defined(unix) */
    } else {
	while (TTYBYTES()) {
	    (void) ttyflush(0);
#if	defined(unix)
	    FD_SET(tout, &o);
	    (void) select(tout+1, (fd_set *) 0, &o, (fd_set *) 0,
				(struct timeval *) 0);	/* wait for TTLOWAT */
#endif	/* defined(unix) */
	}
    }
}

void
SetForExit()
{
    setconnmode(0);
#if	defined(TN3270)
    if (In3270) {
	Finish3270();
    }
#else	/* defined(TN3270) */
    do {
	(void)telrcv();			/* Process any incoming data */
	EmptyTerminal();
    } while (ring_full_count(&netiring));	/* While there is any */
#endif	/* defined(TN3270) */
    setcommandmode();
    fflush(stdout);
    fflush(stderr);
#if	defined(TN3270)
    if (In3270) {
	StopScreen(1);
    }
#endif	/* defined(TN3270) */
    setconnmode(0);
    EmptyTerminal();			/* Flush the path to the tty */
    setcommandmode();
}

void
Exit(returnCode)
int returnCode;
{
    SetForExit();
    exit(returnCode);
}

void
ExitString(string, returnCode)
char *string;
int returnCode;
{
    SetForExit();
    fwrite(string, 1, strlen(string), stderr);
    exit(returnCode);
}
