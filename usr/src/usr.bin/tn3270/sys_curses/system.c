#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/wait.h>

#include <errno.h>
extern int errno;

#include <netdb.h>
#include <signal.h>
#include <stdio.h>
#include <pwd.h>

#include "../general/general.h"
#include "../api/api.h"
#include "api_exch.h"

#include "../general/globals.h"


static int shell_pid = 0;

static char *ourENVlist[200];		/* Lots of room */

static int sock = -1;

static enum { DEAD, UNCONNECTED, CONNECTED } state;

static int
    storage_address,		/* Address we have */
    storage_length = 0,		/* Length we have */
    storage_must_send = 0,	/* Storage belongs on other side of wire */
    storage_accessed = 0;	/* The storage is accessed (so leave alone)! */

static long storage[250];

union REGS inputRegs;
struct SREGS inputSregs;

kill_connection()
{
    state = DEAD;
    (void) close(sock);
    sock = -1;
}


static int
child_died()
{
    union wait *status;
    register int pid;

    while ((pid = wait3(&status, WNOHANG, 0)) > 0) {
	if (pid == shell_pid) {
	    char inputbuffer[100];

	    shell_active = 0;
	    if (sock != -1) {
		(void) close(sock);
		sock = -1;
		printf("[Hit return to continue]");
		fflush(stdout);
		(void) gets(inputbuffer);
		setconnmode();
		ConnectScreen();	/* Turn screen on (if need be) */
	    }
	}
    }
    signal(SIGCHLD, child_died);
}

static int
nextchar()
{
    unsigned char c;

    if (read(sock, &c, 1) != 1) {
	return -1;
    } else {
	return c;
    }
}

static int
checktype(type)
int type;
{
    int was;

    if ((was = nextchar()) != type) {
	fprintf(stderr, "Wrong type of data.  Should be 0x%02x, was 0x%02x.\n",
		type, was);
	return -1;
    } else {
	return 0;
    }
}


static int
fill(where, length)
char	*where;
int	length;
{
    while (length) {
	int i;

	if ((i = read(sock, where, length)) < 0) {
	    perror("read");
	    return -1;
	} else {
	    length -= i;
	    where += i;
	}
    }
}


static int
nextlength()
{
    short length;

    if (fill(&length, sizeof length) == -1) {
	return -1;
    } else {
	return ntohs(length);
    }
}


static int
nextaddress()
{
    long address;

    return fill(&address, sizeof address);
}



static int
nextbytes(type, where, length)
int	type;
char	*where;
int	length;
{
    int was;

    if (checktype(type) == -1) {
	return -1;
    }
    if ((was = nextlength()) != length) {
	fprintf(stderr, "Type 0x%02x had bad length.  Should be %d, was %d.\n",
		type, length, was);
	return -1;
    } else {
	return fill(where, length);
    }
}

static int
nextstore()
{
    if (nextchar() != EXCH_HEREIS) {
	fprintf(stderr, "Bad data from other side.\n");
	fprintf(stderr, "(Encountered at %s, %s.)\n", __FILE__, __LINE__);
	return -1;
    }
    if ((storage_address = nextaddress()) == -1) {
	storage_length = 0;
	return -1;
    }
    if ((storage_length = nextlength()) > sizeof storage) {
	fprintf(stderr, "API client tried to send too much storage (%d).\n",
		storage_length);
	return -1;
    }
    return fill((char *)storage, storage_length);
}


static int
doreject(message)
char	*message;
{
    int length = strlen(message);
    char buffer[100];

    length = htons(length);
    sprintf(buffer, "%c%c%c%s", EXCH_REJECTED, length>>8, length&0xff, buffer);
    if (write(sock, buffer, length+3) != length+3) {
	perror("writing API socket");
	return -1;
    }
    return 0;
}


/*
 * doconnect()
 *
 * Negotiate with the other side and try to do something.
 */

static int
doconnect()
{
    struct passwd *pwent;
    char
	promptbuf[100],
	buffer[200];
    int promptlen, passwdlen;
    int length;
    int was;

    if ((pwent = getpwuid(geteuid())) == 0) {
	return -1;
    }
    sprintf(promptbuf, "Enter password for user %s:", pwent->pw_name);
    promptlen = strlen(promptbuf);
    passwdlen = strlen(pwent->pw_name);
    sprintf(buffer, "%c%c%c%s%c%c%s", EXCH_SEND_AUTH,
		promptlen>>8, promptlen&0xff, promptbuf,
		passwdlen>>8, passwdlen&0xff, pwent->pw_name);
    length = strlen(buffer);
    if (write(sock, buffer, length) != length) {
	perror("write to API socket");
	return -1;
    }
    if ((was = nextchar()) != EXCH_AUTH) {
	fprintf(stderr,
	    "API client sent command 0x%02x when EXCH_AUTH expected.\n", was);
    }
    if ((length = nextlength()) > sizeof buffer) {
	doreject("Password entered was too long");
	return 0;
    }
    if (fill(buffer, length) == -1) {
	return -1;
    }
    buffer[length] = 0;

    /* Is this the correct password? */
    if (strcmp(crypt(buffer, pwent->pw_passwd), pwent->pw_passwd) == 0) {
	char code = EXCH_CONNECTED;
	if (write(sock, &code, 1) != 1) {
	    perror("writing to API socket");
	    return -1;
	}
    } else {
	doreject("Invalid password");
	sleep(10);		/* Don't let us do too many of these */
    }
    return 0;
}


void
freestorage()
{
    int i, j;
    char buffer[40];

    if (storage_accessed) {
	fprintf(stderr, "Internal error - attempt to free accessed storage.\n");
	fprintf(stderr, "(Enountered in file %s at line %s.)\n",
			__FILE__, __LINE__);
	quit();
    }
    if (storage_must_send == 0) {
	return;
    }
    storage_must_send = 0;
    i = htonl(storage_address);
    j = htonl(storage_length);
    sprintf(buffer, "%c%c%c%c%c%c%c",
		EXCH_HEREIS, i>>24, i>>16, i>>8, i, j>>8, j);
    if (write(sock, buffer, 5) != 5) {
	perror("writing to API socket");
	kill_connection();
	return;
    }
    if (write(sock, (char *)storage, storage_length) != storage_length) {
	perror("writing to API socket");
	kill_connection();
	return;
    }
}


void
getstorage(address, length)
{
    int i, j;
    char buffer[40];

    freestorage();
    if (storage_accessed) {
	fprintf(stderr,
		"Internal error - attempt to get while storage accessed.\n");
	fprintf(stderr, "(Enountered in file %s at line %s.)\n",
			__FILE__, __LINE__);
	quit();
    }
    if (storage_must_send == 0) {
	return;
    }
    storage_must_send = 0;
    i = htonl(storage_address);
    j = htonl(storage_length);
    sprintf(buffer, "%c%c%c%c%c%c%c",
		EXCH_GIMME, i>>24, i>>16, i>>8, i, j>>8, j);
    if (write(sock, buffer, 5) != 5) {
	perror("writing to API socket");
	kill_connection();
	return;
    }
    if (nextstore() == -1) {
	kill_connection();
	return;
    }
}

void
movetous(local, es, di, length)
char
    *local;
int
    es,
    di;
int
    length;
{
    if (length > sizeof storage) {
	fprintf(stderr, "Internal API error - movetous() length too long.\n");
	fprintf(stderr, "(detected in file %s, line %d)\n", __FILE__, __LINE__);
	quit();
    } else if (length == 0) {
	return;
    }
    getstorage(di, length);
    memcpy(local, storage+(di-storage_address), length);
}

void
movetothem(es, di, local, length)
int
    es,
    di;
char
    *local;
int
    length;
{
    if (length > sizeof storage) {
	fprintf(stderr, "Internal API error - movetothem() length too long.\n");
	fprintf(stderr, "(detected in file %s, line %d)\n", __FILE__, __LINE__);
	quit();
    } else if (length == 0) {
	return;
    }
    freestorage();
    memcpy((char *)storage, local, length);
    storage_length = length;
    storage_address = di;
    storage_must_send = 1;
}


char *
access_api(location, length)
int
    location,
    length;
{
    if (storage_accessed) {
	fprintf(stderr, "Internal error - storage accessed twice\n");
	fprintf(stderr, "(Encountered in file %s, line %s.)\n",
				__FILE__, __LINE__);
	quit();
    } else if (length != 0) {
	storage_accessed = 1;
	freestorage();
	getstorage(location, length);
    }
    return (char *) storage;
}

unaccess_api(location, local, length)
int	location;
char	*local;
int	length;
{
    if (storage_accessed == 0) {
	fprintf(stderr, "Internal error - unnecessary unaccess_api call.\n");
	fprintf(stderr, "(Encountered in file %s, line %s.)\n",
			__FILE__, __LINE__);
	quit();
    }
    storage_accessed = 0;
    storage_must_send = 1;	/* Needs to go back */
}


/*
 * shell_continue() actually runs the command, and looks for API
 * requests coming back in.
 *
 * We are called from the main loop in telnet.c.
 */

int
shell_continue()
{
    switch (state) {
    case DEAD:
	pause();			/* Nothing to do */
	break;
    case UNCONNECTED:
	if (nextchar() != EXCH_CONNECT) {
	    kill_connection();
	} else {
	    switch (doconnect()) {
	    case -1:
		kill_connection();
		break;
	    case 0:
		break;
	    case 1:
		state = CONNECTED;
	    }
	}
	break;
    case CONNECTED:
	if (nextchar() == EXCH_REQUEST) {
	    /* Eat up request packet. */
	    nextbytes(&inputRegs, sizeof inputRegs);
	    nextbytes(&inputSregs, sizeof inputSregs);
	    nextstore();		/* Initial storage sent */
	    handle_api(&inputRegs, &inputSregs);
	} else {
	    kill_connection();
	}
    }
    return shell_active;
}


/*
 * Called from telnet.c to fork a lower command.com.  We
 * use the spint... routines so that we can pick up
 * interrupts generated by application programs.
 */


int
shell(argc,argv)
int	argc;
char	*argv[];
{
    int serversock, length;
    struct sockaddr_in server;
    char sockNAME[100];
    static char **whereAPI = 0;

    /* First, create the socket which will be connected to */
    serversock = socket(AF_INET, SOCK_STREAM, 0);
    if (serversock < 0) {
	perror("opening API socket");
	return 0;
    }
    server.sin_family = AF_INET;
    server.sin_addr.s_addr = INADDR_ANY;
    server.sin_port = 0;
    if (bind(serversock, &server, sizeof server) < 0) {
	perror("binding API socket");
	return 0;
    }
    length = sizeof server;
    if (getsockname(serversock, &server, &length) < 0) {
	perror("getting API socket name");
	(void) close(serversock);
    }
    listen(serversock, 1);
    /* Get name to advertise in address list */
    strcpy(sockNAME, "API3270=");
    gethostname(sockNAME+strlen(sockNAME), sizeof sockNAME-strlen(sockNAME));
    if (strlen(sockNAME) > (sizeof sockNAME-10)) {
	fprintf(stderr, "Local hostname too large; using 'localhost'.\n");
	strcpy(sockNAME, "localhost");
    }
    sprintf(sockNAME+strlen(sockNAME), ":%d", ntohs(server.sin_port));

    if (whereAPI == 0) {
	char **ptr, **nextenv;
	extern char **environ;

	ptr = environ;
	nextenv = ourENVlist;
	while (*ptr) {
	    if (nextenv >= &ourENVlist[highestof(ourENVlist)-1]) {
		fprintf(stderr, "Too many environmental variables\n");
		break;
	    }
	    *nextenv++ = *ptr++;
	}
	whereAPI = nextenv++;
	*nextenv++ = 0;
	environ = ourENVlist;		/* New environment */
    }
    *whereAPI = sockNAME;

    child_died();			/* Start up signal handler */
    shell_active = 1;			/* We are running down below */
    if (shell_pid = vfork()) {
	if (shell_pid == -1) {
	    perror("vfork");
	    (void) close(serversock);
	} else {
	    fd_set fdset;
	    int i;

	    FD_ZERO(&fdset);
	    FD_SET(serversock, &fdset);
	    while (shell_active) {
		if ((i = select(serversock+1, &fdset, 0, 0, 0)) < 0) {
		    if (errno = EINTR) {
			continue;
		    } else {
			perror("in select waiting for API connection");
			break;
		    }
		} else {
		    i = accept(serversock, 0, 0);
		    if (i == -1) {
			perror("accepting API connection");
		    }
		    sock = i;
		    break;
		}
	    }
	    (void) close(serversock);
	    /* If the process has already exited, we may need to close */
	    if ((shell_active == 0) && (sock != -1)) {
		(void) close(sock);
		sock = -1;
		setcommandmode();	/* In case child_died sneaked in */
	    }
	}
    } else {				/* New process */
	register int i;

	for (i = 3; i < 30; i++) {
	    (void) close(i);
	}
	if (argc == 1) {		/* Just get a shell */
	    char *cmdname;
	    extern char *getenv();

	    cmdname = getenv("SHELL");
	    execlp(cmdname, cmdname, 0);
	    perror("Exec'ing new shell...\n");
	    exit(1);
	} else {
	    execvp(argv[1], &argv[1]);
	    perror("Exec'ing command.\n");
	    exit(1);
	}
	/*NOTREACHED*/
    }
    return shell_active;		/* Go back to main loop */
}
