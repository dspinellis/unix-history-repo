#include <X/mit-copyright.h>
/* Copyright 1985 Massachusetts Institute of Technology	*/
#ifndef lint
static char *rcsid_xhost_c = "$Header: xhost.c,v 10.12 86/11/19 19:24:01 jg Rel $";
#endif
 
#include <signal.h>
#include <setjmp.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>
#include <netdb.h>
#include <netinet/in.h>
#ifdef DNETCONN
#include <netdnet/dn.h>
#include <netdnet/dnetdb.h>
#endif
#include <X/Xlib.h>
#include <X/Xproto.h>
 
char *index();
int local_xerror();

#define NAMESERVER_TIMEOUT 5	/* time to wait for nameserver */
int nameserver_timedout;
 
main(argc, argv)
	int argc;
	char **argv;
{
	Display *dpy;
	char host[256];
	register char *arg;
	int display, i, w, nhosts;
	char *address, *get_address();
	char *hostname, *get_hostname();
	struct in_addr *list, *XGetHosts();
#ifdef DNETCONN
	char *dnet_htoa();
	struct nodeent *np;
	struct dn_naddr *nlist, dnaddr, *dnaddrp, *XGetNodes(), *dnet_addr();
	char *cp, *index();
#endif
 
	if ((dpy = XOpenDisplay(NULL)) == NULL) {
	    fprintf(stderr, "%s: Can't open display '%s'\n",
		    argv[0], XDisplayName("\0"));
	    exit(1);
	}
 
	XErrorHandler(local_xerror);
 
 
	if (argc == 1) {
		/*
		 * Get all the INET host names
		 */
		list = XGetHosts(&nhosts);
		if (nhosts != 0) {
		    sethostent(1); /* don't close the data base each time */
		    for (i = 0; i < nhosts; i++ )  {
		      hostname = get_hostname(list[i]);
		      printf("%s\t", hostname);
		      if (nameserver_timedout)
			printf("(nameserver did not respond in %d seconds)\n",
			        NAMESERVER_TIMEOUT);
		      else printf("\n");
		    }
		    endhostent();
		}
#ifdef DNETCONN
		/*
		 * Get all the DECnet node names
		 */
		nlist = XGetNodes(&nhosts);
		if (nhosts != 0) {
		    setnodeent(1); /* keep the database accessed */
		    for (i = 0; i < nhosts; i++ )  {
			printf("%s::\n", dnet_htoa(&nlist[i]));
		    }
		}
#endif
		exit(0);
	}
 
	for (i = 1; i < argc; i++) {
	    arg = argv[i];
	    if (*arg == '-') {
		arg++;
#ifdef DNETCONN
		if ((cp = index(arg, ':')) && (*(cp + 1) == ':')) {
		    *cp = '\0';
		    if (dnaddrp = dnet_addr(arg)) {
			XRemoveNode(dnaddrp);
		    } else {
			if ((np = getnodebyname (arg)) == NULL) {
			    printf("xhost:bad node: %s::\n", arg);
			} else {
			    dnaddr.a_len = np->n_length;
			    bcopy (np->n_addr, dnaddr.a_addr, np->n_length);
			    XRemoveNode(&dnaddr);
			}
		    }
		} else {
#endif
                    if ((address = get_address(arg)) == NULL) 
		         fprintf(stderr, "%s: bad host: %s\n", argv[0], arg);
                    else XRemoveHost(address);
#ifdef DNETCONN
		}
#endif
	    } else {
		if (*arg == '+') arg++;
#ifdef DNETCONN
		if ((cp = index(arg, ':')) && (*(cp + 1) == ':')) {
		    *cp = '\0';
		    if (dnaddrp = dnet_addr(arg)) {
			XAddNode(dnaddrp);
		    } else {
			if ((np = getnodebyname (arg)) == NULL) {
			    printf("xhost:bad node: %s::\n", arg);
			} else {
			    dnaddr.a_len = np->n_length;
			    bcopy (np->n_addr, dnaddr.a_addr, np->n_length);
			    XAddNode(&dnaddr);
			}
		    }
		} else {
#endif
                    if ((address = get_address(arg)) == NULL) 
		         fprintf(stderr, "%s: bad host: %s\n", argv[0], arg);
                    else XAddHost(address);
#ifdef DNETCONN
		}
#endif
	    }
	}
	XSync(0);
	exit(0);
}

 

/*
 * get_address - return a pointer to an internet address given
 * either a name (CHARON.MIT.EDU) or a string with the raw
 * address (18.58.0.13)
 */

char *get_address (name) 
char *name;
{
  struct hostent *hp;
  static unsigned long address;

  /*
   * First see if inet_addr() can grok the name; if so, then use it.
   */
  if ((address = inet_addr(name)) != -1) {
    return((char *)&address);	/* Found it */
  } 
  /*
   * Is it in the namespace?
   */
  else if (((hp = gethostbyname(name)) == (struct hostent *)NULL)
       || hp->h_addrtype != AF_INET) {
    return (NULL);		/* Sorry, you lose */
  } else {
    return (hp->h_addr);	/* Found it. */
  }
}


/*
 * get_hostname - Given an internet address, return a name (CHARON.MIT.EDU)
 * or a string representing the address (18.58.0.13) if the name cannot
 * be found.
 */

jmp_buf env;

char *get_hostname (address)
struct in_addr *address;
{
  struct hostent *hp = NULL;
  int nameserver_lost();
  char *inet_ntoa();
  /* gethostbyaddr can take a LONG time if the host does not exist.
     Assume that if it does not respond in NAMESERVER_TIMEOUT seconds
     that something is wrong and do not make the user wait.
     gethostbyaddr will continue after a signal, so we have to
     jump out of it. 
   */
  nameserver_timedout = 0;
  signal(SIGALRM, nameserver_lost);
  alarm(4);
  if (setjmp(env) == 0) {
    hp = gethostbyaddr (&address, sizeof(struct in_addr), AF_INET);
  }
  alarm(0);
  if (hp)
    return (hp->h_name);
  else return (inet_ntoa(address));
}

nameserver_lost()
{
  nameserver_timedout = 1;
  longjmp(env, -1);
}

/*
 * local_xerror - local non-fatal error handling routine. If the error was
 * that an X_GetHosts request for an unknown address format was received, just
 * return, otherwise call the default error handler _XError.
 */
local_xerror (dpy, rep)
    Display *dpy;
    XErrorEvent *rep;
{
    if ((rep->error_code == BadValue) && (rep->request_code == X_GetHosts)) {
	return;
    } else {
	_XError(dpy, rep);
    }
}

