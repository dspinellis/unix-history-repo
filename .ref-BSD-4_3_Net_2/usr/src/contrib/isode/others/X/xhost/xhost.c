/*

Copyright 1985, 1986, 1987 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.
M.I.T. makes no representations about the suitability of
this software for any purpose.  It is provided "as is"
without express or implied warranty.

*/

#ifndef lint
static char *rcsid_xhost_c = "$XConsortium: xhost.c,v 11.28 88/10/11 12:48:06 jim Exp $";
#endif
 
#include <signal.h>
#include <setjmp.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>
#include <netdb.h>
#include <netinet/in.h>
#ifdef notdef
#include <arpa/inet.h>
	bogus definition of inet_makeaddr() in BSD 4.2 and Ultrix
#else
extern unsigned long inet_makeaddr();
#endif
#ifdef DNETCONN
#include <netdnet/dn.h>
#include <netdnet/dnetdb.h>
#endif
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xproto.h>
#include <X11/Xmu.h>
#ifdef ISOCONN
#include <isode/psap.h>
#include <isode/tsap.h>
#include <isode/isoservent.h>
#include "../server/osdep.h"
#endif /* ISOCONN */
 
static int local_xerror();

#define NAMESERVER_TIMEOUT 5	/* time to wait for nameserver */

typedef struct {
	int af, xf;
} FamilyMap;

static FamilyMap familyMap[] = {
#ifdef	AF_DECnet
    {AF_DECnet, FamilyDECnet},
#endif
#ifdef	AF_CHAOS
    {AF_CHAOS, FamilyChaos},
#endif
#ifdef AF_OSI
    {AF_OSI, FamilyIso},
#endif
#ifdef	AF_INET
    {AF_INET, FamilyInternet}
#endif
};

#define FAMILIES ((sizeof familyMap)/(sizeof familyMap[0]))

int nameserver_timedout;
 
char *ProgramName;

static int XFamily(af)
    int af;
{
    int i;
    for (i = 0; i < FAMILIES; i++)
	if (familyMap[i].af == af)
	    return familyMap[i].xf;
    return -1;
}


main(argc, argv)
	int argc;
	char **argv;
{
	Display *dpy;
	char host[256];
	register char *arg;
	int display, i, w, nhosts;
	char *hostname, *get_hostname();
	XHostAddress *list;
	Bool enabled = False;
#ifdef DNETCONN
	char *dnet_htoa();
	struct nodeent *np;
	struct dn_naddr *nlist, dnaddr, *dnaddrp, *dnet_addr();
	char *cp;
#endif
 
	ProgramName = argv[0];

#ifdef ISOCONN
/*
 * XXX Shouldd parse -display display here
 */
	if ((dpy = XOpenDisplay("perky:X0")) == NULL) {
#else
	if ((dpy = XOpenDisplay(NULL)) == NULL) {
#endif
	    fprintf(stderr, "%s:  unable to open display \"%s\"\n",
		    ProgramName, XDisplayName (NULL));
	    exit(1);
	}

	XSetErrorHandler(local_xerror);
 
 
	if (argc == 1) {
#ifdef DNETCONN
		setnodeent(1); /* keep the database accessed */
#endif
		sethostent(1); /* don't close the data base each time */
		list = XListHosts(dpy, &nhosts, &enabled);
		printf ("access control %s\n", 
			(enabled ? 
			 "enabled (only the following hosts are allowed)": 
			 "disabled (any host is allowed)"));
		if (nhosts != 0) {
		    for (i = 0; i < nhosts; i++ )  {
		      hostname = get_hostname(&list[i]);
		      printf("%s\t", hostname);
		      if (nameserver_timedout)
			printf("(nameserver did not respond in %d seconds)\n",
			        NAMESERVER_TIMEOUT);
		      else printf("\n");
		    }
		    free(list);
		    endhostent();
		}
		exit(0);
	}
 
	for (i = 1; i < argc; i++) {
	    arg = argv[i];
	    if (*arg == '-') {
	    
	        if (!argv[i][1] && ((i+1) == argc)) {
		    printf ("all hosts being restricted (access control enabled)\n");
		    XEnableAccessControl(dpy);
		} else {
		    arg = argv[i][1]? &argv[i][1] : argv[++i];
		    if (!change_host (dpy, arg, False)) {
			fprintf (stderr, "%s:  bad hostname \"%s\"\n",
				 ProgramName, arg);
		    }
		}
	    } else {
	        if (*arg == '+' && !argv[i][1] && ((i+1) == argc)) {
		    printf ("all hosts being allowed (access control disabled)\n");
		    XDisableAccessControl(dpy);
		} else {
		    if (*arg == '+') {
		      arg = argv[i][1]? &argv[i][1] : argv[++i];
		    }
		    if (!change_host (dpy, arg, True)) {
			fprintf (stderr, "%s:  bad hostname \"%s\"\n",
				 ProgramName, arg);
		    }
		}
	    }
	}
	XCloseDisplay (dpy);  /* does an XSync first */
	exit(0);
}

 

/*
 * change_host - edit the list of hosts that may connect to the server;
 * it parses DECnet names (expo::), Internet addresses (18.30.0.212), or
 * Internet names (expo.lcs.mit.edu); if 4.3bsd macro h_addr is defined
 * (from <netdb.h>), it will add or remove all addresses with the given
 * address.
 */
#ifdef ISOCONN
/*
 * ISO T-Service is selected by server:T-Name:Display
 * where T-Name must start non numeric (i.e. default is by ommission
 *
 */
#endif /* ISOCONN */

int change_host (dpy, name, add)
    Display *dpy;
    char *name;
    Bool add;
{
  struct hostent *hp;
  XHostAddress ha;
  static struct in_addr addr;	/* so we can point at it */
#ifdef DNETCONN
  struct dn_naddr *dnaddrp;
  struct nodeent *np;
  char *cp;
  static struct dn_naddr dnaddr;
#endif /* DNETCONN */
  static char *add_msg = "being added to access control list";
  static char *remove_msg = "being removed from access control list";
#ifdef ISOCONN
  char *cp;
#endif /* ISOCONN */

#ifdef DNETCONN
  if ((cp = index (name, ':')) && (*(cp + 1) == ':')) {
    *cp = '\0';
    ha.family = FamilyDECnet;
    if (dnaddrp = dnet_addr(name)) {
      dnaddr = *dnaddrp;
    } else {
      if ((np = getnodebyname (name)) == NULL) {
	  fprintf (stderr, "%s:  unble to get node name for \"%s::\"\n",
		   ProgramName, name);
	  return 0;
      }
      dnaddr.a_len = np->n_length;
      bcopy (np->n_addr, dnaddr.a_addr, np->n_length);
    }
    ha.length = sizeof(struct dn_naddr);
    ha.address = (char *)&dnaddr;
    if (add) {
	XAddHost (dpy, &ha);
	printf ("%s:: %s\n", name, add_msg);
    } else {
	XRemoveHost (dpy, &ha);
	printf ("%s:: %s\n", name, remove_msg);
    }
    return 1;
  }
#endif /* DNETCONN */
#ifdef ISOCONN
/*
 * TServiceNameDisplayNumber...
 */
  if ((cp = index (name, ':')) && ( (*(cp + 1) == 'X')|| (*(cp + 1) == 'T'))) {
    AEI aei;
    struct PSAPaddr *pa;
    *(cp++) = '\0';
    ha.family = FamilyIso;
    if (cp)
        aei = str2aei(name, cp);
    else
	aei = str2aei(name, DEFAULTTSERVICE);
    if (aei == NULLAEI) {
	  fprintf (stderr, "%s:  unble to get AEI for \"%s::\"\n",
		   ProgramName, name);
	  return 0;
    }
    if ((pa = aei2addr (aei)) == NULLPA) {
	fprintf (stderr, "address translation failed");
	return 0;
    }
    ha.length = NASIZE;
    ha.address = (char *)&(pa->pa_addr.sa_addr);
/*
 * XXXX
 */
{
int i; char *hp = ha.address;
for(i=0;i<sizeof(struct TSAPaddr); i++) {
	fprintf(stderr, "%x ", *hp & 0xff);
	hp++;
}
fprintf(stderr, "\n");
}
    if (add) {
	XAddHost (dpy, &ha);
	printf ("%s:: %s\n", name, add_msg);
    } else {
	XRemoveHost (dpy, &ha);
	printf ("%s:: %s\n", name, remove_msg);
    }
    return 1;
  }
#endif /* ISOCONN */
  /*
   * First see if inet_addr() can grok the name; if so, then use it.
   */
  if ((addr.s_addr = inet_addr(name)) != -1) {
    ha.family = FamilyInternet;
    ha.length = sizeof(addr.s_addr);
    ha.address = (char *)&addr.s_addr;
    if (add) {
	XAddHost (dpy, &ha);
	printf ("%s %s\n", name, add_msg);
    } else {
	XRemoveHost (dpy, &ha);
	printf ("%s %s\n", name, remove_msg);
    }
    return 1;
  } 
  /*
   * Is it in the namespace?
   */
  else if (((hp = gethostbyname(name)) == (struct hostent *)NULL)
       || hp->h_addrtype != AF_INET) {
    return 0;
  } else {
    ha.family = XFamily(hp->h_addrtype);
    ha.length = hp->h_length;
#ifdef h_addr				/* new 4.3bsd version of gethostent */
    {
	char **list;

	/* iterate over the hosts */
	for (list = hp->h_addr_list; *list; list++) {
	    ha.address = *list;
	    if (add) {
		XAddHost (dpy, &ha);
	    } else {
		XRemoveHost (dpy, &ha);
	    }
	}
    }
#else
    ha.address = hp->h_addr;
    if (add) {
	XAddHost (dpy, &ha);
    } else {
	XRemoveHost (dpy, &ha);
    }
#endif
    printf ("%s %s\n", name, add ? add_msg : remove_msg);
    return 1;
  }
  /* NOTREACHED */
}


/*
 * get_hostname - Given an internet address, return a name (CHARON.MIT.EDU)
 * or a string representing the address (18.58.0.13) if the name cannot
 * be found.
 */

jmp_buf env;

char *get_hostname (ha)
XHostAddress *ha;
{
  struct hostent *hp = NULL;
  int nameserver_lost();
  char *inet_ntoa();
#ifdef DNETCONN
  struct nodeent *np;
  static char nodeaddr[16];
#endif /* DNETCONN */

  if (ha->family == FamilyInternet) {
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
      hp = gethostbyaddr (ha->address, ha->length, AF_INET);
    }
    alarm(0);
    if (hp)
      return (hp->h_name);
    else return (inet_ntoa(*((struct in_addr *)(ha->address))));
  }
#ifdef DNETCONN
  if (ha->family == FamilyDECnet) {
    if (np = getnodebyaddr(ha->address, ha->length, AF_DECnet)) {
      sprintf(nodeaddr, "%s::", np->n_name);
    } else {
      sprintf(nodeaddr, "%s::", dnet_htoa(ha->address));
    }
    return(nodeaddr);
  }
#endif
#ifdef ISOCONN
/*
 * We really truly need an nsap to AEI here...
 */
    if (ha->family == FamilyIso) {
	return(ha->address);
    }
#endif /* ISOCONN */

  return (NULL);
}

nameserver_lost()
{
  nameserver_timedout = 1;
  longjmp(env, -1);
}

/*
 * local_xerror - local non-fatal error handling routine. If the error was
 * that an X_GetHosts request for an unknown address format was received, just
 * return, otherwise print the normal error message and continue.
 */
static int local_xerror (dpy, rep)
    Display *dpy;
    XErrorEvent *rep;
{
    if ((rep->error_code == BadAccess) && (rep->request_code == X_ChangeHosts)) {
	fprintf (stderr, 
		 "%s:  must be on local machine to add or remove hosts.\n",
		 ProgramName);
	return 1;
    } else if ((rep->error_code == BadAccess) && 
	       (rep->request_code == X_SetAccessControl)) {
	fprintf (stderr, 
	"%s:  must be on local machine to enable or disable access control.\n",
		 ProgramName);
	return 1;
    } else if ((rep->error_code == BadValue) && 
	       (rep->request_code == X_ListHosts)) {
	return 1;
    }

    XmuPrintDefaultErrorMessage (dpy, rep, stderr);
    return 0;
}
