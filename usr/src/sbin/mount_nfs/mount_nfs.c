/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1992 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mount_nfs.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include <signal.h>
#include <strings.h>
#include <sys/syslog.h>
#include <sys/param.h>
#include <sys/file.h>
#include <sys/errno.h>
#include <sys/ucred.h>
#include <sys/mount.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#include <netdb.h>
#include <rpc/rpc.h>
#include <rpc/pmap_clnt.h>
#include <rpc/pmap_prot.h>
#ifdef ISO
#include <netiso/iso.h>
#endif
#ifdef KERBEROS
#include <kerberosIV/krb.h>
#endif
#include <nfs/rpcv2.h>
#include <nfs/nfsv2.h>
#include <nfs/nfs.h>
#include <nfs/nqnfs.h>

int xdr_dir(), xdr_fh();
struct nfs_args nfsdefargs = {
	(struct sockaddr *)0,
	sizeof (struct sockaddr_in),
	SOCK_DGRAM,
	0,
	(nfsv2fh_t *)0,
	0,
	NFS_WSIZE,
	NFS_RSIZE,
	NFS_TIMEO,
	NFS_RETRANS,
	NFS_MAXGRPS,
	NFS_DEFRAHEAD,
	NQ_DEFLEASE,
	NQ_DEADTHRESH,
	(char *)0,
};

struct nfhret {
	u_long	stat;
	nfsv2fh_t nfh;
};
#define	DEF_RETRY	10000
#define	BGRND	1
#define	ISBGRND	2
int retrycnt = DEF_RETRY;
int opflags = 0;
extern int errno;

#ifdef ISO
struct iso_addr *iso_addr();
#endif

#ifdef KERBEROS
char inst[INST_SZ];
char realm[REALM_SZ];
KTEXT_ST kt;
#endif

main(argc, argv, arge)
	int argc;
	char **argv;
	char **arge;
{
	struct nfs_args nfsargs;
	register int c;
	register struct nfs_args *nfsargsp = &nfsargs;
	struct nfsd_cargs ncd;
	int num, flags = 0, match = 1, i, nfssvc_flag;
	char *spec, *name;
	uid_t last_ruid = -1;
	extern int optind;
	extern char *optarg;

#ifdef KERBEROS
	strcpy(realm, KRB_REALM);
#endif
	nfsargs = nfsdefargs;
	retrycnt = DEF_RETRY;
	if (argc <= 1)
		Usage(argc, argv);
	while ((c = getopt(argc, argv, "bsiTpMlqdckPF:R:r:w:t:x:g:a:L:D:Km:"))
		!= EOF)
		switch (c) {
		case 'b':
			opflags |= BGRND;
			break;
		case 's':
			nfsargsp->flags |= NFSMNT_SOFT;
			break;
		case 'i':
			nfsargsp->flags |= NFSMNT_INT;
			break;
		case 'T':
			nfsargsp->sotype = SOCK_STREAM;
			break;
#ifdef ISO
		case 'p':
			nfsargsp->sotype = SOCK_SEQPACKET;
			break;
#endif
		case 'M':
			nfsargsp->flags |= NFSMNT_MYWRITE;
			break;
		case 'l':
			nfsargsp->flags |= NFSMNT_RDIRALOOK;
			break;
		case 'q':
			nfsargsp->flags |= NFSMNT_NQNFS;
			break;
		case 'd':
			nfsargsp->flags |= NFSMNT_DUMBTIMR;
			break;
		case 'c':
			nfsargsp->flags |= NFSMNT_NOCONN;
			break;
		case 'k':
			nfsargsp->flags |= NFSMNT_NQLOOKLEASE;
			break;
		case 'P':
			nfsargsp->flags |= NFSMNT_RESVPORT;
			break;
		case 'F':
			if ((num = atoi(optarg)) != 0)
				flags = num;
			break;
		case 'R':
			if ((num = atoi(optarg)) > 0)
				retrycnt = num;
			break;
		case 'r':
			if ((num = atoi(optarg)) > 0) {
				nfsargsp->rsize = num;
				nfsargsp->flags |= NFSMNT_RSIZE;
			}
			break;
		case 'w':
			if ((num = atoi(optarg)) > 0) {
				nfsargsp->wsize = num;
				nfsargsp->flags |= NFSMNT_WSIZE;
			}
			break;
		case 't':
			if ((num = atoi(optarg)) > 0) {
				nfsargsp->timeo = num;
				nfsargsp->flags |= NFSMNT_TIMEO;
			}
			break;
		case 'x':
			if ((num = atoi(optarg)) > 0) {
				nfsargsp->retrans = num;
				nfsargsp->flags |= NFSMNT_RETRANS;
			}
			break;
		case 'g':
			if ((num = atoi(optarg)) > 0) {
				set_rpc_maxgrouplist(num);
				nfsargsp->maxgrouplist = num;
				nfsargsp->flags |= NFSMNT_MAXGRPS;
			}
			break;
		case 'a':
			if ((num = atoi(optarg)) >= 0) {
				nfsargsp->readahead = num;
				nfsargsp->flags |= NFSMNT_READAHEAD;
			}
			break;
		case 'L':
			if ((num = atoi(optarg)) >= 2) {
				nfsargsp->leaseterm = num;
				nfsargsp->flags |= NFSMNT_LEASETERM;
			}
			break;
		case 'D':
			if ((num = atoi(optarg)) > 0) {
				nfsargsp->deadthresh = num;
				nfsargsp->flags |= NFSMNT_DEADTHRESH;
			}
			break;
#ifdef KERBEROS
		case 'K':
			nfsargsp->flags |= NFSMNT_KERB;
			break;
		case 'm':
			strncpy(realm, optarg, REALM_SZ - 1);
			realm[REALM_SZ - 1] = '\0';
			break;
#endif /* KERBEROS */
		default:
			Usage(argc, argv);
		};
	if ((argc - optind) == 2) {
		spec = argv[optind];
		name = argv[optind + 1];
	} else
		Usage(argc, argv);
	if (getnfsargs(spec, nfsargsp)) {
		if (mount(MOUNT_NFS, name, flags, nfsargsp))
			exit(1);
		if (nfsargsp->flags & (NFSMNT_NQNFS | NFSMNT_KERB)) {
			if ((opflags & ISBGRND) == 0) {
				if (i = fork()) {
					if (i == -1) {
						perror("nqnfs");
						exit(1);
					}
					exit();
				}
				(void) setsid();
				(void) close(0);
				(void) close(1);
				(void) close(2);
				(void) chdir("/");
			}
			openlog("mount_nfs:", LOG_PID, LOG_DAEMON);
			nfssvc_flag = NFSSVC_MNTD;
			ncd.ncd_dirp = name;
			while (nfssvc(nfssvc_flag, (caddr_t)&ncd) < 0) {
			    if (errno == ENEEDAUTH) {
syslog(LOG_ERR, "in eacces");
				nfssvc_flag = NFSSVC_MNTD | NFSSVC_GOTAUTH |
					NFSSVC_AUTHINFAIL;
#ifdef KERBEROS
syslog(LOG_ERR,"Callin krb uid=%d inst=%s realm=%s",ncd.ncd_authuid,inst,realm);
				/*
				 * Set up as ncd_authuid for the kerberos call.
				 * Must set ruid to ncd_authuid and reset the
				 * ticket name iff ncd_authuid is not the same
				 * as last time, so that the right ticket file
				 * is found.
				 */
				if (ncd.ncd_authuid != last_ruid) {
					krb_set_tkt_string("");
					last_ruid = ncd.ncd_authuid;
				}
				setreuid(ncd.ncd_authuid, 0);
				if (krb_mk_req(&kt, "rcmd", inst, realm, 0) ==
				    KSUCCESS &&
				    kt.length <= (RPCAUTH_MAXSIZ - 2*NFSX_UNSIGNED)) {
syslog(LOG_ERR,"Got it\n");
				    ncd.ncd_authtype = RPCAUTH_NQNFS;
				    ncd.ncd_authlen = kt.length;
				    ncd.ncd_authstr = (char *)kt.dat;
				    nfssvc_flag = NFSSVC_MNTD | NFSSVC_GOTAUTH;
				}
				setreuid(0, 0);
syslog(LOG_ERR,"ktlen=%d\n", kt.length);
#endif /* KERBEROS */
			    } else
				syslog(LOG_ERR, "nfssvc err %m");
			}
		}
		exit();
	} else
		exit(1);
}

getnfsargs(spec, nfsargsp)
	char *spec;
	struct nfs_args *nfsargsp;
{
	register CLIENT *clp;
	struct hostent *hp;
	static struct sockaddr_in saddr;
#ifdef ISO
	static struct sockaddr_iso isoaddr;
	struct iso_addr *isop;
#endif
	struct timeval pertry, try;
	enum clnt_stat clnt_stat;
	int so = RPC_ANYSOCK, isoflag = 0, i;
	char *hostp, *delimp, *cp;
	u_short tport;
	static struct nfhret nfhret;
	static char nam[MNAMELEN + 1];

	strncpy(nam, spec, MNAMELEN);
	nam[MNAMELEN] = '\0';
	if ((delimp = index(spec, '@')) != NULL) {
		hostp = delimp + 1;
	} else if ((delimp = index(spec, ':')) != NULL) {
		hostp = spec;
		spec = delimp + 1;
	} else {
		fprintf(stderr,
		    "No <host>:<dirpath> or <dirpath>@<host> spec\n");
		return (0);
	}
	*delimp = '\0';
	/*
	 * DUMB!! Until the mount protocol works on iso transport, we must
	 * supply both an iso and an inet address for the host.
	 */
#ifdef ISO
	if (!strncmp(hostp, "iso=", 4)) {
		u_short isoport;

		hostp += 4;
		isoflag++;
		if ((delimp = index(hostp, '+')) == NULL) {
			fprintf(stderr, "No iso+inet address\n");
			return (0);
		}
		*delimp = '\0';
		if ((isop = iso_addr(hostp)) == NULL) {
			fprintf(stderr, "Bad iso address\n");
			return (0);
		}
		bzero((caddr_t)&isoaddr, sizeof (isoaddr));
		bcopy((caddr_t)isop, (caddr_t)&isoaddr.siso_addr,
			sizeof (struct iso_addr));
		isoaddr.siso_len = sizeof (isoaddr);
		isoaddr.siso_family = AF_ISO;
		isoaddr.siso_tlen = 2;
		isoport = htons(NFS_PORT);
		bcopy((caddr_t)&isoport, TSEL(&isoaddr), isoaddr.siso_tlen);
		hostp = delimp + 1;
	}
#endif /* ISO */

	/*
	 * Handle an internet host address and reverse resolve it if
	 * doing Kerberos.
	 */
	if (isdigit(*hostp)) {
		if ((saddr.sin_addr.s_addr = inet_addr(hostp)) == -1) {
			fprintf(stderr, "Bad net addr %s\n", hostp);
			return (0);
		}
		if ((nfsargsp->flags & NFSMNT_KERB) &&
		    (hp = gethostbyaddr((char *)&saddr.sin_addr.s_addr,
			sizeof (u_long), AF_INET)) == (struct hostent *)0) {
			fprintf(stderr, "Can't reverse resolve net addr\n");
			return (0);
		}
	} else if ((hp = gethostbyname(hostp)) == NULL) {
		fprintf(stderr, "Can't get net id for host\n");
		return (0);
	}
#ifdef KERBEROS
	if (nfsargsp->flags & NFSMNT_KERB) {
		strncpy(inst, hp->h_name, INST_SZ);
		inst[INST_SZ - 1] = '\0';
		if (cp = index(inst, '.'))
			*cp = '\0';
	}
#endif /* KERBEROS */

	bcopy(hp->h_addr, (caddr_t)&saddr.sin_addr, hp->h_length);
	nfhret.stat = EACCES;	/* Mark not yet successful */
	while (retrycnt > 0) {
		saddr.sin_family = AF_INET;
		saddr.sin_port = htons(PMAPPORT);
		if ((tport = pmap_getport(&saddr, RPCPROG_NFS,
		    NFS_VER2, IPPROTO_UDP)) == 0) {
			if ((opflags & ISBGRND) == 0)
				clnt_pcreateerror("NFS Portmap");
		} else {
			saddr.sin_port = 0;
			pertry.tv_sec = 10;
			pertry.tv_usec = 0;
			if ((clp = clntudp_create(&saddr, RPCPROG_MNT,
			    RPCMNT_VER1, pertry, &so)) == NULL) {
				if ((opflags & ISBGRND) == 0)
					clnt_pcreateerror("Cannot MNT PRC");
			} else {
				clp->cl_auth = authunix_create_default();
				try.tv_sec = 10;
				try.tv_usec = 0;
				clnt_stat = clnt_call(clp, RPCMNT_MOUNT,
				    xdr_dir, spec, xdr_fh, &nfhret, try);
				if (clnt_stat != RPC_SUCCESS) {
					if ((opflags & ISBGRND) == 0)
						clnt_perror(clp, "Bad MNT RPC");
				} else {
					auth_destroy(clp->cl_auth);
					clnt_destroy(clp);
					retrycnt = 0;
				}
			}
		}
		if (--retrycnt > 0) {
			if (opflags & BGRND) {
				opflags &= ~BGRND;
				if (i = fork()) {
					if (i == -1) {
						perror("nqnfs");
						exit(1);
					}
					exit();
				}
				(void) setsid();
				(void) close(0);
				(void) close(1);
				(void) close(2);
				(void) chdir("/");
				opflags |= ISBGRND;
			} 
			sleep(60);
		}
	}
	if (nfhret.stat) {
		if (opflags & ISBGRND)
			exit(1);
		fprintf(stderr, "Can't access %s: ", spec);
		errno = nfhret.stat;
		perror(NULL);
		return (0);
	}
	saddr.sin_port = htons(tport);
#ifdef ISO
	if (isoflag) {
		nfsargsp->addr = (struct sockaddr *) &isoaddr;
		nfsargsp->addrlen = sizeof (isoaddr);
	} else
#endif /* ISO */
	{
		nfsargsp->addr = (struct sockaddr *) &saddr;
		nfsargsp->addrlen = sizeof (saddr);
	}
	nfsargsp->fh = &nfhret.nfh;
	nfsargsp->hostname = nam;
	return (1);
}

/*
 * xdr routines for mount rpc's
 */
xdr_dir(xdrsp, dirp)
	XDR *xdrsp;
	char *dirp;
{
	return (xdr_string(xdrsp, &dirp, RPCMNT_PATHLEN));
}

xdr_fh(xdrsp, np)
	XDR *xdrsp;
	struct nfhret *np;
{
	if (!xdr_u_long(xdrsp, &(np->stat)))
		return (0);
	if (np->stat)
		return (1);
	return (xdr_opaque(xdrsp, (caddr_t)&(np->nfh), NFSX_FH));
}

Usage(argc, argv)
	int argc;
	char *argv[];
{
	register int i;

	for (i = 0; i < argc; i++)
		fprintf(stderr, "%s ", argv[i]);
	fprintf(stderr, "\nBad mount_nfs arg\n");
	exit(1);
}
