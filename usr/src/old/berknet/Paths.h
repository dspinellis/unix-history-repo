/*	@(#)Paths.h	4.2	(Berkeley)	%G%	*/

/* sccs id variable */
static char *Paths_h_sid= "@(#)Paths.h	1.6";
/* note that only the first 8 letters are significant to cpp */

# ifdef RAND
# define NETCMD		"/usr/Berknet/bin/net"
# define NETCMD1	"/usr/Berknet/bin/net"
# define NETDAEMON	"/usr/Berknet/bin/netdaemon"
# define BINSH		"/bin/sh"
# define MAILFWD1 	"/usr/Berknet/bin/Mail"
# define SYSMAIL2 	"/usr/Berknet/bin/v6mail"
# define MMAILCMD	"/usr/Berknet/bin/mmail"
# define MWRITECMD	"/usr/Berknet/bin/mwrite"
# define NETCPCMD	"/usr/Berknet/bin/netcp"
# define PRMAIL		"/usr/Berknet/bin/prmail"
# define STATADDR	"/usr/spool/berknet/statallnet"
# endif RAND

# ifdef NOSC
# define NETCMD		"/usr/ucbnet/bin/net"
# define NETCMD1	"/usr/ucbnet/bin/net"
# define NETDAEMON	"/usr/ucbnet/bin/netdaemon"
# define BINSH		"/bin/sh"
# define MAILFWD1 	"/usr/ucbnet/bin/Mail"
# define SYSMAIL2 	"/usr/ucbnet/bin/v6mail"
# define MMAILCMD	"/usr/ucbnet/bin/mmail"
# define MWRITECMD	"/usr/ucbnet/bin/mwrite"
# define NETCPCMD	"/usr/ucbnet/bin/netcp"
# define PRMAIL		"/usr/ucbnet/bin/prmail"
# define STATADDR	"/usr/spool/berknet/statallnet"
# endif NOSC

# ifdef BERKELEY
/* adjustable path names, may differ on each machine */
# define FREEPATH	"/usr/net/network/freecmd/"
# define NETCMD		"/usr/net/bin/net"
# define NETCMD1	"/usr/net/bin/net"
# define NETDAEMON	"/usr/net/bin/netdaemon"

#ifdef CSH 
#define BINSH		"/bin/csh"
#else CSH
#define BINSH		"/bin/sh"
#endif CSH

/* we use v6mail whether we are or are not on a v6 system */
# define MAILFWD1 "/usr/net/bin/Mail"
# define SYSMAIL2 "/usr/net/bin/v6mail"

/* 
   Adjustable path names, must be the same on all machines.

   MMAILCMD 	sent by sendberkmail
   MWRITECMD 	sent by netdaemon
   NETCPCMD 	sent by netcp
   PRMAIL	sent by netmail

*/
# define MMAILCMD	"/usr/net/bin/mmail"
# define MWRITECMD	"/usr/net/bin/mwrite"
# define NETCPCMD	"/usr/net/bin/netcp"
# define PRMAIL		"/usr/net/bin/prmail"
# define STATADDR	"ucbvax:/usr/spool/berknet/statallnet"
# endif BERKELEY


# define LOGFILE 	"/usr/spool/berknet/plogfileX"
# define PUBLOGFILE 	"/usr/spool/berknet/logfile"

# define RESFILE	"/usr/spool/berknet/rcv/rfaaXXXXX"
# define TEMPFILE 	"/usr/spool/berknet/rcv/tfaaXXXXX"
# define SENDDIR 	"/usr/spool/berknet/send?"
# define DFNAME 	"/usr/spool/berknet/send?/dfaaXXXXX"
# define NETRMPATH	"/usr/spool/berknet/sendY/cfxx0000x"
# define NETRMNAME	"/usr/spool/berknet/sendY/cfaa0000x"
