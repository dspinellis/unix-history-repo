/* note that only the first 8 letters are significant to cpp */

# ifdef RAND
/* Rand definitions */
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
/* end of Rand definitions */

# else

/* for Berkeley */
/* adjustable path names, may differ on each machine */
# define NETCMD		"/usr/net/bin/net"
# define NETCMD1	"/usr/net/bin/net"
# define NETDAEMON	"/usr/net/bin/netdaemon"

# ifdef CORY
# define BINSH		"/bin/csh"
# else
# define BINSH		"/bin/sh"
# endif

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
# define STATADDR	"CSVAX:/usr/spool/berknet/statallnet"
/* end of Berkeley */
# endif


# define LOGFILE 	"/usr/spool/berknet/plogfileX"
# define PUBLOGFILE 	"/usr/spool/berknet/logfile"

# define RESFILE	"/usr/spool/berknet/rcv/rfaaXXXXX"
# define TEMPFILE 	"/usr/spool/berknet/rcv/tfaaXXXXX"
# define SENDDIR 	"/usr/spool/berknet/send?"
# define DFNAME 	"/usr/spool/berknet/send?/dfaaXXXXX"
# define NETRMPATH	"/usr/spool/berknet/sendY/cfxx0000x"
# define NETRMNAME	"/usr/spool/berknet/sendY/cfaa0000x"

