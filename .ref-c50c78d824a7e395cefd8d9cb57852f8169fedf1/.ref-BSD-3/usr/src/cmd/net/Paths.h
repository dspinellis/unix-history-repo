/* note that only the first 8 letters are significant to cpp */

/* adjustable path names, may differ on each machine */
# define NETCMD		"/usr/net/bin/net"
# define NETCMD1	"/usr/net/bin/net"
# define NETDAEMON	"/usr/net/bin/netdaemon"
# define NETQSTAT	"/usr/net/bin/netqstats"

# ifdef CSVAX
# define LOGFILE 	"/usr/spool/berknet/plogfileX"
# define PUBLOGFILE 	"/usr/spool/berknet/logfile"
# define DUMPFILE 	"/usr/spool/berknet/netstatX"
# define NAMEFILE	"/usr/spool/berknet/usernames"

# define RESFILE	"/usr/spool/berknet/rcv/rfaaXXXXX"
# define TEMPFILE 	"/usr/spool/berknet/rcv/tfaaXXXXX"
# define SENDDIR 	"/usr/spool/berknet/send?"
# define DFNAME 	"/usr/spool/berknet/send?/dfaaXXXXX"
# define NETRMPATH	"/usr/spool/berknet/sendY/cfxx0000x"
# define NETRMNAME	"/usr/spool/berknet/sendY/cfaa0000x"

# else

# define LOGFILE 	"/usr/net/plogfileX"
# define PUBLOGFILE 	"/usr/net/logfile"
# define DUMPFILE 	"/usr/net/netstatX"
# define NAMEFILE	"/usr/net/usernames"

# define RESFILE	"/usr/net/rcv/rfaaXXXXX"
# define TEMPFILE 	"/usr/net/rcv/tfaaXXXXX"
# define SENDDIR 	"/usr/net/send?"
# define DFNAME 	"/usr/net/send?/dfaaXXXXX"
# define NETRMPATH	"/usr/net/sendY/cfxx0000x"
# define NETRMNAME	"/usr/net/sendY/cfaa0000x"

# endif

# ifdef CORY
# define BINSH		"/bin/csh"
# else
# define BINSH		"/bin/sh"
# endif

# ifdef CSVAX
# define SYSMAIL1	"/usr/ucb/Mail"
# define SYSMAIL2	"/bin/mail"
# else
# define SYSMAIL1	"/bin/mail"
# define SYSMAIL2	"/usr/bin/mail"
# endif

# define SYSMAIL3	"/bin/mail"
# define SYSMAIL4	"/usr/bin/mail"

/* 
   Adjustable path names, must be the same on all machines.

   MMAILCMD 	sent by sendberkmail
   MWRITECMD 	sent by netdaemon
   NETCPCMD 	sent by netcp
   PRMAIL	sent by netmail
   CATCMD 	sent by netcp and netdaemon
   FILECAT	should be sent by netcp and netdaemon

*/
# define MMAILCMD	"/usr/net/bin/mmail"
# define MWRITECMD	"/usr/net/bin/mwrite"
# define NETCPCMD	"/usr/net/bin/netcp"
# define PRMAIL		"/usr/net/bin/prmail"
# define CATCMD 	"/bin/cat"
# define FILECAT	"/usr/net/bin/filecat"
