/*
 * This is where the lock files are kept.  It MUST be on the same
 * file system as the "mailboxes" directory.  It also must be read/
 * write by the world.  When a mailbox needs locking (while being
 * read and cleared by inc, or written by deliver), a link to the
 * mailbox is made in this directory, under the same name (i.e., the
 * users name).  Links are one of the few things even a privileged
 * process (deliver) cannot over-ride.  The deliver process waits
 * for lockwait seconds for the lock to clear, then it over-rides
 * the lock.  This number should be set around 15-30 seconds in the
 * case of a VERY loaded system.
 */

char    *lockdir =      "/usr/spool/mail/";
int     lockwait =      15;                     /* Seconds */
