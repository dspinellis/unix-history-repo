/*
**  LOCK.H -- Concurency structs and global variables
**
**	Version:
**		@(#)lock.h	7.1	2/5/81
*/

# ifndef KEYSIZE


# define	M_SHARE		2
# define	M_EXCL		1
# define	T_CS		0
# define	T_PAGE		1
# define	T_REL		2
# define	T_DB		3
# define	A_RTN		1
# define	A_SLP		2
# define	A_RLS1		3
# define	A_RLSA		4
# define	KEYSIZE		12
struct lockreq
{
	char	lract;			/* requested action
					 *	=1 request lock,err return
					 *	=2 request lock,sleep
					 *	=3 release lock
					 *	=release all locks for pid
					 */
	char	lrtype;			/* type of lock:
					 *   =0, critical section lock
					 *   =1, page lock
					 *   =2, logical lock
					 *   =3, data base lock
					 */
	char	lrmod;			/* mode of lock
					 *	=1 exclusive lock
					 *	=2 shared lock
					*/
					/* key for the lock */
	char	dbnode[4];		/* inode of data base */
	char	lrel[4];		/* relation tid */
	char	lpage[4];		/* page address		*/
};

extern char	Acclock;		/* locks enabled flag */
extern int	Alockdes;		/* file descriptor for lock device*/
extern int	Lockrel;		/* lock relations flag*/


# endif KEYSIZE
