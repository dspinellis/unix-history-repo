/*	mush.h	Melb 4.1	82/07/16	*/

/*
 * MUSH
 *
 *	format of messages sent from the system to the mush daemon (pid 3)
 */

#define	MUSHPID	3

typedef	union	{

	struct	d_s	{
		char	D_req;
		char	D_info;
		short	D_uid;
		union	{
			struct	{
				short	D_sdat;
				short	D_xdat;
			} d_us;
			long	D_ldat;
			float	D_fdat;
		} d_u;
	} d_s;

	char	d_c[sizeof(struct d_s)];

} data_t;
#define	d_req	d_s.D_req
#define	d_info	d_s.D_info
#define	d_uid	d_s.D_uid
#define	d_sdat	d_s.d_u.d_us.D_sdat
#define	d_xdat	d_s.d_u.d_us.D_xdat
#define	d_ldat	d_s.d_u.D_ldat
#define	d_fdat	d_s.d_u.D_fdat

#define	DATA_T	data_t

/*
 * MUSH messages (D_req values) - to MUSH from kernel
 */
#define	MM_PROC1	1	/* first process created for uid */
#define	MM_PROCX	2	/* last process for uid exited */
#define	MM_LOGIN	3	/* additional login for uid */
#define	MM_LOGOUT	4	/* user login proc exited (not last proc) */
#define	MM_ATJOB	5	/* at job has started for uid */
#define	MM_NOCPU	6	/* user isn't getting any cpu time */

/*
 * nb: the preceding msgs are ignored if they concern uid 0
 *	the ones that follow are not
 */

#define	MM_NEWCLASS	9	/* current class has (might have) altered */
