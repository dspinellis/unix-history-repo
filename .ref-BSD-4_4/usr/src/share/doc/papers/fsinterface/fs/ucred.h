/*
 * Credentials.
 */
struct ucred {
	u_short	cr_ref;			/* reference count */
	uid_t	cr_uid;			/* effective user id */
	short	cr_ngroups;		/* number of groups */
	gid_t	cr_groups[NGROUPS];	/* groups */
	/*
	 * The following either should not be here,
	 * or should be treated as opaque.
	 */
	uid_t   cr_ruid;		/* real user id */
	gid_t   cr_svgid;		/* saved set-group id */
};

#ifdef KERNEL
#define	crhold(cr)	(cr)->cr_ref++
struct ucred *crget();
struct ucred *crcopy();
struct ucred *crdup();
#endif KERNEL
