/*	mtab.h	4.4	83/05/28	*/

/*
 * Mounted device accounting file.
 */
struct mtab {
	char	m_path[32];		/* mounted on pathname */
	char	m_dname[32];		/* block device pathname */
	char	m_type[4];		/* read-only, quotas */
};
