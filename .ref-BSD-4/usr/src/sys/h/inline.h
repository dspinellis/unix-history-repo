/*	inline.h	4.1	11/9/80	*/
/*
 * Definitions of inlines for FASTVAX, and macro replacements
 * for them if not FASTVAX (latter only scantily tested).
 */

#ifdef FASTVAX

#define	plock(ip) \
{ \
	while ((ip)->i_flag & ILOCK) { \
		(ip)->i_flag |= IWANT; \
		sleep((caddr_t)(ip), PINOD); \
	} \
	(ip)->i_flag |= ILOCK; \
}

#define	prele(ip) \
{ \
	(ip)->i_flag &= ~ILOCK; \
	if ((ip)->i_flag&IWANT) { \
		(ip)->i_flag &= ~IWANT; \
		wakeup((caddr_t)(ip)); \
	} \
}

#define	GETF(fp, fd) { \
	if ((unsigned)(fd) >= NOFILE || ((fp) = u.u_ofile[fd]) == NULL) { \
		u.u_error = EBADF; \
		return; \
	} \
}

#define	IUPDAT(ip, t1, t2, waitfor) { \
	if (ip->i_flag&(IUPD|IACC|ICHG)) \
		iupdat(ip, t1, t2, waitfor); \
}
#define	ISSIG(p)	((p)->p_sig && \
	((p)->p_flag&STRC || ((p)->p_sig &~ (p)->p_ignsig)) && issig())
#else

#define	GETF(fp, fd) { \
	(fp) = getf(fd); \
	if ((fp) == NULL) \
		return; \
}

#define	IUPDAT(ip, t1, t2, waitfor)	iupdat(ip, t1, t2, waitfor)

#define	ISSIG(p)	issig(p)
#endif
