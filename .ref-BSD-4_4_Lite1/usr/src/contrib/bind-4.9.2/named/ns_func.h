/* ns_proc.h - process declarations for ns_*.c's externally visible functions
 *
 * $Id: ns_func.h,v 1.6 1993/12/06 00:43:02 vixie Exp $
 */

/* ++from ns_resp.c++ */
extern void		ns_resp __P((u_char *, int)),
			prime_cache __P((void)),
			delete_all __P((struct namebuf *, int, int));
extern struct qinfo	*sysquery __P((char *, int, int,
				       struct in_addr *, int));
extern int		doupdate __P((char *, int, u_char *, int,
				      struct databuf **,
				      int
#ifdef CRED
				      , u_int
#endif
				      )),
			send_msg __P((char *, int, struct qinfo *)),
			findns __P((struct namebuf **, int,
				    struct databuf **, int *, int)),
			finddata __P((struct namebuf *, int, int, HEADER *,
				      char **, int *, int *)),
			wanted __P((struct databuf *, int, int)),
			add_data __P((struct namebuf *,
				      struct databuf **,
				      u_char *, int));
/* --from ns_resp.c-- */

/* ++from ns_req.c++ */
extern void		ns_req __P((u_char *, int, int,
				    struct qstream *,
				    struct sockaddr_in *,
				    int));
extern int		stale __P((struct databuf *)),
			make_rr __P((char *, struct databuf *,
				     u_char *, int, int)),
			doaddinfo __P((HEADER *, u_char *, int)),
			doaddauth __P((HEADER *, u_char *, int,
				       struct namebuf *,
				       struct databuf *));
/* --from ns_req.c-- */

/* ++from ns_forw.c++ */
extern time_t		retrytime __P((struct qinfo *));
extern int		ns_forw __P((struct databuf *nsp[],
				     u_char *msg,
				     int msglen,
				     struct sockaddr_in *fp,
				     struct qstream *qsp,
				     int dfd,
				     struct qinfo **qpp,
				     char *dname)),
			haveComplained __P((char *, char *)),
			nslookup __P((struct databuf *nsp[],
				      struct qinfo *qp,
				      char *syslogdname,
				      char *sysloginfo)),
			qcomp __P((struct qserv *, struct qserv *));
extern struct qdatagram	*aIsUs __P((struct in_addr));
extern void		nslookupComplain __P((char *, char *, char *, char *,
					      struct databuf *)),
			schedretry __P((struct qinfo *, time_t)),
			unsched __P((struct qinfo *)),
			retry __P((struct qinfo *)),
			qflush __P((void)),
			qremove __P((struct qinfo *)),
			qfree __P((struct qinfo *));
extern struct qinfo	*qfindid __P((u_int16_t)),
#ifdef DMALLOC
			*qnew_tagged __P((void));
#		define	qnew() qnew_tagged(__FILE__, __LINE__)
#else
			*qnew();
#endif
/* --from ns_forw.c-- */

/* ++from ns_main.c++ */
extern u_int32_t	net_mask __P((struct in_addr));
extern void		sqrm __P((struct qstream *)),
			sqflush __P((void)),
			sq_done __P((struct qstream *)),
			setproctitle __P((char *, int)),
			getnetconf __P((void));
extern struct netinfo	*findnetinfo __P((struct in_addr));
/* --from ns_main.c-- */

/* ++from ns_maint.c++ */
extern void		ns_maint __P((void)),
			sched_maint __P((void)),
			remove_zone __P((struct hashbuf *, int)),
			loadxfer __P((void)),
			qserial_answer __P((struct qinfo *, u_int32_t));
extern SIG_FN		endxfer __P((void));
#ifdef DEBUG
extern void		printzoneinfo __P((int));
#endif
/* --from ns_maint.c-- */

/* ++from ns_sort.c++ */
extern struct netinfo	*local __P((struct sockaddr_in *));
extern void		sort_response __P((char *, int,
					   struct netinfo *,
					   u_char *));
/* --from ns_sort.c-- */

/* ++from ns_init.c++ */
extern void		ns_init __P((char *));
/* --from ns_init.c-- */

/* ++from ns_ncache.c++ */
extern void		cache_n_resp __P((u_char *, int));
/* --from ns_ncache.c-- */

/* ++from ns_stats.c++ */
extern void		ns_stats __P((void));
/* --from ns_stats.c-- */

/* ++from ns_validate.c++ */
extern int		validate __P((char *, struct sockaddr_in *,
                                    int, int, char *, int
#ifdef NCACHE
                                    , int
#endif
                                    )),
			dovalidate __P((char *, int, u_char *, int, int,
					struct sockaddr_in *, int *)),
			update_msg __P((u_char *, int *, int Vlist[], int));
extern void		store_name_addr __P((char *, struct in_addr *,
					     char *, char *));
/* --from ns_validate.c-- */
