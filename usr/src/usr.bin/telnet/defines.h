#define	settimer(x)	clocks.x = clocks.system++

#if	!defined(TN3270)

#define	ExitString(s,r)	{ fprintf(stderr, s); exit(r); }
#define	Exit(x)			exit(x)
#define	SetIn3270()

#endif	/* !defined(TN3270) */

#define	NETADD(c)	{ *netoring.supply = c; ring_supplied(&netoring, 1); }
#define	NET2ADD(c1,c2)	{ NETADD(c1); NETADD(c2); }
#define	NETBYTES()	(ring_full_count(&netoring))
#define	NETROOM()	(ring_empty_count(&netoring))

#define	TTYADD(c)	if (!(SYNCHing||flushout)) { \
				*ttyoring.supply = c; \
				ring_supplied(&ttyoring, 1); \
			}
#define	TTYBYTES()	(ring_full_count(&ttyoring))
#define	TTYROOM()	(ring_empty_count(&ttyoring))

/*	Various modes */
#define	MODE_LINE(m)	(modelist[m].modetype & LINE)
#define	MODE_LOCAL_CHARS(m)	(modelist[m].modetype &  LOCAL_CHARS)
#define	MODE_LOCAL_ECHO(m)	(modelist[m].modetype & LOCAL_ECHO)
#define	MODE_COMMAND_LINE(m)	(modelist[m].modetype & COMMAND_LINE)

#define	LOCAL_CHARS	0x01		/* Characters processed locally */
#define	LINE		0x02		/* Line-by-line mode of operation */
#define	LOCAL_ECHO	0x04		/* Echoing locally */
#define	COMMAND_LINE	0x08		/* Command line mode */
