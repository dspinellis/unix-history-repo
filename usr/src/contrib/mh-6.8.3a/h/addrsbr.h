/* addrsbr.h - definitions for the address parsing system */

#define	AD_HOST	1		/* getm(): lookup official hostname */
#define	AD_NHST	0		/* getm(): do not lookup official name */
#define	AD_NAME	AD_NHST		/* AD_HOST is TOO slow */


struct mailname {
    struct mailname *m_next;

    char   *m_text,
           *m_pers,
           *m_mbox,
           *m_host,
	   *m_path;

    int     m_type;
#define	UUCPHOST	(-1)
#define	LOCALHOST	0
#define	NETHOST		1
#define	BADHOST		2

    char    m_nohost;

    char    m_bcc;

    int     m_ingrp;
    char   *m_gname;

    char   *m_note;

#ifdef	MHMTS
    char   *m_aka;
#endif /* MHMTS */
};


void	mnfree ();
int     ismymbox ();
char   *getname (), *adrsprintf (), *auxformat ();
struct mailname *getm ();

#define	adrformat(m)	auxformat ((m), 1)

char   *LocalName (), *SystemName (), *UucpChan ();
char   *OfficialName ();
