/*
 * Datakit ioctls
 */
#define	DIOCLHN		(('d'<<8)|32)	/* announce mgr channel */
#define	DIOCHUP		(('d'<<8)|33)	/* tell ctlr to reinitialize */
#define	DIOCSTREAM	(('d'<<8)|34)	/* no input delimiters */
#define	DIOCRECORD	(('d'<<8)|35)	/* input delimiters */
#define	DIOCCHAN	(('d'<<8)|38)	/* suggest channel # */
#define	DIOCSTOP	(('d'<<8)|39)	/* delay input for cmcld */
#define	DIOCSTART	(('d'<<8)|40)	/* restart input for cmcld */
#define	DIOCNXCL	(('d'<<8)|41)	/* turn off exclusive use */
#define	DIOCXWIN	(('d'<<8)|42)	/* set xmtr window sizes */
#define	DIOCSCTL	(('d'<<8)|43)	/* send control character */
#define	DIOCRCTL	(('d'<<8)|44)	/* receive stored control character */

#define	KIOCISURP	(('k'<<8)|1)	/* is URP already turned on? */
#define	KIOCINIT	(('k'<<8)|2)	/* force transmitter reinit */
#define	KIOCSHUT	(('k'<<8)|3)	/* shut down all chans, force reinit */
