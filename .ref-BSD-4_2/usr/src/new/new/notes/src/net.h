#
/*
 *	This file describes the format of the net.how file in the
 *	notesfile utility directory. The file is designed to
 *	allow the reconfiguration of the network software to follow
 *	non-uucp links. The format of a line in the file is:
 *
 *	system:direction:::printf control string
 *
 *	the direction field is contains an x or r, for transmit and
 *	reply. The transmit field is used for sending notes to the
 *	remote site. The reply lines are used for tickling the 
 *	remote system into sending notes back.
 *	The third and fourth fields are reserved for future expansion.
 *	The printf control string contains 2 %s arguements.
 *	The first one is the name of the notesfile being sent,
 *	the second is for the local system name.
 *	The receive string is similar.
 *
 *	The default control string is:
 *		"uux - tosite!nfrcv %s %s"
 *	for transmission, and for replies it is:
 *		"uux  farsite!nfxmit %s -d%s"
 *
 *	In the default case, the tosite/farsite names are filled in.
 *
 *	A sample of our file:
 *
 *	uicsovax:x:::uux - uicsovax\!/mnt/dcs/essick/.commands/nfrcv %s %s
 *	uicsovax:r:::uux uicsovax\!/mnt/dcs/essick/.commands/nfxmit %s -d%s
 *
 *	Ray Essick	Aprli 23, 1982
 */

#define	NFXMIT "nfxmit"				/* path to nf xmit */
#define	NFRCV	"nfrcv"				/* path to reciever */

#define	DFLTXMIT "uux - %s\!nfxmit %s %s"	/* default xmit */
#define	DFLTRPLY "uux %s\!%s %s -d%s"		/* default reply */

#define	NETHOW	"net.how"			/* file containing net links */
#define	ALIASES	"net.aliases"			/* alias directory */
