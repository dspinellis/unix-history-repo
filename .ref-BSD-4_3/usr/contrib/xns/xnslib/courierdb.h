/*	$Header$	*/

/* description for the database of courier services, normally
 * in the file /etc/Courierservices
 */

struct courierdbent {
	char *cr_programname;	/* the name of the Courier program */
	unsigned long cr_programnumber;	/* official number of program */
	unsigned short cr_version;	/* version number of this server */
	char *cr_description;	/* file containing the Courier description */
	char *cr_serverbin;	/* file containing the server binary */
};

extern struct courierdbent *getcourierdbent();
extern struct courierdbent *getcourierservice();
