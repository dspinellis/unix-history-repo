/*
 * kpasswd_proto
 *
 * definitions for the kpasswd "protocol"
 * (We hope this to be temporary until a real admin protocol is worked out)
 */

#define	MSGSIZE	255
struct kpasswd_data {
	C_Block	random_key;
	char	secure_msg[MSGSIZE];
};
struct update_data {
	char	pw[255];
	char	secure_msg[MSGSIZE];
};
#define	SERVICE		"kpasswd"
#define	SECURE_STRING	"Kerberos password update program -- 12/9/88 UC Berkeley"
