/*
 * Definitions for PasswordLookup.
 */
#include <courier/courier.h>
#include <courier/courierconnection.h>
#define PasswordLookup_VERSION 1
#define PasswordLookup_NUMBER 754


typedef struct {
	String pw_name;
	String pw_passwd;
	LongCardinal pw_uid;
	LongCardinal pw_gid;
	LongCardinal pw_quota;
	String pw_comment;
	String pw_gecos;
	String pw_dir;
	String pw_shell;
} Passwd;

#define NoSuchUser (ERROR_OFFSET+0)
#define NoSuchUserArgs T_cn754_1

typedef struct {
	String errorstring;
} T_cn754_2;

#define OtherError (ERROR_OFFSET+1)
#define OtherErrorArgs T_cn754_2

typedef struct {
	Passwd passwd;
} LookupUidResults;

extern LookupUidResults LookupUid();

typedef struct {
	Passwd passwd;
} LookupUserResults;

extern LookupUserResults LookupUser();
