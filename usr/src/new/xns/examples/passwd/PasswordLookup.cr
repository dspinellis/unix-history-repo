PasswordLookup : PROGRAM 754 VERSION 1 =

BEGIN

    -- This is a translation of the passwd structure in <pwd.h>

    Passwd : TYPE = RECORD [
	pw_name, pw_passwd : STRING,
	pw_uid, pw_gid, pw_quota : LONG CARDINAL,
	pw_comment, pw_gecos, pw_dir, pw_shell : STRING
    ];

    -- Errors

    NoSuchUser: ERROR = 0;
    OtherError: ERROR [errorstring: STRING]
		 = 1;

    -- Remote entry points.

    LookupUid : PROCEDURE [uid : CARDINAL] RETURNS [passwd : Passwd]
		REPORTS [NoSuchUser]
		    = 0;

    LookupUser : PROCEDURE [user : STRING] RETURNS [passwd : Passwd]
		REPORTS [NoSuchUser, OtherError]
		    = 1;

END.
