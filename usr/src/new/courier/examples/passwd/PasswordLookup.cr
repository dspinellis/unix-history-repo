PasswordLookup : PROGRAM =

BEGIN

    -- This is a translation of the passwd structure in <pwd.h>

    Passwd : TYPE = RECORD [
	pw_name, pw_passwd : STRING,
	pw_uid, pw_gid, pw_quota : LONG CARDINAL,
	pw_comment, pw_gecos, pw_dir, pw_shell : STRING
    ];

    -- Remote entry points.

    LookupUser : PROCEDURE [user : STRING] RETURNS [passwd : Passwd]
		    = 0;

    LookupUid : PROCEDURE [uid : CARDINAL] RETURNS [passwd : Passwd]
		    = 1;
END.
