Time : PROGRAM =

BEGIN

    -- This is a translation of the tm structure in <time.h>

    Time : TYPE = RECORD [
	tm_sec,
	tm_min,
	tm_hour,
	tm_mday,
	tm_mon,
	tm_year,
	tm_wday,
	tm_yday,
	tm_isdst : LONG CARDINAL
    ];

    -- Remote entry points.

    LocalTime : PROCEDURE RETURNS [time : Time]
		    = 0;

    GMTime : PROCEDURE RETURNS [time : Time]
		    = 1;
END.
