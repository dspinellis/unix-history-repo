-- $Header: Time2.cr,v 2.1 85/11/21 07:33:09 jqj Exp $

-- $Log:	Time2.cr,v $
-- Revision 2.1  85/11/21  07:33:09  jqj
-- fixed comment leaders
-- 
-- Revision 2.0  85/11/21  07:24:35  jqj
-- 4.3BSD standard release
-- 
-- Revision 1.2  85/11/20  13:09:02  jqj
-- 4.3 beta version


Time: PROGRAM 15 VERSION 2 =
BEGIN
	-- number of seconds since 12:00:00 AM, 1 Jan. 1901, GMT --
	Time: TYPE = LONG CARDINAL;

	-- Thurs., 12:00:00AM, 1 Jan. 1968 --
	earliestTime: Time = 2114294400;
END.

