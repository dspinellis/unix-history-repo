create parts (
	pnum is i2,
	pname is c20,
	color is c8,
	weight is i2,
	qoh is i2)
\p\g
copy parts (
	pnum is i2,
	pname is c20,
	color is c8,
	weight is i2,
	qoh is i2)
from "{pathname}/demo/parts"

\p\g

range of p is parts
create parts1 (
	pnum is i2,
	pname is c20,
	color is c8,
	weight is i2,
	qoh is i2)
\p\g

append to parts1 (p.all) where p.pnum = 1
\p\g

create parts14 (
	pnum is i2,
	pname is c20,
	color is c8,
	weight is i2,
	qoh is i2)
\p\g

append to parts14 (p.all)
\p\g

/* 28 tuples */

create parts28 (
	pnum is i2,
	pname is c20,
	color is c8,
	weight is i2,
	qoh is i2)
\p\g
append to parts28 (p.all)
append to parts28 (p.all)
\p\g

/* 210 tuples */

range of p28 is parts28

create parts210 (
	pnum is i2,
	pname is c20,
	color is c8,
	weight is i2,
	qoh is i2)
\p\g

append to parts210 (p.all)
append to parts210 (p28.all)
append to parts210 (p28.all)
append to parts210 (p28.all)
\p\g
append to parts210 (p28.all)
append to parts210 (p28.all)
append to parts210 (p28.all)
append to parts210 (p28.all)
\p\g

/* 840 tuples */

range of p210 is parts210

create parts840 (
	pnum is i2,
	pname is c20,
	color is c8,
	weight is i2,
	qoh is i2)
\p\g

append to parts840 (p210.all)
append to parts840 (p210.all)
append to parts840 (p210.all)
append to parts840 (p210.all)
\p\g

/* 3360 tuples */

range of p840 is parts840

create parts3360 (
	pnum is i2,
	pname is c20,
	color is c8,
	weight is i2,
	qoh is i2)
\p\g

append to parts3360 (p840.all)
append to parts3360 (p840.all)
append to parts3360 (p840.all)
append to parts3360 (p840.all)
\p\g

/* 3360 tuples, ISAM */

create i3360 (
	pnum is i4,
	pname is c96,
	color is c96,
	weight is i2,
	qoh is i2)

\p\g

copy i3360 (pnum = c0,
		 pname = c0,
		 color = c0,
		 weight = c0,
		 qoh = c0) from "/a/guest/kalash/=timing/parts3230"
\p\g

/* 3360 tuples, hashed */
create h3360 (
	pnum is i4,
	pname is c96,
	color is c96,
	weight is i2,
	qoh is i2)

\p\g

copy h3360 (pnum = c0,
		 pname = c0,
		 color = c0,
		 weight = c0,
		 qoh = c0) from "/a/guest/kalash/=timing/parts3230"
\p\g

modify i3360 to isam on pnum\p\g
modify h3360 to hash on pnum where fillfactor = 100\p\g


create b130 (
	pnum is i4,
	pname is c96,
	color is c96,
	weight is i2,
	qoh is i2)

\p\g
copy b130 (pnum = c0,
		 pname = c0,
		 color = c0,
		 weight = c0,
		 qoh = c0) from "/a/guest/kalash/=timing/parts130"
\p\g

range of p130 is b130\p\g

append to i3360 (p130.all)\p\g

range of r is relation
retrieve (r.relid, r.reltups)
	where
		r.relid = "parts1" 
	    or  r.relid = "parts14"
	    or	r.relid = "parts28"
	    or 	r.relid = "parts210"
	    or	r.relid = "parts840"
	    or	r.relid = "parts3360"
	    or  r.relid = "h3360"
	    or  r.relid = "i3360"
\p\g
\q
