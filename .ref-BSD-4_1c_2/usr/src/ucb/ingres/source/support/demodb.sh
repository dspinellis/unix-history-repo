:	"@(#)demodb.sh	7.1	2/6/81"
if test "$1" = "" ; then
	echo no database name specified
	exit
fi
echo creating database $1 -- please wait
if creatdb $2 $1 ; then echo loading relations ; else exit; fi
ingres -s $2 $1 << 'EOF'
	create item (
		number is i2,
		name is c20,
		dept is i2,
		price is i2,
		qoh is i2,
		supplier is i2)

	create sale (
		number is c6,
		date is c8,
		store is i2,
		dept is i2,
		item is i2,
		quantity is i2,
		employee is i2,
		credit is c8)

	create employee (
		number is i2,
		name is c20,
		salary is i2,
		manager is i2,
		birthdate is i2,
		startdate is i2)

	create dept (
		number is i2,
		name is c20,
		store is i2,
		floor is i2,
		manager is i2)

	create supplier (
		number is i2,
		name is c15,
		city is c15,
		state is c6)

	create store (
		number is i2,
		city is c15,
		state is c6)

	create parts (
		pnum is i2,
		pname is c20,
		color is c8,
		weight is i2,
		qoh is i2)

	create supply (
		snum is i2,
		pnum is i2,
		jnum is i2,
		shipdate is c8,
		quan is i2)


	copy item (number is i2,
		name is c20,
		dept is i2,
		price is i2,
		qoh is i2,
		supplier is i2)
	from "{pathname}/demo/item"

	copy sale (
		number is c6,
		date is c8,
		store is i2,
		dept is i2,
		item is i2,
		quantity is i2,
		employee is i2,
		credit is c8)
	from "{pathname}/demo/sale"

	copy employee (
		number is i2,
		name is c20,
		salary is i2,
		manager is i2,
		birthdate is i2,
		startdate is i2)
	from "{pathname}/demo/employee"

	copy dept (
		number is i2,
		name is c20,
		store is i2,
		floor is i2,
		manager is i2)
	from "{pathname}/demo/dept"

	copy supplier (
		number is i2,
		name is c15,
		city is c15,
		state is c6)
	from "{pathname}/demo/supplier"

	copy store (
		number is i2,
		city is c15,
		state is c6)
	from "{pathname}/demo/store"

	copy parts (
		pnum is i2,
		pname is c20,
		color is c8,
		weight is i2,
		qoh is i2)
	from "{pathname}/demo/parts"

	copy supply (
		snum is i2,
		pnum is i2,
		jnum is i2,
		shipdate is c8,
		quan is i2)
	from "{pathname}/demo/supply"

	range of i is item
	define permit all on i to all

	range of s is sale
	define permit all on s to all

	range of e is employee
	define permit all on e to all

	range of d is dept
	define permit all on d to all

	range of s is supplier
	define permit all on s to all

	range of s is store
	define permit all on s to all

	range of p is parts
	define permit all on p to all

	range of s is supply
	define permit all on s to all
	\g
	\q
EOF
echo database $1 created
