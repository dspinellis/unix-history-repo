struct t { struct s { character(6) a(2),b;integer c} a(3); integer b(6) }
procedure sam(x)
t x(4),y
integer z(5,7)
x(2).a(2).a(2) = "abc"
y.b(3) = 4
z(2,4)->s.c = 2
z->s.a(2) = "xyz"
end
procedure
struct { field(3) a,b,c,d; field(10000) e; field(3) f} x
integer m
x.a = 2
x.b = 2
x.c = 2
x.d = 2
x.e = 2
x.f = 2
x.a-=2
x.b-=2
x.c-=2
x.d-=2
x.a*=2
x.b*=2
x.c*=2
x.d*=2
x.c+=2
x.c *= x.d += x.b = (x.a=1)+2
end


procedure
struct t { field(0:3) a; field(20) b; field(60) c}
t x
integer k
k = x.a+x.b+x.c
end
procedure
struct t { field(50) a,b,c,d,e }
integer i
t x(5)
x(2).a = i
x(2).b = i
x(2).c = i
x(2).d = i
x(2).e = i

do i = x(1).a, x(3).b,x(5).e
	x(i**2).b *= x(i**2+1).e
end
procedure
struct t {real a} x
x.a = 1.
	{
	struct t {integer b, c} y
	y.b=1
	}
x.a = 2.
end
common(cc) complex a
integer b

procedure
integer a
{logical a;a = .true.}
a = 1
{logical a;a=.false.}
a=2
end
procedure
a = 1
end
procedure a(b)
end

procedure alltyp(y)
struct	{
	real a(3)
	long real b(3)
	integer c(3)
	complex d(3)
	logical e(3)
	character(8) f(3)
	character(9) g(3)
	} x, y

x.a(3) = x.b(3) = x.c(3) = x.d(3) = 1
x.e(3) = false
x.f(3) = x.g(3) = "abcdefg"
end
procedure
integer i,j
logical l
switch(i)
	{
	case 1: if(l) goto case 2
		else switch(j)
			{
			case 1: goto case 2
			case 2: goto case 1
			}
	case 3: j=2
	case 2: while(i!=j) ++j
	}
end
