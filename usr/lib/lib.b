scale = 20
define e(x){
	auto a, b, c, d, e, g
	a=1
	b=10
	c=b
	d=1
	e=1
	for(a=1;1==1;a++){
		b=b*x
		c=c*a+b
		d=d*a
		g = c/d
		if(g == e) return(g/10)
		e=g
	}
}
scale = 20
define s(x){
	auto a, b, c, d, e, g, y
	y = -x*x
	a=1
	b=x
	c=b
	d=1
	e=1
	for(a=3;1==1;a=a+2){
		b=b*y
		c=c*a*(a-1) + b
		d=d*a*(a-1)
		g=c/d
		if(g==e) return(g)
		e=g
	}
}
scale = 20
define c(x){
	auto a, b, c, d, e, g, y
	y = -x*x
	a=1
	b=1
	c=b
	d=1
	e=1
	for(a=2;1==1;a=a+2){
		b=b*y
		c=c*a*(a-1) + b
		d=d*a*(a-1)
		g=c/d
		if(g==e) return(g)
		e=g
	}
}
scale = 20
define l(x){
	auto a, b, c, d, e, f, g, u, s, t
	if(x <=0) return(1-10^scale)
	t = scale
	scale = 0
	f = 1
	s = x
	while(s > 0){
		s = s/10
		f = f + 1
	}
	scale = t + f
	f=1
	while(x > 2){
		x = sqrt(x)
		f=f*2
	}
	while(x < .5){
		x = sqrt(x)
		f=f*2
	}
	u = (x-1)/(x+1)
	s = u*u
	b = 2*f
	c = b
	d = 1
	e = 1
	for(a=3;1==1;a=a+2){
		b=b*s
		c=c*a+d*b
		d=d*a
		g=c/d
		if(g==e){
			scale = t
			return(u*c/d)
		}
		e=g
	}
}
scale = 20
define a(x){
	auto a, b, c, d, e, f, g, s, t
	if(x==0) return(0)
	t = scale
	f=1
	while(x > .5){
		scale = scale + 1
		x= -(1-sqrt(1.+x*x))/x
		f=f*2
	}
	while(x < -.5){
		scale = scale + 1
		x = -(1-sqrt(1.+x*x))/x
		f=f*2
	}
	s = -x*x
	b = f
	c = f
	d = 1
	e = 1
	for(a=3;1==1;a=a+2){
		b=b*s
		c=c*a+d*b
		d=d*a
		g=c/d
		if(g==e){
			scale = t
			return(x*c/d)
		}
		e=g
	}
}
scale = 20
define j(n,x){
auto a,b,c,d,e,g,i,s
s= -x*x/4
if(n<0){
	n= -n
	x= -x
	}
a=1
c=1
for(i=1;i<=n;i++){
	a=a*x
	c = c*2*i
	}
b=a
d=1
e=1
for(i=1;1;i++){
	a=a*s
	b=b*i*(n+i) + a
	c=c*i*(n+i)
	g=b/c
	if(g==e){
		return(g)
		}
	e=g
	}
}
