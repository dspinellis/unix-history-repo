struct a {
	short	foo;
	short	timer[4];
};
f()
{
	register struct a *p;

	p->timer[2] = (int)p->foo;
}
