	call tstil
	call tstch
	call tstrd
	call tstcmp
	end
	subroutine tstil
c		test integers and logicals
	integer ivec(8)
	logical lvec(6), l1, l2

	namelist /nml/ key, ii, ivec, l1, l2, lvec
	data ii/-1/, ivec/8*2/, lvec/6*.false./, l1/.false./, l2/.false./

	print '(//"test integers and logicals...")'
10	continue
	read(*,nml)
	print nml
	if(key.lt.0) return
	go to 10
	end

	subroutine tstch
	character ch, chvec(12), str*10, strv(-1:4)*5
	namelist /nmc/ key, ch, chvec, str, strv
	data ch/'+'/, chvec/12*'-'/, str/'+..-..-..+'/, strv/6*':...:'/

	print '(//"test characters and strings...")'
10	continue
	read(*,nmc)
	print nmc
	if(key.lt.0) return
	go to 10
	
	end

	subroutine tstrd
	real r, rvec(10)
	double precision d, darr(-10:-8, 0:1, 1:2)
	namelist /nmrd/ key, r,d, rvec, darr

	print '(//"test reals and doubles...")'
10	continue
	read(*,nmrd)
	print nmrd
	if(key.lt.0) return
	go to 10
	
	end

	subroutine tstcmp
	complex c, cvec(6)
	double complex z, zvec(6)
	namelist /nmcmp/ key, c,cvec,z,zvec

	print '(//"test complex and double complex...")'
10	continue
	read(*,nmcmp)
	print nmcmp
	if(key.lt.0) return
	go to 10
	
	end
