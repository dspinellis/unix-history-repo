	character*39 errno
	character*32 gerror
	external gerror

	do 100 i = 0, 36
	call serrno(i)
	write(errno,'("[",i3,"] ",a,":")') ierrno(), gerror()
	call perror(errno)
  100	continue

	do 200 i = 99, 123
	call serrno(i)
	write(errno,'("[",i3,"] ",a,":")') ierrno(), gerror()
	call perror(errno)
  200	continue

	write(**,*) "-----"
	call serrno(-1)
	call perror("Last line          ")
	end
