
main()
{
    int gate;

    if ((gate = api_name_resolve("SESSMGR")) == -1) {
	printf("api_sup_errno = 0x%x.\n", api_sup_errno);
    } else {
	printf("SESSMGR is %d.\n", gate);
    }
    if ((gate = api_name_resolve("KEYBOARD")) == -1) {
	printf("api_sup_errno = 0x%x.\n", api_sup_errno);
    } else {
	printf("KEYBOARD is %d.\n", gate);
    }
    if ((gate = api_name_resolve("COPY")) == -1) {
	printf("api_sup_errno = 0x%x.\n", api_sup_errno);
    } else {
	printf("COPY is %d.\n", gate);
    }
    if ((gate = api_name_resolve("OIAM")) == -1) {
	printf("api_sup_errno = 0x%x.\n", api_sup_errno);
    } else {
	printf("OIAM is %d.\n", gate);
    }
}
