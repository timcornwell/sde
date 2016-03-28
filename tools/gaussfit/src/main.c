#include <stdio.h>
#include <string.h>

#include "main.proto.h"

 main()
 {
	int i, file_prec;
	char dnum[30];


	file_prec = 28;
	strcpy(dnum , "AAAAAAAAAAAAAAAAAAAAAA");
	i = file_prec;
	printf("%-*s\tbooh!\n",file_prec,dnum);
	printf("%s\tbooh!\n",dnum);
	strcpy(dnum , "AAAAAAAAAAAAAAAA");
	printf("%-*s\tbooh!\n",file_prec,dnum);
	printf("%s\tbooh!\n",dnum);
	strcpy(dnum , "AAAAAAAAAAAAA");
	printf("%-*s\tbooh!\n",file_prec,dnum);
	printf("%s\tbooh!\n",dnum);
	strcpy(dnum , "AAAAAAA");
	printf("%-*s\tbooh!\n",file_prec,dnum);
	printf("%s\tbooh!\n",dnum);
	strcpy(dnum , "AAAA");
	printf("%-*s\tbooh!\n",file_prec,dnum);
	printf("%s\tbooh!\n",dnum);
 }
