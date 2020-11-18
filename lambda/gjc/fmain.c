
/**************************************
 * Copyright LISP Machine, Inc. 1985  *
 *  See filename "Copyright" for      *
 * licensing and release information. *
 **************************************/

#include <stdio.h>

#include <share.h>

main(argc,argv) int argc; char **argv;
{ int n,chan;
  char c;
  if (argc != 2) {printf("\n%s <syncronizing-device>\n",argv[0]);exit(1);}
  chan = open(argv[1],2);
  if (chan < 0) {printf("\ncouldnt open syncronizing device: %s\n",argv[1]);
                          exit(1);}
  if (share_setup() < 0) {printf("share_setup failed");exit(1);}
  while(1)
  { n = read(chan,&c,1);
    if (n != 1) {printf("\nsyncronizing device read failed\n");exit(1);}
    if ( c == 'S') {printf("\nbeen told to stop\n");exit(1);}
    if ( c == 'X') fexec(sharebase);}}
