
/**************************************
 * Copyright LISP Machine, Inc. 1985  *
 *  See filename "Copyright" for      *
 * licensing and release information. *
 **************************************/

/* The fload module could provide either dynamic or static linking.
   Let us assume for now that it is static linking */


#include "fort.h"

int noop()
{return(0);}



int (*(fload(opcode)))() int opcode;
{ return(ftable[opcode]);}
