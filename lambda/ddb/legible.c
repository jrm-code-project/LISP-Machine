/*** legible.c ***
**** program copies input file spec to output file spec
**** replacing unusual non-visible chracters with newlines
**** DDBII 6/20/1988   ***/


#include <stdio.h>

#define TRUE  1
#define FALSE 0


main(argc,argv)

  int argc;          /* number of arguments */
  char *argv[];      /* string pointer to args */

  {
    int  bcnt, ccnt;       /* byte count and changed count */
    char infilename[40], outfilename[40], tmpstr[80], byte;
    FILE *infile, *outfile, *fopen();

    switch (argc)
      {
        case 3:      /* both input and output specs given */
          strcpy(infilename,argv[1]);
          strcpy(outfilename,argv[2]);
          break;
        case 2:      /* one spec given */
          strcpy(outfilename,argv[1]);
          strcpy(infilename,outfilename);
          strcat(infilename,".old");
          sprintf(tmpstr,"mv %s %s",outfilename,infilename);
          system(tmpstr);       /* rename input file */
          break;
        default:
          printf ("name of input file: ");
          scanf ("%s",infilename);
          printf ("name of output file: ");
          scanf ("%s",outfilename);
          break;
      }

    if ((infile = fopen(infilename,"r")) == NULL)
      {
        printf ("?error with input spec...\n");
        exit(1);
      }

    if ((outfile = fopen(outfilename,"w")) == NULL)
      {
        printf ("?error with output spec...\n");
        exit(1);
      }

    ccnt = 0;
    bcnt = 0;
    while ((byte = getc(infile)) != EOF)
      {
        if (visible(byte))
          putc(byte,outfile);
        else
          {
            putc('\n',outfile);
            ccnt++;
          }
        bcnt++;
      }

    fclose(infile);
    fclose(outfile);
    printf ("Copied %d, changed %d (bytes)\n",
                                bcnt, ccnt);
  }


int visible(byte)       /* TRUE if normal char */
        char   byte;
    {
        if ((byte >= ' '  ||  byte == '\t'  ||  byte == '\n'
                ||  byte == '\f')  &&  byte <= '~')
            return(TRUE);
        return(FALSE);
    }
