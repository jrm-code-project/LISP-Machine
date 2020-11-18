
/*  -*- Mode:Text -*-
 *
 * Copyright (c) 1984 Lisp Machine Inc.
 *
 * Declarations of system configuration structures.
 * This file created 7/15/84 17:07:52
 *
 */

/* this is lisp structure SYSTEM-CONFIGURATION-QS */
struct sysconf {
  long s_version;  /* VERSION-NUMBER */
  long s_size;  /* SIZE */
  long s_nproc;  /* NUMBER-OF-PROCESSORS */
  long s_psize;  /* PROCESSOR-BLOCK-SIZE */
  long s_sstruct;  /* SHARE-STRUCT-POINTER */
  long s_debug;  /* DEBUG-LEVEL */
  long s_lock;  /* LOCK */
  long s_ether;  /* ETHERNET-OWNER */
  long s_tapemaster;  /* TAPEMASTER-OWNER */
  long s_mti8;  /* MTI-8-LINE-OWNER  (not used) */
  long s_mti16;  /* MTI-16-LINE-OWNER */
  long s_qtr;  /* QUARTER-INCH-TAPE-OWNER */
  long s_sdua;  /* SDU-SERIAL-A-OWNER */
  long s_sdub;  /* SDU-SERIAL-B-OWNER */
  long s_stty0;  /* SHARE-TTY-0 */
  long s_stty1;  /* SHARE-TTY-1 */
  long s_stty2;  /* SHARE-TTY-2 */
  long s_grey_owner;  /* GREY-OWNER */
  long s_grey_slot;  /* GREY-SLOT */
  long s_nshare_tty;  /* NUMBER-OF-SHARE-TTYS */
  long s_tty_npages;  /* NUMBER-OF-SHARE-TTY-PAGES */
  long s_shr_base;      /* starting nubus address of user-def shared area */
  long s_shr_size;      /* size in bytes */
  long s_excelan;       /* excelan owner */
  long s_2excelan;      /* second board */
  long s_exc_initted;   /* excelan has been initialized */
  long s_2exc_initted;
  long s_int_map;       /* nubus addr of interrupt map; non-zero if used */
  long s_tm_base_map;   /* tapemaster map base */
  long s_tm_map_size;   /* tapemaster map size */
  long s_titn;          /* TITN lock */
  long s_console;       /* slot number of processor that is system console */
  long s_memb0;         /* nubus ram for sdu to run code in */
  long s_memp0;         /* size in bytes ... */
  long s_tm_iopb;       /* multibus ram offset for tapemaster iopb (newboot) */
  long s_ex_base_map;   /* excelan base map reg */
  long s_ex_map_size;   /* excelan map size */
  long s_clock;         /* cmos clock chip lock */
  long s_u_base_map;    /* user-defined area multibus map base (map number) */
  long s_u_map_size;    /* user-def map size (pages) */
#define MAX_QUAD 2
  long s_quad0;         /* lock for quad video screen 0 */
  long s_quad1;
  long s_quad2;
  long s_quad3;
  long s_quad4;
  long s_quad5;
  long s_quad6;
  long s_quad7;
  long s_newboot;       /* newboot version number */
  long s_sdumon;        /* sdu monitor version number */
  long s_burr_brown;    /* lock for burr-brown multibus card */
  long s_nu_disk;       /* lock for nubus disk control (just present or not) */
  long s_2181;          /* lock for Interphase 2181 disk controller */
  long s_unit0;         /* disk unit 0 has been initialized */
  long s_unit1;                 /* 0 = present but not initialized */
  long s_unit2;                 /* 1 = has been initialized */
  long s_unit3;                 /* -1 = not-present (later ...) */
  long s_unit4;
  long s_unit5;
  long s_unit6;
  long s_unit7;         /* disk unit 7 */
  long s_spare1;
  long s_spare2;
  long s_spare3;
  long s_spare4;
  long s_spare5;
};

/* this is lisp structure PROCESSOR-CONFIGURATION-QS */
struct procconf {
  long p_sysptr;  /* SYS-CONF-PTR */
  long p_slot;  /* SLOT-NUMBER */
  long p_major;  /* MAJOR-VERSION */
  long p_minor;  /* MINOR-VERSION */
  long p_switches;  /* STARTING-PROCESSOR-SWITCHES */
  long p_runme;  /* SHARE-RUNME */
  long p_sslot;  /* SHARE-SLOT */
  long p_type;  /* SHARE-TYPE */
  long p_iopb;  /* SHARE-IOPB */
  long p_intr;  /* SHARE-INTERRUPT-ADDR */
  long p_pare1;  /* SHARE-SPARE-1 */
  long p_pare2;  /* SHARE-SPARE-2 */
  long p_pare3;  /* SHARE-SPARE-3 */
  long p_pare4;  /* SHARE-SPARE-4 */
  long p_chaos_addr;  /* CHAOS-ADDRESS */
  long p_send_sharedev;  /* SEND-CHAOS-SHARE-DEV */
  long p_rcv_sharedev;  /* RCV-CHAOS-SHARE-DEV */
  long p_memb0;  /* MEMORY-BASE-0 */
  long p_memb1;  /* MEMORY-BASE-1 */
  long p_memb2;  /* MEMORY-BASE-2 */
  long p_memb3;  /* MEMORY-BASE-3 */
  long p_memb4;  /* MEMORY-BASE-4 */
  long p_memb5;  /* MEMORY-BASE-5 */
  long p_memb6;  /* MEMORY-BASE-6 */
  long p_memb7;  /* MEMORY-BASE-7 */
  long p_memb8;  /* MEMORY-BASE-8 */
  long p_memb9;  /* MEMORY-BASE-9 */
  long p_memp0;  /* MEMORY-BYTES-0 */
  long p_memp1;  /* MEMORY-BYTES-1 */
  long p_memp2;  /* MEMORY-BYTES-2 */
  long p_memp3;  /* MEMORY-BYTES-3 */
  long p_memp4;  /* MEMORY-BYTES-4 */
  long p_memp5;  /* MEMORY-BYTES-5 */
  long p_memp6;  /* MEMORY-BYTES-6 */
  long p_memp7;  /* MEMORY-BYTES-7 */
  long p_memp8;  /* MEMORY-BYTES-8 */
  long p_memp9;  /* MEMORY-BYTES-9 */
  long p_vcmem_slot;  /* VCMEM-SLOT; struct vcm_slot */
  long p_proc_type;  /* PROCESSOR-TYPE; PT_ */
  long p_micro_band;  /* MICRO-BAND */
  long p_load_band;  /* LOAD-BAND */
  long p_paging_band;  /* PAGING-BAND */
  long p_file_band;  /* FILE-BAND */
  long p_base_map;  /* BASE-MULTIBUS-MAPPING-REGISTER */
  long p_booted;  /* BOOT-STATUS */
  long p_chsh0;  /* CHAOS-SHARE-0 */
  long p_chsh1;  /* CHAOS-SHARE-1 */
  long p_chsh2;  /* CHAOS-SHARE-2 */
  long p_chsh3;  /* CHAOS-SHARE-3 */
  long p_chsh4;  /* CHAOS-SHARE-4 */
  long p_parity_enable;
  long p_scan_size;
  long p_map_size;
  long p_boot_cmd;
  long p_boot_mode;
  long p_console;       /* console type CT_; if screen, p_vcmem is slot+type */
  long p_baudrate;      /* console baud rate, if serial */
  long p_watchdog;
  long p_int_map;       /* pre-allocated map reg to point at interrupt page */
  long p_spare1;
  long p_spare2;
  long p_spare3;
  long p_spare4;
  long p_spare5;
};

/* this is lisp structure CHAOS-SHARE-DEV-QS */
struct chsharedev {
  long s_csr;  /* CHAOS-SHARE-CSR */
  long s_offset;  /* CHAOS-SHARE-SIZE */
  long s_bufsiz;  /* CHAOS-SHARE-BUF-SIZE */
  long s_intr;  /* CHAOS-SHARE-INTR-ADDR */
  long s_length;  /* CHAOS-SHARE-PKT-LENGTH */
};

/* this is lisp structure SHARE-TTY-QS */
struct ttyshare {
  unsigned char s_rxoffs;  /* LISP-TO-UNIX-BUFFER */
  char XX163, XX164, XX165;
  unsigned char s_txoffs;  /* UNIX-TO-LISP-BUFFER */
  char XX166, XX167, XX168;
  unsigned char s_bufsiz;  /* BUF-SIZE */
  char XX169, XX170, XX171;
  long s_unixintr;  /* UNIX-INTR */
  long s_lamintr;  /* LAM-INTR */
  unsigned char s_rxrp;  /* LISP-TO-UNIX-OUT-PTR */
  char XX172, XX173, XX174;
  unsigned char s_rxwp;  /* LISP-TO-UNIX-IN-PTR */
  char XX175, XX176, XX177;
  unsigned char s_txrp;  /* UNIX-TO-LISP-OUT-PTR */
  char XX178, XX179, XX180;
  unsigned char s_txwp;  /* UNIX-TO-LISP-IN-PTR */
  char XX181, XX182, XX183;
  unsigned char s_lcsr;  /* LCSR */
  char XX184, XX185, XX186;
  unsigned char s_ucsr;  /* UCSR */
  char XX187, XX188, XX189;
  long s_owner;  /* OWNER */
  unsigned char s_minor;
  char XX190, XX191, XX192;
};

/* bits from CHAOS-SHARE-DEV-CSR-BITS */
#define VALID 01L /* VALID-BIT */

/* bits from LAMBDA-PROCESSOR-SWITCHES-BITS */
#define USE_USEC_CLOCK          020000000000L /* USE-STAT2-FOR-USEC-CLOCK */
#define ALLOW_BOOT_CHARS        010000000000L /* ALLOW-BOOT-CHARS */
#define USE_MULTIPLIER          04000000000L /* USE-MULTIPLIER-IN-UC-TV */
#define USE_DISK_SHARE          02000000000L /* USE-DISK-SHARING-PROTOCOL */
#define PROM_COLD_BOOTS         01000000000L /* PROM-JUMPS-TO-COLD-BOOT */
#define SLOT_NUMBERS_SET_UP     0400000000L  /* SLOT-NUMBERS-SET-UP */
#define VALID_2X2               0200000000L  /* 2X2-STUFF-VALID-IN-CONF... */
#define FAST_CACHE              020L         /* use fast cache csmram */
#define VIDEO_CACHE             010L         /* CACHE-PERMIT-FOR-VIDEO-BUFFER */
#define CACHE_ON                04L          /* CACHE-PERMIT */
#define BLK_XFER_SIZE           03L          /* PACKET-SIZE-CODE */

/* bits from SHARE-TTY-CSR-BITS */
#define SHR_CARR 01L /* CARRIER */
#define SHR_RAW 02L /* RAW */
#define SHR_OPENED 4 /* line has been opened by unix */
#define SHR_DEBUG 8 /* unix-to-lisp is being driven by newboot */


/* procconf->p_console - console type
 *      if VCMEM or QUAD, slot number and screen are p_vcmem_slot and p_screen
 *      if SERIAL, is ttya; if SHARETTY, is sharetty #0
 */
#define CT_SERIAL   0   /* ttya for unix console */
#define CT_VCMEM    1   /* vcmem / AI kbd for lambda or unix */
#define CT_QUAD     2   /* 4-port video / AI kbd for lambda or unix */
#define CT_SHARETTY 3   /* sharetty for unix console */
#define CT_TTYB     4   /* ttyb for unix console */

/* sysconfp->s_int_map */
struct int_map {
        short im_type;          /* none, sdu or nubus */
        short im_p0;
        long im_addr;           /* sdu function or nubus interrupt address */
        short im_ds;            /* ds for sdu handler */
        short im_pic_offs;      /* pic control reg offset for sdu handler */
        short im_size;          /* size in words of struct int_map */
        short im_p1;
};
/* im_type types */
#define IM_NONE  0              /* line is on but sdu handler no-ops */
#define IM_SDU   1              /* handler calls sdu code */
#define IM_NUBUS 2              /* handler writes 1 to nubus address */

/* structure that overloads procconf->p_vcmem_slot */
struct vcm_slot {
        char vcs_slot;          /* slot number, 0..31 */
        char vcs_type;          /* CT_ */
        char vcs_screen;        /* screen number, 0..N */
        unsigned char vcs_hi;   /* 0xff if no board present; 0=port, 1=land */
};
