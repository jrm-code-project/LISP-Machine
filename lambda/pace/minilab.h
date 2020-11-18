/* Structure in track 0, block 10 of the disk ...
 *
 * If ml_magic is correct, ml_size bytes has been allocated for the structure.
 *
 * If ml_label is non-zero, then this structure replaces the old mini-label,
 *      and the lisp label is at blkno ml_label.  Block 22 should still have
 *      an old format mini-label.  For compatability with old lisp code and
 *      the old mini-label, ml_label should be a cylinder boundary.
 *
 *      The cmos ram uib is build from fmt_info; this should be enough
 *      state to support other disk controllers later.
 *
 *      If ml_oktracks is non-zero, it is the number tracks that useable.
 *      That is, (heads * sectors * cyls) - number-of-bad-tracks.
 *      If zero, subtract two or three cyls.
 */

struct minilab {
        union {
                struct {
                        long    mu_magic;       /* MINI */
                        long    mu_size;        /* size in bytes of structure */
                        long    mu_label;       /* blkno of lisp label */
                        long    mu_bklabel;     /* blkno of backup lisp label */
                        long    mu_badtrk;      /* actual bad-track info track*/
                        long    mu_sp1;
                        long    mu_oktracks;    /* number of good tracks */
                        struct fmt_info {
                                long    fi_type; /* index in diskinfo[] */
                                long    fi_heads;
                                long    fi_sectors;
                                long    fi_cyls;
                                long    fi_gap1;
                                long    fi_gap2;
                                long    fi_interleave;
                                long    fi_skew;
                                long    fi_secsize;
                                long    fi_badtrk; /* dfl bad-track info trk */
                                long    fi_bklbltrk; /* backup-label-track */
                                long    fi_sp0;         /* spares */
                                long    fi_sp1;
                                long    fi_sp2;
                        } mu_fmtinfo;
                } mu_l;
                char mu_b[1024];
        } ml_u;
};
#define ml_magic ml_u.mu_l.mu_magic
#define ml_size ml_u.mu_l.mu_size
#define ml_label ml_u.mu_l.mu_label
#define ml_bklabel ml_u.mu_l.mu_bklabel
#define ml_oktracks ml_u.mu_l.mu_oktracks
#define ml_fmtinfo ml_u.mu_l.mu_fmtinfo
#define ml_badtrack ml_u.mu_l.mu_badtrk

#define ml_type       ml_fmtinfo.fi_type
#define ml_heads      ml_fmtinfo.fi_heads
#define ml_sectors    ml_fmtinfo.fi_sectors
#define ml_cyls       ml_fmtinfo.fi_cyls
#define ml_gap1       ml_fmtinfo.fi_gap1
#define ml_gap2       ml_fmtinfo.fi_gap2
#define ml_interleave ml_fmtinfo.fi_interleave
#define ml_skew       ml_fmtinfo.fi_skew
#define ml_secsize    ml_fmtinfo.fi_secsize
#define ml_dflbadtrk  ml_fmtinfo.fi_badtrack

extern struct minilab ml;

/* 8086 / lambda order */
#define MINI 0x494E494D
#define FOOB 0x424f4f46
#define LISP 0x5053494c
#define LABL 0x4c42414c

#define MINILAB_BLOCK 10L

#define read_minilab(f,p) rdblk(f, (char *)p, MINILAB_BLOCK)
#define write_minilab(f,p) wrblk(f, (char *)p, MINILAB_BLOCK)

#define read_oldmini(f,p) rdblk(f, (char *)p, 22L)
#define write_oldmini(f,p) wrblk(f, (char *)p, 22L)

/* default number of bad cyls if not recorded by formatter.
 * this number was assumed by the old formatter and lisp code;
 * > 2.1 uses mini-label ml_oktracks instead.
 */
#define N_BAD_CYLS 1

struct diskinfo {
        char *di_name;
        struct fmt_info *di_info;
};
extern struct diskinfo diskinfo[];      /* libboot/diskinfo.c */
extern int ndisktypes;                  /* number of entries in diskinfo[] */
