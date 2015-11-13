#include <pocketsphinx.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "pocketsphinx_internal.h"
#include "fsg_search_internal.h"
#include "ps_lattice_internal.h"
#include "test_macros.h"

int
main(int argc, char *argv[])
{
    ps_decoder_t *ps;
    cmd_ln_t *config;
    acmod_t *acmod;
    fsg_search_t *fsgs;
    ps_lattice_t *dag;
    ps_seg_t *seg;
    int32 score;

    TEST_ASSERT(config =
            cmd_ln_init(NULL, ps_args(), TRUE,
                "-hmm", MODELDIR "/en-us/en-us",
                "-fsg", DATADIR "/goforward.fsg",
                "-dict", DATADIR "/turtle.dic",
                "-bestpath", "no",
                "-input_endian", "little",
                "-samprate", "16000", NULL));
    TEST_ASSERT(ps = ps_init(config));

    fsgs = (fsg_search_t *) ps->search;
    acmod = ps->acmod;

    setbuf(stdout, NULL);
    {
        FILE *rawfh;
        int16 buf[2048];
        size_t nread;
        int16 const *bptr;
        char const *hyp;
        int nfr;
        int is_final;

        TEST_ASSERT(rawfh = fopen(DATADIR "/goforward.raw", "rb"));
        TEST_EQUAL(0, acmod_start_utt(acmod));
        fsg_search_start(ps_search_base(fsgs));
        while (!feof(rawfh)) {
            nread = fread(buf, sizeof(*buf), 2048, rawfh);
            bptr = buf;
            while ((nfr = acmod_process_raw(acmod, &bptr, &nread, FALSE)) > 0) {
                while (acmod->n_feat_frame > 0) {
                    fsg_search_step(ps_search_base(fsgs),
                            acmod->output_frame);
                    acmod_advance(acmod);
                }
                hyp = fsg_search_hyp(ps_search_base(fsgs), &score, &is_final);
                printf("FSG: %d %s (%d) %s\n", acmod->output_frame, hyp, score, is_final ? "FINAL" : "");
                TEST_EQUAL (is_final, acmod->output_frame > 173);
            }
        }
        fsg_search_finish(ps_search_base(fsgs));
        hyp = fsg_search_hyp(ps_search_base(fsgs), &score, NULL);
        printf("FSG: %s (%d)\n", hyp, score);

        TEST_ASSERT(acmod_end_utt(acmod) >= 0);
        fclose(rawfh);
    }
    TEST_EQUAL(0, strcmp("go forward ten meters",
                 fsg_search_hyp(ps_search_base(fsgs), &score, NULL)));
    for (seg = ps_seg_iter(ps, &score); seg;
         seg = ps_seg_next(seg)) {
        char const *word;
        int sf, ef;
        int32 post, lscr, ascr, lback;

        word = ps_seg_word(seg);
        ps_seg_frames(seg, &sf, &ef);
        if (sf == ef)
            continue;
        post = ps_seg_prob(seg, &ascr, &lscr, &lback);
        printf("%s (%d:%d) P(w|o) = %f ascr = %d lscr = %d lback = %d\n", word, sf, ef,
               logmath_exp(ps_get_logmath(ps), post), ascr, lscr, lback);
    }

    /* Now get the DAG and play with it. */
    dag = ps_get_lattice(ps);
    ps_lattice_write(dag, "test_fsg.lat");
    printf("BESTPATH: %s\n",
           ps_lattice_hyp(dag, ps_lattice_bestpath(dag, NULL, 1.0, 15.0)));
    ps_lattice_posterior(dag, NULL, 15.0);
    ps_free(ps);
    cmd_ln_free_r(config);

    return 0;
}
