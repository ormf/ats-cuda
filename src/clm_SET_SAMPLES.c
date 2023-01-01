/* translate set-samples in /home/orm/work/programmieren/lisp/ats-cuda/src/utilities.lisp to C
 *   written Sat 24-Dec-22 at 14:03 by CLM of 8-May-18
 */

#include <mus-config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <math.h>
#include <signal.h>
#include <cmus.h>

static sig_atomic_t got_sigint = 0; /* catch C-C if hung */
static void sig_err(int sig) {got_sigint = sig;}

