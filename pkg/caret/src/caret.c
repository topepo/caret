/*
 *  class/class.c by W. N. Venables and B. D. Ripley  Copyright (C) 1994-2002
 *  modificatios by Andre Williams
 */

#define NO_S_TYPEDEFS

#include <R.h>

#define EPS 1e-4		/* relative test of equality of distances */

#define RANDIN  GetRNGstate()
#define RANDOUT PutRNGstate()
#define UNIF unif_rand()

#define MAX_TIES 1000
/* Not worth doing this dynamically -- limits k + # ties + fence, in fact */

typedef int Sint;

void
knn3(Sint *kin, Sint *lin, Sint *pntr, Sint *pnte, Sint *p,
       double *train, Sint *class, double *test,
       Sint *votes, Sint *nc, Sint *cv, Sint *use_all,double *all_vote)
{
    int   i, index, j, k, k1, kinit = *kin, kn, l = *lin, mm, npat, ntie,
          ntr = *pntr, nte = *pnte, extras;
    int   pos[MAX_TIES], nclass[MAX_TIES];
    int   j1, j2, needed, t;
    double dist, tmp, nndist[MAX_TIES];

    RANDIN;
/*
    Use a 'fence' in the (k+1)st position to avoid special cases.
    Simple insertion sort will suffice since k will be small.
 */

    for (npat = 0; npat < nte; npat++) {
	kn = kinit;
	for (k = 0; k < kn; k++)
	    nndist[k] = 0.99 * DBL_MAX;
	for (j = 0; j < ntr; j++) {
	    if ((*cv > 0) && (j == npat))
		continue;
	    dist = 0.0;
	    for (k = 0; k < *p; k++) {
		tmp = test[npat + k * nte] - train[j + k * ntr];
		dist += tmp * tmp;
	    }
/* Use 'fuzz' since distance computed could depend on order of coordinates */
	    if (dist <= nndist[kinit - 1] * (1 + EPS))
		for (k = 0; k <= kn; k++)
		    if (dist < nndist[k]) {
			for (k1 = kn; k1 > k; k1--) {
			    nndist[k1] = nndist[k1 - 1];
			    pos[k1] = pos[k1 - 1];
			}
			nndist[k] = dist;
			pos[k] = j;
/* Keep an extra distance if the largest current one ties with current kth */
			if (nndist[kn] <= nndist[kinit - 1])
			    if (++kn == MAX_TIES - 1)
				error("too many ties in knn");
			break;
		    }
	    nndist[kn] = 0.99 * DBL_MAX;
	}

	for (j = 0; j <= *nc; j++)
	    votes[j] = 0;
	if (*use_all) {
	    for (j = 0; j < kinit; j++)
		votes[class[pos[j]]]++;
	    extras = 0;
	    for (j = kinit; j < kn; j++) {
		if (nndist[j] > nndist[kinit - 1] * (1 + EPS))
		    break;
		extras++;
		votes[class[pos[j]]]++;
	    }
	} else { /* break ties at random */
	    extras = 0;
	    for (j = 0; j < kinit; j++) {
		if (nndist[j] >= nndist[kinit - 1] * (1 - EPS))
		    break;
		votes[class[pos[j]]]++;
	    }
	    j1 = j;
	    if (j1 == kinit - 1) { /* no ties for largest */
		votes[class[pos[j1]]]++;
	    } else {
/* Use reservoir sampling to choose amongst the tied distances */
		j1 = j;
		needed = kinit - j1;
		for (j = 0; j < needed; j++)
		    nclass[j] = class[pos[j1 + j]];
		t = needed;
		for (j = j1 + needed; j < kn; j++) {
		    if (nndist[j] > nndist[kinit - 1] * (1 + EPS))
			break;
		    if (++t * UNIF < needed) {
			j2 = j1 + (int) (UNIF * needed);
			nclass[j2] = class[pos[j]];
		    }
		}
		for (j = 0; j < needed; j++)
		    votes[nclass[j]]++;
	    }
	}

/* Use reservoir sampling to choose amongst the tied votes */
	ntie = 1;
	if (l > 0)
	    mm = l - 1 + extras;
	else
	    mm = 0;
	index = 0;
	for (i = 1; i <= *nc; i++){
	    if (votes[i] > mm) {
		ntie = 1;
		index = i;
		mm = votes[i];
	    } else if (votes[i] == mm && votes[i] >= l) {
		if (++ntie * UNIF < 1.0)
		    index = i;
	    }

		all_vote[npat*(*nc) + (i-1)] = (double)votes[i]/(kinit+extras);
	}

    }
    RANDOUT;
}



void
knn3reg(Sint *kin, Sint *pntr, Sint *pnte, Sint *p,
		double *train, double *y, double *test,
		double *means, Sint *cv, Sint *use_all)
{
	int   j, k, k1, kinit = *kin, kn, npat,
		  ntr = *pntr, nte = *pnte, extras;
	int   pos[MAX_TIES];
	int   j1, j2, needed, t;
	double dist, tmp, nndist[MAX_TIES], ny[MAX_TIES];

	int count;
	double sum;

	RANDIN;
	/*
	Use a 'fence' in the (k+1)st position to avoid special cases.
	Simple insertion sort will suffice since k will be small.
	*/

	for (npat = 0; npat < nte; npat++) {
		sum = 0.0;
		count = 0;
		kn = kinit;
		for (k = 0; k < kn; k++)
			nndist[k] = 0.99 * DBL_MAX;
		for (j = 0; j < ntr; j++) {
			if ((*cv > 0) && (j == npat))
				continue;
			dist = 0.0;
			for (k = 0; k < *p; k++) {
				tmp = test[npat + k * nte] - train[j + k * ntr];
				dist += tmp * tmp;
			}
			/* Use 'fuzz' since distance computed could depend on order of coordinates */
			if (dist <= nndist[kinit - 1] * (1 + EPS))
				for (k = 0; k <= kn; k++)
					if (dist < nndist[k]) {
						for (k1 = kn; k1 > k; k1--) {
							nndist[k1] = nndist[k1 - 1];
							pos[k1] = pos[k1 - 1];
						}
						nndist[k] = dist;
						pos[k] = j;
						/* Keep an extra distance if the largest current one ties with current kth */
						if (nndist[kn] <= nndist[kinit - 1])
							if (++kn == MAX_TIES - 1)
								error("too many ties in knn");
						break;
					}
					nndist[kn] = 0.99 * DBL_MAX;
		}

		if (*use_all) {
			for (j = 0; j < kinit; j++)
			{
				sum += y[pos[j]];
			}
			count += kinit;

			extras = 0;
			for (j = kinit; j < kn; j++) {
				if (nndist[j] > nndist[kinit - 1] * (1 + EPS))
					break;
				extras++;
				sum += y[pos[j]];
			}
			count += extras;


		} else { /* break ties at random */
			extras = 0;
			for (j = 0; j < kinit; j++) {
				if (nndist[j] >= nndist[kinit - 1] * (1 - EPS))
					break;
				sum += y[pos[j]];
			}
			count += j;
			j1 = j;
			if (j1 == kinit - 1) { /* no ties for largest */
				sum += y[pos[j1]];
				count += 1;
			} else {
				/* Use reservoir sampling to choose amongst the tied distances */
				j1 = j;
				needed = kinit - j1;
				for (j = 0; j < needed; j++)
					ny[j] = y[pos[j1 + j]];
				t = needed;
				for (j = j1 + needed; j < kn; j++) {
					if (nndist[j] > nndist[kinit - 1] * (1 + EPS))
						break;
					if (++t * UNIF < needed) {
						j2 = j1 + (int) (UNIF * needed);
						ny[j2] = y[pos[j]];
					}
				}
				for (j = 0; j < needed; j++) {
					sum += ny[j];
				}
				count += j;
			}
		}

		means[npat] = sum /(double)count;
	}
	RANDOUT;
}



#include "R_ext/Rdynload.h"

static const R_CMethodDef CEntries[] = {
	{"knn3", (DL_FUNC) &knn3, 13}, {"knn3reg", (DL_FUNC) &knn3reg, 10},
	{NULL, NULL, 0}
};

void R_init_caret(DllInfo *dll)
{
	R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
	R_useDynamicSymbols(dll, FALSE);
}
