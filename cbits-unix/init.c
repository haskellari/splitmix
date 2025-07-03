// This is needed to ensure that "getentropy" is available when glibc is used.
#define _DEFAULT_SOURCE

#include <stdint.h>

#if defined(__GLIBC__) && (__GLIBC__ < 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ < 25))
// "getentropy" was added in glibc 2.25, so a fallback implementation is used
// for older versions.

#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

uint64_t splitmix_init() {
    /* if there is /dev/urandom, read from it */
    FILE *urandom = fopen("/dev/urandom", "r");
    if (urandom) {
        uint64_t result = 0;
        size_t r = fread(&result, sizeof(uint64_t), 1, urandom);
        fclose(urandom);

        if (r == 1) {
            return result;
        } else {
            return 0xfeed1000;
        }

    } else {
        /* time of day */
        struct timeval tp = {0, 0};
        gettimeofday(&tp, NULL);

        /* cputime */
        clock_t c = clock();

        /* process id */
        pid_t p = getpid();

        return ((uint64_t) tp.tv_sec)
            ^ ((uint64_t) tp.tv_usec)
            ^ ((uint64_t) c << 16)
            ^ ((uint64_t) p << 32);
    }
}

#else

#include <unistd.h>

uint64_t splitmix_init() {
	uint64_t result;
	int r = getentropy(&result, sizeof(uint64_t));
	return r == 0 ? result : 0xfeed1000;
}

#endif
