#include <stdint.h>
#include <unistd.h>
#ifndef __OpenBSD__ // OpenBSD does not use sys/random.h.
#include <sys/random.h>
#endif

uint64_t splitmix_init() {
	uint64_t result;
	int r = getentropy(&result, sizeof(uint64_t));
	return r == 0 ? result : 0xfeed1000;
}
