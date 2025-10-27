#include <stdint.h>
#include <unistd.h>

#if defined(__linux__) || defined(__NetBSD__) || defined(__FreeBSD__) || defined(__DragonFly__) || defined(__ANDROID__) || defined(__APPLE__)
#include <sys/random.h>
#endif

uint64_t splitmix_init() {
	uint64_t result;
	int r = getentropy(&result, sizeof(uint64_t));
	return r == 0 ? result : 0xfeed1000;
}
