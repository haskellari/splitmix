#include <stdint.h>
#include <windows.h>
#include <ntsecapi.h>

uint64_t splitmix_init() {
	uint64_t result;
	int r = RtlGenRandom(&result, sizeof(uint64_t));
	return r ? result : 0xfeed1000;
}
