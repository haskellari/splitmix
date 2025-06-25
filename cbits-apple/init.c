#include <stdint.h>
#include <Security/SecRandom.h>

uint64_t splitmix_init() {
	uint64_t result;
	int r = SecRandomCopyBytes(kSecRandomDefault, sizeof(uint64_t), &result);
	return r == errSecSuccess ? result : 0xfeed1000;
}
