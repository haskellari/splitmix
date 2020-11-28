#include "TestU01.h"

#include <stdint.h>

/* Utilities */

inline unsigned int popcount32(uint32_t i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0xF0F0F0F) * 0x1010101) >> 24;
}

inline uint64_t rotl64(uint64_t value, unsigned int count) {
    return value << count | value >> (64 - count);
}

/* For comparison, SplitMix32 generator in C */
#define GOLDEN_GAMMA 0x9e3779b9U

static uint32_t seed  = 0;
static uint32_t gamma = 0;

uint32_t mix32(uint32_t z) {
    z = (z ^ (z >> 16)) * 0x85ebca6b;
    z = (z ^ (z >> 13)) * 0xc2b2ae35;
    z = (z ^ (z >> 16));
    return z;
}

uint32_t mix32gamma(uint32_t z) {
    z = (z ^ (z >> 16)) * 0x69ad6ccbU;
    z = (z ^ (z >> 13)) * 0xcd9ab5b3U;
    z = (z ^ (z >> 16));
    return z;
}

void splitmix32_init(uint32_t s) {
    seed  = mix32(s);
    gamma = mix32gamma(s + GOLDEN_GAMMA) | 0x1;
    if (popcount32(gamma ^ (gamma >> 1)) < 12) {
        gamma = gamma ^ 0xaaaaaaaa;
    }
}

unsigned int splitmix32() {
    seed = seed + gamma;
    return mix32(seed);
}

/* Exported from Haskell */
uint32_t haskell_splitmix32();

unsigned int exported_splitmix32() {
    return haskell_splitmix32();
}

uint32_t haskell_splitmix64();

unsigned int exported_splitmix64() {
    return haskell_splitmix64();
}

double haskell_splitmix64_double();
double haskell_splitmix32_double();

/* Test suite */

int run_testu01(int gen_k, int bat_k) {
    /* Create TestU01 PRNG object for our generator */
    unsigned int (*funcBits)() = NULL;
    double (*func01)() = NULL;
    unif01_Gen* gen = NULL;

    switch (gen_k) {
        case 0:
            func01 = haskell_splitmix64_double;
            gen = unif01_CreateExternGen01 ("SplitMix (Double)", haskell_splitmix64_double);
            break;

        case 1:
            funcBits = exported_splitmix64;
            gen = unif01_CreateExternGenBits("SplitMix (low 32bit)", exported_splitmix64);
            break;

        case 2:
            func01 = haskell_splitmix32_double;
            gen = unif01_CreateExternGen01("SplitMix32 (Double)", haskell_splitmix32_double);
            break;

        case 3:
            funcBits = exported_splitmix32;
            gen = unif01_CreateExternGenBits("SplitMix32", exported_splitmix32);
            break;

        default:
            splitmix32_init(42);
            printf("Initial state: %u %u\n", seed, gamma);

            funcBits = splitmix32;
            gen = unif01_CreateExternGenBits("SplitMix32 (C implementation)", splitmix32);
    }

    /* Run the tests. */
    switch (bat_k) {
        case 0:
            bbattery_SmallCrush(gen);
            break;

        case 1:
            bbattery_Crush(gen);
            break;

        case 2:
            bbattery_BigCrush(gen);
            break;

        default:
            if (funcBits != NULL) {
                for (int i = 0; i < 32; i++) {
                    printf("%x\n", funcBits());
                }
            }

            if (func01 != NULL) {
                for (int i = 0; i < 32; i++) {
                    printf("%.09lf\n", func01());
                }
            }
    }

    if (funcBits != NULL) {
        unif01_DeleteExternGenBits(gen);
    } else if (func01 != NULL) {
        unif01_DeleteExternGen01(gen);
    }

    return 0;
}
