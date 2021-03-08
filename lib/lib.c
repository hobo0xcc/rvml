#include <stdio.h>
#include <stdint.h>

uint8_t unit = 0;

float float_of_int(char *fv, int n) {
    return (float)n;
}

uint8_t print_float(char *fv, float f) {
    printf("%g", f);

    return unit;
}

uint8_t print_int(char *fv, int n) {
    printf("%d", n);

    return unit;
}

uint8_t print_newline(char *fv, uint8_t _unit1) {
    putchar('\n');

    return unit;
}
