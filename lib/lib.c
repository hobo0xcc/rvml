#include <stdio.h>
#include <stdint.h>
#include <math.h>

uint8_t unit = 0;

float mincaml_sin(char *fv, float f) {
    return sinf(f);
}

float mincaml_cos(char *fv, float f) {
    return cosf(f);
}

float mincaml_sqrt(char *fv, float f) {
    return sqrtf(f);
}

float mincaml_abs_float(char *fv, float f) {
    return fabsf(f);
}

int mincaml_int_of_float(char *fv, float f) {
    return truncf(f);
}

int mincaml_truncate(char *fv, float f) {
    return mincaml_int_of_float(fv, f);
}

float mincaml_float_of_int(char *fv, int n) {
    return (float)n;
}

uint8_t mincaml_print_float(char *fv, float f) {
    printf("%g", (float)f);

    return unit;
}

uint8_t mincaml_print_int(char *fv, int n) {
    printf("%d", n);

    return unit;
}

uint8_t mincaml_print_newline(char *fv, uint8_t _unit1) {
    putchar('\n');

    return unit;
}
