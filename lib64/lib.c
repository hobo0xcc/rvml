#include <stdio.h>
#include <stdint.h>
#include <math.h>

uint8_t unit = 0;

double mincaml_floor(char *fv, double f) {
    return floor(f);
}

double mincaml_atan(char *fv, double f) {
    return atan(f);
}

double mincaml_sin(char *fv, double f) {
    return sin(f);
}

double mincaml_cos(char *fv, double f) {
    return cos(f);
}

double mincaml_sqrt(char *fv, double f) {
    return sqrt(f);
}

double mincaml_abs_float(char *fv, double f) {
    return fabs(f);
}

int mincaml_int_of_float(char *fv, double f) {
    return trunc(f);
}

int mincaml_truncate(char *fv, double f) {
    return mincaml_int_of_float(fv, f);
}

float mincaml_float_of_int(char *fv, int n) {
    return (double)n;
}

uint8_t mincaml_print_float(char *fv, double f) {
    printf("%lg", (double)f);

    return unit;
}

uint8_t mincaml_print_int(char *fv, int n) {
    printf("%d", n);

    return unit;
}

uint8_t mincaml_print_byte(char *fv, int n) {
    printf("%c", (char)n);

    return unit;
}

uint8_t mincaml_prerr_byte(char *fv, int n) {
    fprintf(stderr, "%c", (char)n);

    return unit;
}

int mincaml_read_int(char *fv, uint8_t _unit) {
    int res;
    scanf("%d", &res);
    return res;
}

double mincaml_read_float(char *fv, uint8_t _unit) {
    double res;
    scanf("%lf", &res);
    return res;
}

uint8_t mincaml_print_newline(char *fv, uint8_t _unit1) {
    putchar('\n');

    return unit;
}
