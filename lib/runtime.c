#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void printInt(int x) {
    printf("%d\n", x);
}

void printString(char *s) {
    printf("%s\n", s);
}

void error() {
    printf("runtime error\n");
    exit(1);
}

int readInt() {
    int x;
    scanf("%d\n", &x);
    return x;
}

char *readString() {
    char *ptr = NULL;
    size_t size;
    int len;
    getline(&ptr, &size, stdin);
    len = strlen(ptr);
    ptr[strlen(ptr)-1] = 0;
    return ptr;
}

char *_concatString(char *first, char *second) {
    char *result = NULL;
    int len = strlen(first) + strlen(second) + 1;

    result = malloc(len);
    strcpy(result, first);
    strcat(result, second);
    return result;
}
