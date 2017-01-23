#include <stdio.h>
#include <stdlib.h>

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
    scanf("%d", &x);
    return x;
}

char *readString() {
    char *ptr = NULL;
    size_t size;
    getline(&ptr, &size, stdin);
    return ptr;
}
