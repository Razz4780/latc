#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define SAFE_STR(x) (x == NULL ? "" : x)

typedef struct {
    void * elems;
    int32_t length;
} array_t;

void _fun_error() {
    printf("runtime error\n");
    exit(1);
}

void _fun_printInt(int32_t i) {
    if (printf("%d\n", i) < 0) {
        fprintf(stderr, "writing to stdout failed\n");
        exit(1);
    }
}

void _fun_printString(char const * s) {
    char const * safe = SAFE_STR(s);
    if (printf("%s\n", safe) < 0) {
        fprintf(stderr, "writing to stdout failed\n");
        exit(1);
    }
}

char * _fun_readString() {
    char * buffer = NULL;
    size_t n;
    if (getline(&buffer, &n, stdin) < 0) {
        fprintf(stderr, "reading from stdin failed\n");
        exit(1);
    }

    size_t len = strlen(buffer);
    if (buffer[len - 1] == '\n') {
        buffer[len - 1] = '\0';
    }

    return buffer;
}

int32_t _fun_readInt() {
    char * line = _fun_readString();

    int32_t i;
    if (sscanf(line, "%d", &i) != 1) {
        fprintf(stderr, "reading an integer from stdin failed\n");
        exit(1);
    }

    free(line);

    return i;
}

void * _lib_newObject(int32_t size, void * vtable_ptr) {
    void * obj = calloc(1, size);
    if (obj == NULL) {
        fprintf(stderr, "allocating memory failed\n");
        exit(1);
    }

    void ** casted = (void **) obj;
    casted[0] = vtable_ptr;

    return obj;
}

array_t * _lib_newArray(int32_t elem_size, int32_t length) {
    if (length < 0) {
        fprintf(stderr, "array length cannot be negative\n");
        exit(1);
    }

    if (elem_size < 1) {
        fprintf(stderr, "array element size cannot be less than 1\n");
        exit(1);
    }

    array_t * array = calloc(1, sizeof(array_t));
    if (array == NULL) {
        fprintf(stderr, "allocating memory failed\n");
        exit(1);
    }

    if (length > 0) {
        void * elems = calloc(length, elem_size);
        if (elems == NULL) {
            fprintf(stderr, "allocating memory failed\n");
            exit(1);
        }
        array->elems = elems;
        array->length = length;
    }

    return array;
}

char * _lib_addStrings(char const * s1, char const * s2) {
    char const * safe_1 = SAFE_STR(s1);
    char const * safe_2 = SAFE_STR(s2);
    size_t size_1 = strlen(safe_1);
    size_t size_2 = strlen(safe_2);

    char * buffer = malloc(size_1 + size_2);
    if (buffer == NULL) {
        fprintf(stderr, "allocating memory failed\n");
        exit(1);
    }

    strcpy(buffer, safe_1);
    strcpy(buffer + size_1, safe_2);

    return buffer;
}

int _lib_cmpStrings(char const * s1, char const * s2) {
    char const * safe_1 = SAFE_STR(s1);
    char const * safe_2 = SAFE_STR(s2);

    if (strcmp(safe_1, safe_2)) {
        return 0;
    } else {
        return 1;
    }
}
