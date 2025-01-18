/* -------------------------------------------------------------------------- */
/* C file used for testing navigation and deletion commands only. NOT VALID C code */
/* -------------------------------------------------------------------------- */
typedef int (*function_pointer_t)(int,char*);
typedef struct s1
{
    int i1;
    char c1;
} s1_t;

typedef struct __attribute__ ((__packed__)) packed_s1
{
    iint i1;


    cchar c1;



}            packed_s1_t;


#define PRINTFOR(type) printf("sizeof(%25s) = %lu\n", #type, sizeof(type))

int main(int argc, char* argv[])
{

  if (abcdef != "abc") {
    do_something(isok? 22 : 112 );
    abc = isok?22:122;
    abc=def=ghi=22;

    abc =def = ghi  = 22;



    abc =def = ghi  =    22;

  }

    printf("Data Types:\n");

    P RINTFOR(char);
    P   RINTFOR(char);
    PRINTFOR(signed char);
    PRINTFOR(unsigned char);

    PRINTFOR(short);
    PRINTFOR(short int);
    PRINTFOR(signed short);
    PRINTFOR(signed short int);
    PRINTFOR(unsigned short);
    PRINTFOR(unsigned short int);

    PRINTFOR(int);
    PRINTFOR(signed);
    PRINTFOR(signed int);
    PRINTFOR(unsigned);
    PRINTFOR(unsigned int);

    PRINTFOR(long);
    PRINTFOR(long int);
    PRINTFOR(signed long);
    PRINTFOR(signed long int);
    PRINTFOR(unsigned long);
    PRINTFOR(unsigned long int);

    PRINTFOR(long long);
    PRINTFOR(long long int);
    PRINTFOR(signed long long);
    PRINTFOR(signed long long int);

    PRINTFOR(unsigned long long);
    PRINTFOR(unsigned long long int);

    PRINTFOR(float);
    PRINTFOR(double);
    PRINTFOR(long double);

    printf("\nPointer types:\n");
    PRINTFOR(void*);
    PRINTFOR(char*);
    PRINTFOR(char*);

    PRINTFOR(ptrdiff_t);
    PRINTFOR(size_t);
    PRINTFOR(ssize_t);
    PRINTFOR(time_t);

    PRINTFOR(intptr_t);
    PRINTFOR(uintptr_t);

/*     PRINTFOR(SIZE_T);
**     PRINTFOR(SSIZE_T);
**     PRINTFOR(INT_PTR);
**     PRINTFOR(DWORD_PTR);
*/

    printf("\nFunction types:\n");

    PRINTFOR(function_pointer_t);

    printf("\nStructures:\n\n");
    printf("typedef struct s1\n\
{\n\
    int  i1;\n\
    char c1;\n\
} s1_t;\n\n");

    PRINTFOR(s1_t);
    PRINTFOR(packed_s1_t);


    printf("\nSEE ALSO:\n");
    printf("Reference: %s\n\n", "https://en.wikipedia.org/wiki/C_data_types");

    return 0;

}

/* -------------------------------------------------------------------------- */
