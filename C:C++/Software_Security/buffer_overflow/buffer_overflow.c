#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define HASH_OF_CORRECT_PASSWORD 0x4b665b0

void winner_handler() {
    printf("\033[32mCongraz! You complete the task!\033[0m\n");
    exit(0);
}

unsigned int getHash(const char* str, unsigned int len){
    const unsigned int bitsOfUnsignedInt = (unsigned int)(sizeof(unsigned int) * 8);
    const unsigned int threeQuarters = (unsigned int)((bitsOfUnsignedInt * 3)/4);
    const unsigned int halfQuarter = (unsigned int)(bitsOfUnsignedInt/8);
    const unsigned int highBits = (unsigned int)(0xFFFFFFFF <<(bitsOfUnsignedInt - halfQuarter));

    unsigned int hash = 0;
    unsigned int test = 0;

    for(int i = 0; i < len; ++i){
        hash = (hash << halfQuarter) + (*str++);

        if((test = hash & highBits) != 0){
            hash = ((hash^(test >> threeQuarters)) & (~highBits));
        }
    }

    return hash;
}

void setup() {
    setvbuf(stdin, NULL, _IONBF, 0);
    setvbuf(stdout, NULL, _IONBF, 0);
    setvbuf(stderr, NULL, _IONBF, 0);
}

int main() {
    setup();

    char user_name[48] = {0};
    char password[48] = {0};
    char flags[48] = {0};
    printf("Please input your user name> ");
    scanf("%s", user_name);
    printf("Please input your password> ");
    scanf("%s", password);
    if (strncmp(user_name, "CoatOfArms", 10) == 0 && 
        getHash(password, 32) == HASH_OF_CORRECT_PASSWORD) {
        strncpy(flags, "AcCes50k", 8);
    }
    if (strncmp(flags, "AcCes50k", 8) == 0) {
        printf("[+] Login Success!\n");
        winner_handler();
    }
    else {
        printf("[-] Wrong user name or password\n");
    }
}

// gcc -g ./buffer_overflow.c -o ./buffer_overflow
