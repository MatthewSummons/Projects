#include <stdio.h>
#include <string.h>
#include <stdlib.h>

char secret_message[8];

#define MAX_USERS 4
#define NAME_SIZE 8
struct Name {
    char name[NAME_SIZE];
};
struct NameList {
    unsigned long int user_num;
    struct Name users[MAX_USERS];
};


void view_user(char* data) {
    // this function's source code is irrelevant to the task requirements
    // it does pretty output of the data in a name
}

void generate_random_string(char *dest, size_t length) {
    // this function's source code is irrelevant to the task requirements
}

void input_name(char* dest) {
    // this function's source code is irrelevant to the task requirements
    // it takes user input and put it into a destination
}

int entry(int is_admin) {
    unsigned int index;
    int choice;
    struct NameList NameList;
    NameList.user_num = 0;
    for (int i = 0; i < MAX_USERS; i++) {
        memset(NameList.users[i].name, 0, NAME_SIZE);
    }
    
    if (is_admin != 0) {
        generate_random_string(secret_message, 8);
        printf("\nDear admin, here are the secret message for you %s\n", secret_message);
    } 

    while (1) {
        printf("======== COMP3355's StudentList =========\n");

        printf("1. Add\n");
        printf("2. View\n");
        printf("3. Delete\n");
        printf("4. Refresh\n");
        printf("5. Exit\n");
        printf("------ Used Pages: %- 11lu -------\n", NameList.user_num);

        printf("--------------------------------------\n");
        printf("Choose an option: ");
        scanf("%d", &choice);
        getchar();
        fflush(stdin);

        switch (choice) {

            case 1:
                input_name(NameList.users[NameList.user_num].name);

                printf("Added:\n");
                view_user(NameList.users[NameList.user_num].name);
                
                NameList.user_num++;
                break;

            case 2:
                printf("Enter an index to view: ");
                scanf("%d", &index);
                
                if (index >= NameList.user_num || index < 0) {
                    printf("Invalid index!\n");
                    return;
                }

                view_user(&NameList.users[index].name);
                break;

            case 3:
                printf("Enter an index to delete: ");
                scanf("%d", &index);
                
                if (index > NameList.user_num || index < 0) {
                    printf("Invalid index!\n");
                    return;
                }

                int a = NameList.user_num - 1;
                for (int i = index; i < a; i++) {
                    NameList.users[i] = NameList.users[i + 1];
                }
                
                NameList.user_num--;
                printf("Deleted!\n");
                break;

            case 4:
                return 1;

            case 5:
                return 0;

            default:
                printf("Invalid choice!\n");
        }
        printf("\n");

    }
}

int main() {
    long int is_admin = 0; 
    char input_password[8];
    char admin_password[8];
    
    memset(&input_password, 0, 8);
    generate_random_string(admin_password, 7);

    printf("\nThis is COMP3355's student list\n");
    printf("\nEnter your password: ");
    scanf("%8s", input_password);

    if (strncmp(input_password, admin_password, 7) == 0) {
        is_admin = 1;
    }

    while (1) {
        if (entry(is_admin) == 0) {
            break;
        }
    }
    
    return 0;
}
