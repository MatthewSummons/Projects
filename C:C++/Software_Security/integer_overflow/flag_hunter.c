#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <time.h>


int MAX_MAGE = 500;
struct Warrior
{
    int type;
    unsigned int health;
    int mage;
    int deadly_slashed_used;
    int burn_cost;
    int shield_cost;
    int damage;
    int deadly_damage;
    int deadly_cost;
};

struct Guardian
{
	int type;
	int health;
	int damage;
	int heal;
};

void winner_handler() {
    printf("\033[32mCongraz! You complete the task!\033[0m\n");
    exit(0);
}

int getInt(char prompt[], int lower, int upper) {
	int a = -1;
	while (!(a >= lower && a <= upper)) {
		printf("%s", prompt);
		scanf("%d", &a);
		while(getchar() != 10);
	}
	return a;
}

void print_hero_info(int type) {
	if (type == 0 || type == 1) {
		printf("Hero: Mage\n Skills:\n");
		printf("    [1] Burn: Causes 200000000 Damage [cost : 100 MP]\n");
		printf("    [2] Drink potion: Refresh MP [cost : 0 MP]\n");
		printf("    [3] Water Shield: Defense [cost : 25 MP]\n");;
	}
	if (type == 0 || type == 2) {
		printf("Hero: Slayer\n Skills:\n");
		printf("    [1] Slash: Causes 200000000 Damage\n");
		printf("    [2] Deadly Slash: Sacrifices 200000000 health and causes 500000000 damage!\n");
	}
}

void print_status(struct Guardian* guardian, struct Warrior* warrior) {
	printf("\n================================================================\n");
	printf("\t---------------------------------------------\n");
	printf("\tGuardian health: %d damage: %d\n", guardian->health, guardian->damage);
	printf("\tYour helath: %u mage: %d\n", warrior->health, warrior->mage);
	printf("\t---------------------------------------------\n");
	print_hero_info(warrior->type);
}

void hunt() {
	struct Warrior warrior;
	int choice;
	print_hero_info(0);
	choice = getInt(">>Choose your hero:\n\t[1] Mage\n\t[2] Slayer\n>>", 1, 2);
	if (choice == 1) {
		warrior.type = 1;
		warrior.health = 400000000;
		warrior.mage = MAX_MAGE;
		warrior.burn_cost = 100;
		warrior.shield_cost = 25;
		warrior.deadly_slashed_used = 0;
		warrior.damage = 200000000;
		warrior.deadly_damage = 0;
		warrior.deadly_cost  =  0;
	} else if (choice == 2) {
		warrior.type = 2;
		warrior.health =  400000000;
		warrior.mage = 0;
		warrior.burn_cost = 0;
		warrior.shield_cost = 0;
		warrior.deadly_slashed_used = 0;
		warrior.damage =  200000000;
		warrior.deadly_damage = 500000000;
		warrior.deadly_cost  =  200000000;
	}

	struct Guardian guardian;
	printf("\n!!!Guardian has appeared!!!\n");
	guardian.type = 1;
	guardian.health = 2000000000;
	guardian.damage =  100000000;
	guardian.heal =      5000000;

	while (1) {
		print_status(&guardian, &warrior);
		if (warrior.type == 2) {
			choice = getInt(">>Your choice of skill:\n>>", 1, 2);
			if (choice == 1) {
				guardian.health -= warrior.damage;
				warrior.health -= guardian.damage;
				printf("[!] Slash deals %d damage to the Guardian!\n", warrior.damage);
				printf("[!] Guardian cause %d damage to you!\n", guardian.damage);
				guardian.health += guardian.heal;
				printf("[!] Guardian heals %d health!\n", guardian.heal);
			} else if (choice == 2) {
				if (warrior.deadly_slashed_used >= 1) printf("You can't use it more than once\n");
				else if (warrior.health >= warrior.deadly_cost) {
					warrior.health -= warrior.deadly_cost;
					guardian.health -= warrior.deadly_damage;
					printf("[!] Deadly Slash deals %d damage to the Guardian!\n", warrior.deadly_damage);
					warrior.health -= guardian.damage;
					printf("[!] Guardian cause %d damage to you!\n", guardian.damage);
					guardian.health += guardian.heal;
					printf("[!] Guardian heals %d health!\n", guardian.heal);
					warrior.deadly_slashed_used = 1;
				} else {
					printf("[!] Your health not enough!\n");
				}
			}

		} 
        else if (warrior.type == 1) {
			choice = getInt(">>Your choice of skill:\n>>", 1, 3);
			if (choice == 1) {
				if (warrior.mage >= warrior.burn_cost) {
					warrior.mage -= warrior.burn_cost;
                    guardian.health -= warrior.damage;
					printf("[!] Magic Fire deals %d damage to the Guardian!\n", warrior.damage);
				} else {
					printf("[!] Your mage not enough!\n");
				}

                if (warrior.health >= guardian.damage) {
    			    warrior.health -= guardian.damage;
                }
                else {
    			    warrior.health = 0;
                }
				printf("[!] Guardian cause %d damage to you!\n", guardian.damage);
				guardian.health += guardian.heal;
				printf("[!] Guardian heals %d health!\n", guardian.heal);
			} else if (choice == 2) {
				warrior.mage = MAX_MAGE;
				printf("[!] Your mage has been refreshed\n");
                if (warrior.health >= guardian.damage) {
    				warrior.health -= guardian.damage;
                }
                else {
    				warrior.health = 0;
                }
				printf("[!] Guardian cause %d damage to you!\n", guardian.damage);
				guardian.health += guardian.heal;
				printf("[!] Guardian heals %d health!\n", guardian.heal);
			} else if (choice == 3) {
				if (warrior.mage >= warrior.shield_cost) {
					warrior.mage -= warrior.shield_cost;
					printf("[!] You use your water shield\n");
				    printf("[!] Guardian cause %d damage to you!\n", guardian.damage/10);
				    warrior.health -= guardian.damage/10;
					guardian.health += guardian.heal;
					printf("[!] Guardian heals %d health!\n", guardian.heal);
				} else {
					printf("[!] Your mage not enough!\n");
				}
			}
		}
		if (warrior.health <= 0) {
			printf("[FINAL STATUS]\n\tYour helath: %u\n\tThe Guardian: %d\n", warrior.health, guardian.health);
			printf("You lose\n");
			break;
		}
		if (guardian.health <= 0) {
			printf("[FINAL STATUS]\n\tYour helath: %u\n\tThe Guardian: %d\n", warrior.health, guardian.health);
			printf("You win!\n");
			winner_handler();
		}
	}

}

void setup() {
    setvbuf(stdin, NULL, _IONBF, 0);
    setvbuf(stdout, NULL, _IONBF, 0);
    setvbuf(stderr, NULL, _IONBF, 0);
}

int main() {
	setup();
	printf("\n============================================================================\n");
	printf("            Welcome! Defeat the Guardian to gain your points.\n");
	printf("============================================================================\n\n");
	printf(">>press enter to start>>");
	getchar();
	hunt();
	return 0;
}

