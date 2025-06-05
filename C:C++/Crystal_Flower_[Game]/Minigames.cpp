// MAZE MINI GAME 
#include "Minigames.hpp"
#include "Story.hpp"    // For print()
#include <string>
#include <iostream>

using namespace std;

const char width = 20, height = 9;
char player = 'P';                  // player character
int posx = 1, posy = 1;
char action;

void playerMove1 ();
void playerMove2 ();
void playerMove3 ();

struct enemies
{
    char symbol;
    bool active;
    int x;
    int y;
};

char maze1[height][width] = {

    {'#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'}, //0
    {'#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', '#', '#', ' ', '#'}, //1
    {'#', ' ', '#', ' ', '#', '#', '#', ' ', ' ', ' ', ' ', '#', '#', ' ', '#', ' ', ' ', ' ', ' ', '#'}, //2
    {'#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', '#', ' ', ' ', ' ', '#', ' ', '#', '#', '#'}, //3
    {'#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ', '#', ' ', ' ', '#', ' ', '#', ' ', ' ', ' ', '#'}, //4
    {'#', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', ' ', '#', '#', ' ', ' ', ' ', '#', ' ', '#'}, //5
    {'#', ' ', ' ', '#', ' ', ' ', ' ', ' ', '#', ' ', '#', ' ', ' ', ' ', '#', ' ', '#', '#', ' ', '#'}, //6
    {'#', ' ', '#', '#', '#', ' ', '#', ' ', ' ', ' ', '#', '#', '#', ' ', ' ', ' ', ' ', '#', ' ', '#'}, //7
    {'#', ' ', ' ', ' ', ' ', ' ', '#', ' ', '#', ' ', '#', ' ', ' ', ' ', '#', '#', ' ', ' ', ' ', '#'}, //8
    //0     1     2    3    4    5    6    7    8    9    0    1    2    3    4    5    6    7    8    9
};

char maze2[height][width] = {

    {'#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'}, //0
    {'#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', '#', '#', ' ', '#'}, //1
    {'#', ' ', '#', '#', '#', '#', '#', ' ', ' ', ' ', ' ', '#', '#', ' ', '#', ' ', ' ', ' ', ' ', '#'}, //2
    {'#', ' ', '#', ' ', ' ', ' ', '#', ' ', ' ', '#', '#', ' ', ' ', ' ', ' ', '#', ' ', ' ', ' ', '#'}, //3
    {'#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', '#', ' ', ' ', '#', ' ', '#', ' ', ' ', ' ', '#'}, //4
    {'#', ' ', ' ', '#', ' ', '#', '#', '#', ' ', ' ', ' ', ' ', '#', '#', ' ', ' ', ' ', '#', ' ', '#'}, //5
    {'#', '#', ' ', '#', ' ', ' ', ' ', ' ', '#', ' ', '#', ' ', ' ', ' ', '#', ' ', '#', '#', ' ', '#'}, //6
    {'#', '#', ' ', ' ', '#', ' ', '#', ' ', ' ', ' ', '#', '#', '#', ' ', ' ', ' ', ' ', '#', ' ', '#'}, //7
    {'#', '#', '#', ' ', ' ', ' ', '#', ' ', '#', ' ', '#', '#', ' ', ' ', '#', '#', ' ', ' ', ' ', '#'}, //8
    //0     1     2    3    4    5    6    7    8    9    0    1    2    3    4    5    6    7    8    9
};

char maze3[height][width] = {

    {'#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'}, //0
    {'#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ', ' ', '#'}, //1
    {'#', ' ', '#', '#', ' ', '#', ' ', '#', '#', ' ', ' ', '#', '#', ' ', '#', ' ', ' ', '#', ' ', '#'}, //2
    {'#', ' ', ' ', ' ', ' ', '#', ' ', '#', ' ', ' ', '#', '#', ' ', ' ', '#', '#', '#', '#', ' ', '#'}, //3
    {'#', ' ', ' ', ' ', '#', '#', ' ', '#', ' ', '#', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', '#'}, //4
    {'#', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', ' ', '#', '#'}, //5
    {'#', '#', ' ', ' ', ' ', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ', '#', ' ', '#', '#'}, //6
    {'#', ' ', '#', ' ', '#', ' ', '#', '#', '#', ' ', ' ', '#', '#', ' ', ' ', ' ', ' ', ' ', ' ', '#'}, //7
    {'#', ' ', ' ', ' ', ' ', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', '#', ' ', ' ', ' ', '#'}, //8
    //0     1     2    3    4    5    6    7    8    9    0    1    2    3    4    5    6    7    8    9
};

// MINI GAME 1
void minigame_1() {   
    while (action != 'Q' || 'q') {  
        if (posx == 9 || posy == 9){
            cout << "\nYou have escaped the maze!" << endl;
            return;
        }
        cout << endl;
        cout << "Press WASD to move... " << endl;
        cout << endl;

    maze1[posx][posy] = player;

    for (int y0 = 0; y0 < height; y0++) {             // runs 2d array
        cout << endl;
        for (int x0 = 0; x0 < height; x0++) {
            cout << maze1[y0][x0];
        }
        
    //1st enemy 
    enemies enemy1;
    enemy1.symbol = 'L';
    enemy1.active = true;
    enemy1.x = 3;
    enemy1.y = 6;
    maze1[enemy1.y][enemy1.x] = enemy1.symbol;
    //2nd enemy
    enemies enemy2;
    enemy2.symbol = 'I';
    enemy2.active = true;
    enemy2.x = 8;
    enemy2.y = 1;
    maze1[enemy2.y][enemy2.x] = enemy2.symbol;

    //3rd enemy
    enemies enemy3;
    enemy3.symbol = 'E';
    enemy3.active = true;
    enemy3.x = 6;
    enemy3.y = 6;
    maze1[enemy3.y][enemy3.x] = enemy3.symbol;
    
    }
    playerMove1();
    }

    return;
}

void playerMove1() {
    cout << "\nYour move: ";
    cin >> action;

    int prevposx = posx;
    int prevposy = posy;
    char space = ' ';             

    switch (action) {

    case 'a':
        if (maze1 [posx][posy-1]!='#' && maze1 [posx][posy-1]!= 'L' && maze1 [posx][posy-1]!= 'I' && maze1 [posx][posy-1]!= 'E') {      // future position is not hash
            posy--;

        maze1[prevposx][prevposy] = space;}
    system("clear");
    break;

    case 'A':
        if (maze1 [posx][posy-1]!='#' && maze1 [posx][posy-1]!= 'L' && maze1 [posx][posy-1]!= 'I' && maze1 [posx][posy-1]!= 'E') {      // future position is not hash
            posy--;

        maze1[prevposx][prevposy] = space;}
    system("clear");
    break;

    case 'd':
        if (maze1 [posx][posy + 1] != '#' && maze1 [posx][posy + 1] != 'L' && maze1 [posx][posy + 1] != 'I' && maze1 [posx][posy + 1] != 'E') {
            posy++;

        maze1[prevposx][prevposy] = space;}
    system("clear");
    break;

    case 'D':
        if (maze1 [posx][posy + 1] != '#' && maze1 [posx][posy + 1] != 'L' && maze1 [posx][posy + 1] != 'I' && maze1 [posx][posy + 1] != 'E') {
            posy++;

        maze1[prevposx][prevposy] = space;}
    system("clear");
    break;

    case 's':
        if (maze1 [posx + 1][posy] != '#' && maze1 [posx + 1][posy] != 'L' && maze1 [posx + 1][posy] != 'I' && maze1 [posx + 1][posy] != 'E') {
            posx++;

        maze1[prevposx][prevposy] = space;}
    system("clear");                               // system clear screen to redraw maze
    break;
    
    case 'S':
        if (maze1 [posx + 1][posy] != '#' && maze1 [posx + 1][posy] != 'L' && maze1 [posx + 1][posy] != 'I' && maze1 [posx + 1][posy] != 'E') {
            posx++;

        maze1[prevposx][prevposy] = space;}
    system("clear");                               
    break;

    case 'w':
        if (maze1 [posx - 1][posy] != '#' && maze1 [posx - 1][posy] != 'L' && maze1 [posx - 1][posy] != 'I' && maze1 [posx - 1][posy] != 'E') {
            posx--;

        maze1[prevposx][prevposy] = space;}
    system("clear");
    break;
    
    case 'W':
        if (maze1 [posx - 1][posy] != '#' && maze1 [posx - 1][posy] != 'L' && maze1 [posx - 1][posy] != 'I' && maze1 [posx - 1][posy] != 'E') {
            posx--;

        maze1[prevposx][prevposy] = space;}
    system("clear");
    break;

    default :
    cout << "Incorrect action... try a valid key" << endl;
    break;
    
    }
}

// MINI GAME 2
void minigame_2() {

    while (action != 'Q' || 'q') {  
      if (posx == 9 || posy == 9) {
            cout << "\nYou have escaped the maze!" << endl;
            return;
        }
      cout << endl;
        cout << "Press WASD to move... " << endl;
        cout << endl;

    maze2[posx][posy] = player;

    for (int y0 = 0; y0 < height; y0++) {             // runs 2d array
        cout << endl;
        for (int x0 = 0; x0 < height; x0++) {
            cout << maze2[y0][x0];
        }

    //1st enemy 
    enemies enemy1;
    enemy1.symbol = 'E';
    enemy1.active = true;
    enemy1.x = 3;
    enemy1.y = 6;
    maze2[enemy1.y][enemy1.x] = enemy1.symbol;
    //2nd enemy
    enemies enemy2;
    enemy2.symbol = 'V';
    enemy2.active = true;
    enemy2.x = 8;
    enemy2.y = 4;
    maze2[enemy2.y][enemy2.x] = enemy2.symbol;

    //3rd enemy
    enemies enemy3;
    enemy3.symbol = 'I';
    enemy3.active = true;
    enemy3.x = 6;
    enemy3.y = 6;
    maze2[enemy3.y][enemy3.x] = enemy3.symbol;
    
    //4th enemy
    enemies enemy4;
    enemy4.symbol = 'L';
    enemy4.active = true;
    enemy4.x = 2;
    enemy4.y = 3;
    maze2[enemy4.y][enemy4.x] = enemy4.symbol;
    
    }
    playerMove2();
    }

    return;
}

void playerMove2() {
    cout << "\nYour move: ";
    cin >> action;

    int prevposx = posx;
    int prevposy = posy;
    char space = ' ';                 

    switch (action) {
    case 'a':
        if (maze2 [posx][posy-1]!='#' && maze2 [posx][posy-1]!= 'L' && maze2 [posx][posy-1]!= 'I' && maze2 [posx][posy-1]!= 'E' && maze2 [posx][posy-1]!= 'V') {      // future position is not hash
            posy--;

        maze2[prevposx][prevposy] = space;}
    system("clear");
    break;

    case 'A':
        if (maze2 [posx][posy-1]!='#' && maze2 [posx][posy-1]!= 'L' && maze2 [posx][posy-1]!= 'I' && maze2 [posx][posy-1]!= 'E' && maze2 [posx][posy-1]!= 'V') {      // future position is not hash
            posy--;

        maze2[prevposx][prevposy] = space;}
    system("clear");
    break;

    case 'd':
        if (maze2 [posx][posy + 1] != '#' && maze2 [posx][posy + 1] != 'L' && maze2 [posx][posy + 1] != 'I' && maze2 [posx][posy + 1] != 'E' && maze2 [posx][posy + 1] != 'V') {
            posy++;

        maze2[prevposx][prevposy] = space;}
    system("clear");
    break;

    case 'D':
        if (maze2 [posx][posy + 1] != '#' && maze2 [posx][posy + 1] != 'L' && maze2 [posx][posy + 1] != 'I' && maze2 [posx][posy + 1] != 'E' && maze2 [posx][posy + 1] != 'V') {
            posy++;

        maze2[prevposx][prevposy] = space;}
    system("clear");
    break;

    case 's':
        if (maze2 [posx + 1][posy] != '#' && maze2 [posx + 1][posy] != 'L' && maze2 [posx + 1][posy] != 'I' && maze2 [posx + 1][posy] != 'E' && maze2 [posx + 1][posy] != 'V') {
            posx++;

        maze2[prevposx][prevposy] = space;}
    system("clear");                               // system clear screen to redraw maze
    break;
    
    case 'S':
        if (maze2 [posx + 1][posy] != '#' && maze2 [posx + 1][posy] != 'L' && maze2 [posx + 1][posy] != 'I' && maze2 [posx + 1][posy] != 'E' && maze2 [posx + 1][posy] != 'V') {
            posx++;

        maze2[prevposx][prevposy] = space;}
    system("clear");                               
    break;

    case 'w':
        if (maze2 [posx - 1][posy] != '#' && maze2 [posx - 1][posy] != 'L' && maze2 [posx - 1][posy] != 'I' && maze2 [posx - 1][posy] != 'E' && maze2 [posx - 1][posy] != 'V') {
            posx--;

        maze2[prevposx][prevposy] = space;}
    system("clear");
    break;
    
    case 'W':
        if (maze2 [posx - 1][posy] != '#' && maze2 [posx - 1][posy] != 'L' && maze2 [posx - 1][posy] != 'I' && maze2 [posx - 1][posy] != 'E' && maze2 [posx - 1][posy] != 'V') {
            posx--;

        maze2[prevposx][prevposy] = space;}
    system("clear");
    break;

    default :
    cout << "Incorrect action... try a valid key" << endl;
    break;
    }
}

// MINI GAME 3
void minigame_3() {

    while (action != 'Q' || 'q') {  
        if (posx == 9 || posy == 9) {
            cout << "\nYou have escaped the maze!" << endl;
            return;
        }
        cout << endl;
        cout << "Press WASD to move... " << endl;
        cout << endl;

    maze3[posx][posy] = player;

    for (int y0 = 0; y0 < height; y0++) {             // runs 2d array
        cout << endl;
        for (int x0 = 0; x0 < height; x0++) {
            cout << maze3[y0][x0];
        }

    //1st enemy 
    enemies enemy1;
    enemy1.symbol = 'I';
    enemy1.active = true;
    enemy1.x = 3;
    enemy1.y = 6;
    maze3[enemy1.y][enemy1.x] = enemy1.symbol;
    //2nd enemy
    enemies enemy2;
    enemy2.symbol = 'I';
    enemy2.active = true;
    enemy2.x = 5;
    enemy2.y = 4;
    maze3[enemy2.y][enemy2.x] = enemy2.symbol;

    //3rd enemy
    enemies enemy3;
    enemy3.symbol = 'S';
    enemy3.active = true;
    enemy3.x = 6;
    enemy3.y = 6;
    maze3[enemy3.y][enemy3.x] = enemy3.symbol;
    
    //4th enemy
    enemies enemy4;
    enemy4.symbol = 'R';
    enemy4.active = true;
    enemy4.x = 2;
    enemy4.y = 3;
    maze3[enemy4.y][enemy4.x] = enemy4.symbol;
    
    //5th enemy
    enemies enemy5;
    enemy5.symbol = 'T';
    enemy5.active = true;
    enemy5.x = 6;
    enemy5.y = 3;
    maze3[enemy5.y][enemy5.x] = enemy5.symbol;
    
    }
    playerMove3();
    }

    return;
}

void playerMove3() {
    cout << "\nYour move: ";
    cin >> action;

    int prevposx = posx;
    int prevposy = posy;
    char space = ' ';                  // ASCII for space

    switch (action) {
    case 'a':
        if (maze3 [posx][posy - 1] != '#' && maze3 [posx][posy - 1] != 'S' && maze3 [posx][posy - 1] != 'P' && maze3 [posx][posy - 1] != 'I' && maze3 [posx][posy - 1] != 'R' && maze3 [posx][posy - 1] != 'T') {      // future position is not hash
            posy--;

        maze3[prevposx][prevposy] = space;}
    system("clear");
    break;

    case 'A':
        if (maze3 [posx][posy - 1] != '#' && maze3 [posx][posy - 1] != 'S' && maze3 [posx][posy - 1] != 'P' && maze3 [posx][posy - 1] != 'I' && maze3 [posx][posy - 1] != 'R' && maze3 [posx][posy - 1] != 'T') {      // future position is not hash
            posy--;

        maze3[prevposx][prevposy] = space;}
    system("clear");
    break;

    case 'd':
        if (maze3 [posx][posy + 1] != '#' && maze3 [posx][posy + 1] != 'S' && maze3 [posx][posy + 1] != 'P' && maze3 [posx][posy + 1] != 'I' && maze3 [posx][posy + 1] != 'R' && maze3 [posx][posy + 1] != 'T') {
            posy++;

        maze3[prevposx][prevposy] = space;}
    system("clear");
    break;

    case 'D':
        if (maze3 [posx][posy + 1] != '#' && maze3 [posx][posy + 1] != 'S' && maze3 [posx][posy + 1] != 'P' && maze3 [posx][posy + 1] != 'I' && maze3 [posx][posy + 1] != 'R' && maze3 [posx][posy + 1] != 'T') {
            posy++;

        maze3[prevposx][prevposy] = space;}
    system("clear");
    break;

    case 's':
        if (maze3 [posx + 1][posy] != '#' && maze3 [posx + 1][posy] != 'S' && maze3 [posx + 1][posy] != 'P' && maze3 [posx + 1][posy] != 'I' && maze3 [posx + 1][posy] != 'R' && maze3 [posx + 1][posy] != 'T') {
            posx++;

        maze3[prevposx][prevposy] = space;}
    system("clear");                               // system clear screen to redraw maze
    break;

    case 'S':
        if (maze3 [posx + 1][posy] != '#' && maze3 [posx + 1][posy] != 'S' && maze3 [posx + 1][posy] != 'P' && maze3 [posx + 1][posy] != 'I' && maze3 [posx + 1][posy] != 'R' && maze3 [posx + 1][posy] != 'T') {
            posx++;

        maze3[prevposx][prevposy] = space;}
    system("clear");                               // system clear screen to redraw maze
    break;

    case 'w':
        if (maze3 [posx - 1][posy] != '#' && maze3 [posx - 1][posy] != 'S' && maze3 [posx - 1][posy] != 'P' && maze3 [posx - 1][posy] != 'I' && maze3 [posx - 1][posy] != 'R' && maze3 [posx - 1][posy] != 'T') {
            posx--;

        maze3[prevposx][prevposy] = space;}
    system("clear");
    break;

    case 'W':
        if (maze3 [posx - 1][posy] != '#' && maze3 [posx - 1][posy] != 'S' && maze3 [posx - 1][posy] != 'P' && maze3 [posx - 1][posy] != 'I' && maze3 [posx - 1][posy] != 'R' && maze3 [posx - 1][posy] != 'T') {
            posx--;

        maze3[prevposx][prevposy] = space;}
    system("clear");
    break;

    default :
    cout << "Incorrect action... try a valid key" << endl;
    break;
    }
}
