#include <iostream>
#include <string>
#include <iomanip>

#include "Story.hpp"    // For print()

#define CENTRE 110
#define TERMINAL_HEIGHT 15

using namespace std;

// Takes in a unique end_screen message and displays it below the Game Over Screen
void End_Screen (string end_line) {
  // 25 tabs
  string End_Title = R"(
                                                                            ▄████  ▄▄▄       ███▄ ▄███▓▓█████     ▒█████   ██▒   █▓▓█████  ██▀███  
                                                                            ██▒ ▀█▒▒████▄    ▓██▒▀█▀ ██▒▓█   ▀    ▒██▒  ██▒▓██░   █▒▓█   ▀ ▓██ ▒ ██▒
                                                                            ▒██░▄▄▄░▒██  ▀█▄  ▓██    ▓██░▒███      ▒██░  ██▒ ▓██  █▒░▒███   ▓██ ░▄█ ▒
                                                                            ░▓█  ██▓░██▄▄▄▄██ ▒██    ▒██ ▒▓█  ▄    ▒██   ██░  ▒██ █░░▒▓█  ▄ ▒██▀▀█▄  
                                                                            ░▒▓███▀▒ ▓█   ▓██▒▒██▒   ░██▒░▒████▒   ░ ████▓▒░   ▒▀█░  ░▒████▒░██▓ ▒██▒
                                                                            ░▒   ▒  ▒▒   ▓▒█░░ ▒░   ░  ░░░ ▒░ ░   ░ ▒░▒░▒░    ░ ▐░  ░░ ▒░ ░░ ▒▓ ░▒▓░
                                                                              ░   ░   ▒   ▒▒ ░░  ░      ░ ░ ░  ░     ░ ▒ ▒░    ░ ░░   ░ ░  ░  ░▒ ░ ▒░
                                                                            ░ ░   ░   ░   ▒   ░      ░      ░      ░ ░ ░ ▒       ░░     ░     ░░   ░ 
                                                                                  ░       ░  ░       ░      ░  ░       ░ ░        ░     ░  ░   ░     
                                                                                                                                ░                   
                          )";

  
  system("clear");

  // Centre Text Vertically
  for (int i = 0; i < TERMINAL_HEIGHT; i++) {
    cout << endl;
  }

  cout << End_Title << endl;

  // Print the end line centred under the End Screen
  int offset = end_line.length() / 2;
  cout << setw(CENTRE - offset) << "";
  print(end_line, 90);

  // End The Game for choosing the wrong choice
  exit(0);
}