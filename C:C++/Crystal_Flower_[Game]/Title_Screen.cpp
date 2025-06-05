#include <unistd.h>     // For implementing ...
#include <chrono>       // a .. delay ..
#include <thread>       // when printing stuff

#include <iostream>     // For cout, cin
#include <string>       // For C++ Strings
#include <fstream>      // For reading and writing to save files
#include <iomanip>      // For setw in centering in text

using namespace std;

#define CENTRE 110
#define TERMINAL_HEIGHT 15

#include "Title_Screen.hpp"

void Title_Screen(int &game_mode, bool &saved) {
  // Update the save status of the game
  saved = isSaved();
  
  print_title_screen(saved);
  
  // Update the game mode after user selects it
  game_mode = ask_game_mode(saved);
}

// Print a string character by character with a mixed-delay
void delayed_print(std::string str, bool isEndline=true) {
  int len = str.length();
  for (int i = 0; i < len; i++) {
    cout << str[i] << flush;
    // Add the delay, alternating it to make it look more natural
    if (i % 2 == 0)
      std::this_thread::sleep_for(std::chrono::milliseconds(35));
    else
      std::this_thread::sleep_for(std::chrono::milliseconds(50));
  }
  
  // Hold at the end of the string for emphasis
  std::this_thread::sleep_for(std::chrono::milliseconds(350));
  
  if (isEndline)
    cout << endl;
}

// Prints the game title, and game mode options for the user to select from. Detects if a save file exists
//  and prints the continue option only if it exists.
void print_title_screen(bool save_status) {
  
  system("clear");

  // Centre Text Vertically
  for (int i = 0; i < TERMINAL_HEIGHT; i++) {
    cout << endl;
  }
  
  // The Game Title with "50" spaces
  string Title = R"(                                                         
                                                                                                                                                                                 
                                                                             .--.            .       .    .--- .                      
                                                                            :               _|_      |    |    |                      
                                                                            |    .--..  ..--.|  .-.  |    |--- | .-. .  .    ._.-. .--.
                                                                            :    |   |  |`--.| (   ) |    |    |(   ) \  \  / (.-' |   
                                                                             `--''   `--|`--'`-'`-'`-`-   '    `-`-'   `' `'   `--''   
                                                                                        ;                                            
                                                                                      `-'                                             )";
  cout << Title << endl;

  // Game mode options below title
  string N = "1. New Game", Q = "2. Quit";

  // Check save from save file and print the continue option if save file exists
  bool isContinue = save_status;

  // Print the Game Mode options
  int offset = N.length();

  cout << setw(CENTRE - offset) << "";
  delayed_print(N);
  // cout << N << endl;

  if (isContinue) {
    string C = "2. Continue";
    cout << setw(CENTRE - offset) << "";
    delayed_print(C);

    // Update the Q string
    Q = "3. Quit";
  }

  cout << setw(CENTRE - offset) << "";
  delayed_print(Q);
  // cout << Q << endl;
  
}

// Opens the save file and checks if a save exists or not
// Returns true if "Save Exists", false if "No Save" & otherwise rewrites the save file
bool isSaved() {
  ifstream save_file;
  save_file.open(".Save/save.save");

  if (save_file.fail()) {
    delayed_print("Error loading from file");
    exit(1);
  }

  string save_status, No_Save = "No Save";
  getline(save_file, save_status);    // First line of Save file contins save status

  if (save_status == No_Save)
    return false;
  else if (save_status == "Save Exists")
    return true;
  // If corrupted save file then rewrite so the player can start anew
  else {
    cout << setw(CENTRE - 5);  // "Save file is corrupted!" has length 23
    delayed_print("Save file is corrupted!");
    save_file.close();  // Close previously opened file
    
    // Open new file to rewrite it
    ofstream save_file_E;
    save_file_E.open(".Save/save.save");
    save_file_E << No_Save << endl;
    save_file_E.close();
    exit(1);
  }
}

int ask_game_mode(bool isSaved) {
  int game_mode = -1;
  
  delayed_print("Please select an option from above: ", false);
  int user_option;
  cin >> user_option;
  
  // Quit will be 3
  if (isSaved) {
    if(user_option == 3)
      quit_game();
  }
  // Quit will be 2
  else if (user_option == 2) {
    quit_game();
  }

  // Easter Egg
  if (user_option == 69) {
    delayed_print("nice.");
    exit(0);
  }

  // If user wants to play a new game when a save already exists, confirm their choice
  if (isSaved && (user_option == 1)) {
    delayed_print("Are you sure you want to play a new game when a save already exists?");
    delayed_print("State the game mode you want to play");
    delayed_print("If you choose (1) New Game, your save file will be overwritten");
    
    cin >> user_option;

    if (user_option == 3) {
      quit_game();
    }

    // If user decides to overwire save file
    if (user_option == 1) {
      ofstream save_file;
      save_file.open(".Save/save.save");

      if (save_file.fail())
      {
        delayed_print("Error opening save file");
        exit(1);
      }

      string updated_save_status = "No Save";
      save_file << updated_save_status;
      save_file.close();
    }
  }

  return user_option;
} 

void quit_game() {
  delayed_print("Come back next time!");
  system("clear");
  exit(0);
}