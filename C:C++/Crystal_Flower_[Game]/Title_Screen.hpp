#ifndef TITLE_SCREEN_HPP
#define TITLE_SCREEN_HPP

// Handles the title screen ouput and input, decides the game mode as well
void Title_Screen(int &game_mode, bool &saved);

// Handle output to the screen, dependent on the save status
void delayed_print(std::string str, bool isEndline);
void print_title_screen(bool save_status);
bool isSaved();

// Update the game mode dependent on what the user inputs
int ask_game_mode(bool save_status);
void quit_game();

#endif