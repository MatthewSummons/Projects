#include <iostream>

using namespace std;

#include "Title_Screen.hpp"
#include "Save_Load.hpp"
#include "Story.hpp"
#include "End_Screen.hpp"

#define inventory_length 8

int main() {
  
  int game_mode = 0;    // 1 :=  New Game, 2 := Continue
  bool saved = false;   // Store if a save file exists
  
  // Update game mode by determining if a save exists or not
  Title_Screen(game_mode, saved);

  // Update the stage and inventory by loading save and creating a new one in none exists
  string stage = "";
  string *inventory = new string[inventory_length];
  // Hold the seed for the game, undecided/empty
  int seed [4] = {0};
  
  // Initialize Inventory to be empty
  for (int i = 0; i < inventory_length; i++) {
    inventory[i] = "Empty";
  }
  
  // Load data from save if continuing previous game, otherwise create a new save
  Start_Save_Load(game_mode, stage, inventory, seed);

  // Play corresponding stage
  while (stage != "Final Battle") {
    Play_Stage(stage, inventory, seed);
  }
  
  // Play the Final Stage
  Play_Stage(stage, inventory, seed);
}