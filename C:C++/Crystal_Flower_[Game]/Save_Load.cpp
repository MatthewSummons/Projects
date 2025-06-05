#include "Save_Load.hpp"

#define inventory_length 8

// Store the Staus of the Game
struct Game_State
{
  std::string stage;
  std::string *inventory = new std::string[inventory_length];
  unsigned order[4] = {};
};

std::string stages[] = {
    "Introduction",
    "Monster 1",
    "Monster 2",
    "Monster 3",
    "Monster 4",
    "Final Battle"};

// File responsible for handling the saving/loading from save files.
// Takes in the game_mode, stage & inventory and updates it accordingly
// Communicate data between save file & running program (or create new game data)
void Start_Save_Load(const int game_mode, std::string &stage, std::string *inventory, int *seed)
{

  Game_State Crystal_Flower;

  // Create a new save
  if (game_mode == 1) {
    Crystal_Flower.stage = "Introduction";

    for (unsigned i = 0; i < inventory_length; i++) {
      Crystal_Flower.inventory[i] = "Empty";
    }
    
    // TODO
    // Generate seed here!! @shaheer
    int seed [4] = {6, 9, 4, 2};
    
    generate_seed(seed);
    // Copy seed to struct
    for (unsigned i = 0; i < 4; i++) {
      Crystal_Flower.order[i] = seed[i];
    }
    
    update_save(Crystal_Flower);
  }

  // Load from the save file
  else {
    std::cout << "Loading ... !" << std::endl;
    load_save(Crystal_Flower);
  }

  // Update the stage, inventory & seed after loading from the save file
  stage = Crystal_Flower.stage;

  // Copy inventory items to send to main()
  for (unsigned i = 0; i < inventory_length; i++) {
    inventory[i] = Crystal_Flower.inventory[i];
  }

  // Copy over the seed for the playthrough
  for (unsigned i = 0; i < 4; i++) {
    seed[i] = Crystal_Flower.order[i];
  }

  // Free up memory
  delete [] Crystal_Flower.inventory;
}

// Encapsulate game status and update save file
// Given current stage, determine what the next stage will be and write that to the save file
void Save(const std::string next_stage, std::string *inventory, int *seed)
{
  Game_State game_status;

  game_status.stage = next_stage;
  game_status.inventory = inventory;

  // Copy the seed
  for (unsigned i = 0; i < 4; i++) {
    game_status.order[i] = seed[i];
  }

  update_save(game_status);
}

// Given a game state, write to the save file
// Performs the writing to the save file
void update_save(struct Game_State game_status)
{

  std::cout << "Saving file. Do no Quit." << std::endl;

  std::ofstream save_file;
  save_file.open(".Save/save.save");

  if (save_file.fail())
  {
    std::cout << "Error opening save file" << std::endl;
    exit(1);
  }

  save_file << "Save Exists" << std::endl;

  // Write the stage
  save_file << game_status.stage << std::endl;

  // Write the inventory items
  for (int i = 0; i < inventory_length; i++)
  {
    save_file << game_status.inventory[i] << std::endl;
  }

  // Write the seed for the play-through
  for (int i = 0; i < 4; i++) {
    save_file << game_status.order[i] << " ";
  }

  save_file << std::endl;


  save_file.close();

  std::cout << "File Saved" << std::endl;
}

// Read from a save file & store it in game_status
void load_save(struct Game_State &game_status)
{
  // Open the save file
  std::ifstream save_file;
  save_file.open(".Save/save.save");

  if (save_file.fail())
  {
    std::cout << "Error loading from save file" << std::endl;
  }

  // Read from the save file
  std::string save_state;
  std::getline(save_file, save_state);

  // Get stage
  std::getline(save_file, game_status.stage);

  // Get inventory items
  for (int i = 0; i < inventory_length; i++)
  {
    std::getline(save_file, game_status.inventory[i]);
  }

  // Get the seed for the playthrough
  for (unsigned i = 0; i < 4; i++) {
    save_file >> game_status.order[i];
  }

  save_file.close();
}

void generate_seed(int *play) {
  // The seed for the random shuffle
  unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
  
  // Shuffle the stages playable in the game
  std::vector<int> stages = {1, 2, 3, 4};
  shuffle(stages.begin(), stages.end(), std::default_random_engine(seed));

  // Modify the given play array to reflect the changes
  for (unsigned i = 0; i < 4; i++)
  {
    play[i] = stages[i];
  }
}