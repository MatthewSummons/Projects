#ifndef SAVE_LOAD_HPP
#define SAVE_LOAD_HPP

#include <iostream>   // For printing error messages
#include <fstream>    // For reading & writing to save files

#include <string>     // For C++ Strings

#include <algorithm>  // std::shuffle
#include <vector>     // For STL vectors
#include <random>     // std::default_random_engine
#include <chrono>     // std::chrono::system_clock

// Abstract functions to communcate with the main function and/or otherwise encapsulate steps
void Start_Save_Load(const int game_mode, std::string &stage, std::string* inventory, int *seed);
void Save(const std::string stage, std::string *inventory, int *seed);

// Functions that read/write the save files directly
void update_save(struct Game_State game_status);
void load_save(struct Game_State &game_status);

// Generates the seed for the playthrough
void generate_seed(int *seed);

#endif