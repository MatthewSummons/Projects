#ifndef STORY_HPP
#define STORY_HPP

// For printing stuff character by chracter
void print(std::string line, int delay = 35, bool isEndLine = true);

// Call the apprpriate stage function
void Play_Stage(std::string &stage, std::string *inventory, int *seed);

// Ask to save after chapter concluded
void Ask_Save(const std::string stage, std::string *inventory, int *seed);
// Update Stage after completing a stage
void Update_Stage(std::string &stage, int *seed);

// Add/Remove items from inventory
void add_item(std::string item, std::string *inventory);
void remove_item(std::string item, std::string *inventory);

// Print the story for the respective stage
void Introduction(std::string *inventory);

void Monster_1(std::string *inventory);
void Monster_2(std::string *inventory);
void Monster_3(std::string *inventory);
void Monster_4(std::string *inventory);

void Final_Battle(std::string *inventory);

#endif