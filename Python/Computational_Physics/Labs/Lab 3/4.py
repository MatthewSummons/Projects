# Test Checking Bot
# Created by Shaheer Ziya

# Solution to problem is much easier with inbuilt python strings

def main():
  correctAns = input("Enter the correct answers to the MC questions: ")

  # Iterate over 5 students
  for student in range(1, 5+1):
    # Obtain student answers
    stdAns = input(f"Enter the answers of Student-{student}: ")
    correctQuestions = []
    # Iterate over every answer and compare with solutions
    for sol, stdAns, Q in zip(correctAns, stdAns, range(1, 20+1)):
      # If the answer is correct, append the question number to the list
      if stdAns == sol:
        correctQuestions.append(Q)
    # For each student, print how many questions they got correct & the list of correct questions
    print(f"Number of correct answers: {len(correctQuestions)}")
    print("Answers to the following questions are correct:")
    if len(correctQuestions) == 20: print("ALL")
    else:
      for i in correctQuestions:
        print(i, end=" ")
      print()
main()