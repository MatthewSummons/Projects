from __future__ import print_function

from copy import copy
import json
import small_exponent_short_message_attack

class Tester:
    def __init__(self):
        with open("./students_number.txt", "r") as f:
            self.student_ids = list()
            for line in f.readlines():
                self.student_ids.append(line.strip())
        
        with open("./pub_keys_ciphertexts.json", "r") as f:
            self.inputs = json.load(f)

        with open("./original_texts.json", "r") as f:
            self.outputs = json.load(f)

    def test_one_student(self):
        student_id = small_exponent_short_message_attack.get_student_number()

        if student_id not in self.student_ids:
            print("Your student id is not valid!")
            return -1
        tmp_N = int(self.inputs[student_id]["N"], 16)
        tmp_E = int(self.inputs[student_id]["E"], 16)
        tmp_C = int(self.inputs[student_id]["C"], 16)

        tmp_m = small_exponent_short_message_attack.recover_message(tmp_N, tmp_E, tmp_C)
        print("Your calculated message is: {}".format(hex(tmp_m)))
        if tmp_m != int(self.outputs[student_id]["m"], 16):
            print("The message is wrong!")
            print("The correct one should be: {}".format(self.outputs[student_id]["m"]))
            return 0
        print("The message is correct!\n")
        return 1
    
    ### For testing data given to students
    def test_all_student(self):
        tot = 0
        for student_id in self.student_ids:
            tmp_N = int(self.inputs[student_id]["N"], 16)
            tmp_E = int(self.inputs[student_id]["E"], 16)
            tmp_C = int(self.inputs[student_id]["C"], 16)

            tmp_m = small_exponent_short_message_attack.recover_message(tmp_N, tmp_E, tmp_C)
            print("[{} - No. {}] Your calculated message is: {}".format(student_id, tot, hex(tmp_m)))
            if tmp_m != int(self.outputs[student_id]["m"], 16):
                print("[{} - No. {}] The message is wrong!".format(student_id, tot))
                print("[{} - No. {}] The correct one should be: {}".format(student_id, tot, self.outputs[student_id]["m"]))
                return 1
            print("[{} - No. {}] The message is correct!\n".format(student_id, tot))
            tot += 1


if __name__ == "__main__":
    test = Tester() 
    test.test_one_student()

