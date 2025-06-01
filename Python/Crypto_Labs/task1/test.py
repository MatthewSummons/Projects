from __future__ import print_function

from copy import copy
import json
import decrypt_message

class Tester:
    def __init__(self):
        with open("./students_number.txt", "r") as f:
            self.student_ids = list()
            for line in f.readlines():
                self.student_ids.append(line.strip())
        
        with open("./student_keys.json", "r") as f:
            self.student_keys = json.load(f)

        with open("./decrypted_msgs.json", "r") as f:
            self.decrypted_msgs = json.load(f)

    def test_one_student(self):
        student_id = decrypt_message.get_student_number()

        if student_id not in self.student_ids:
            print("Your student id is not valid!")
            return -1
        
        tmp_c = int(self.student_keys[student_id]["c"], 16)
        tmp_d = int(self.student_keys[student_id]["d"], 16)
        tmp_N = int(self.student_keys[student_id]["N"], 16)
        tmp_e = int(self.student_keys[student_id]["e"], 16)

        tmp_m = decrypt_message.decrypt_message(tmp_N, tmp_e, tmp_d, tmp_c)
        print("Your decrypted message is: ", tmp_m)
        if tmp_m != self.decrypted_msgs[student_id]["m"]:
            print("The decryption is wrong!\n")
            print("The correct one should be:", self.decrypted_msgs[student_id]["m"])
            return 0
        print("The decryption is correct!\n")
        return 1

if __name__ == "__main__":
    test = Tester() 
    test.test_one_student()

