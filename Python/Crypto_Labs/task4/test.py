from __future__ import print_function

from copy import copy
import json
import broadcast_attack

class Tester:
    def __init__(self):
        with open("./students_number.txt", "r") as f:
            self.student_ids = list()
            for line in f.readlines():
                self.student_ids.append(line.strip())
        
        with open("./keys_and_ciphertexts.json", "r") as f:
            self.ks_cs = json.load(f)

        with open("./plaintexts.json", "r") as f:
            self.plaintexts = json.load(f)

    def test_one_student(self):
        student_id = broadcast_attack.get_student_number()

        if student_id not in self.student_ids:
            print("Your student id is not valid!")
            return -1
        tmp_n0 = int(self.ks_cs[student_id]["N0"], 16)
        tmp_n1 = int(self.ks_cs[student_id]["N1"], 16)
        tmp_n2 = int(self.ks_cs[student_id]["N2"], 16)
        tmp_c0 = int(self.ks_cs[student_id]["C0"], 16)
        tmp_c1 = int(self.ks_cs[student_id]["C1"], 16)
        tmp_c2 = int(self.ks_cs[student_id]["C2"], 16)

        tmp_m = broadcast_attack.recover_msg(tmp_n0, tmp_n1, tmp_n2, tmp_c0, tmp_c1, tmp_c2)
        print("Your m is: {}".format(tmp_m))
        if tmp_m != int(self.plaintexts[student_id]["m"], 16):
            print("The m is wrong!")
            print("The correct one should be: {}".format(self.plaintexts[student_id]["m"]))
            return 0
        print("The plain text is correct!")
        return 1

    ### For testing data given to students
    def test_all_student(self):
        tot = 0
        for student_id in self.student_ids:
            tmp_n0 = int(self.ks_cs[student_id]["N0"], 16)
            tmp_n1 = int(self.ks_cs[student_id]["N1"], 16)
            tmp_n2 = int(self.ks_cs[student_id]["N2"], 16)
            tmp_c0 = int(self.ks_cs[student_id]["C0"], 16)
            tmp_c1 = int(self.ks_cs[student_id]["C1"], 16)
            tmp_c2 = int(self.ks_cs[student_id]["C2"], 16)

            tmp_m = broadcast_attack.recover_msg(tmp_n0, tmp_n1, tmp_n2, tmp_c0, tmp_c1, tmp_c2)
            print("[{} - No. {}] Your m is: {}".format(student_id, tot, tmp_m))
            if tmp_m != int(self.plaintexts[student_id]["m"], 16):
                print("[{} - No. {}] The m is wrong!".format(student_id, tot))
                print("[{} - No. {}] The correct one should be: {}".format(student_id, tot, self.plaintexts[student_id]["m"]))
                return 0
            print("[{} - No. {}] The plain text is correct!".format(student_id, tot))
            tot += 1


if __name__ == "__main__":
    test = Tester() 
    test.test_one_student()

