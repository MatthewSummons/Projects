from __future__ import print_function

from copy import copy
import json
import get_private_key

class Tester:
    def __init__(self):
        with open("./students_number.txt", "r") as f:
            self.student_ids = list()
            for line in f.readlines():
                self.student_ids.append(line.strip())
        
        with open("./student_pub_keys.json", "r") as f:
            self.pub_keys = json.load(f)

        with open("./student_priv_keys.json", "r") as f:
            self.priv_keys = json.load(f)

    def test_one_student(self):
        student_id = get_private_key.get_student_number()

        if student_id not in self.student_ids:
            print("Your student id is not valid!")
            return -1
        waldo = "nobody"
        tmp_N1 = int(self.pub_keys[student_id]["N"], 16)
        tmp_e = int(self.pub_keys[student_id]["e"], 16)
        for key in self.pub_keys.keys():
            if key == student_id:
                continue
            tmp_N2 = int(self.pub_keys[key]["N"], 16)
            if get_private_key.is_waldo(tmp_N1, tmp_N2):
                waldo = key
                break
        print("You think your waldo is: {}".format(waldo))
        if waldo != self.priv_keys[student_id]["waldo"]:
            print("Waldo is wrong!")
            print("Your waldo should be: {}".format(self.priv_keys[student_id]["waldo"]))
            return 0
        print("Your waldo is correct!")
        tmp_d = get_private_key.get_private_key_from_n1_n2_e(tmp_N1, tmp_N2, tmp_e)
        print("The private key is: {}".format(tmp_d))
        if tmp_d != int(self.priv_keys[student_id]["d"], 16):
            print("The private key is wrong!")
            print("The correct one should be: {}".format(self.priv_keys[student_id]["d"]))
            return 1
        print("The private key is correct!\n")
        return 2
    
    ### For testing data given to students
    def test_all_student(self):
        tot = 0
        for student_id in self.student_ids:
            waldo = "nobody"
            tmp_N1 = int(self.pub_keys[student_id]["N"], 16)
            tmp_e = int(self.pub_keys[student_id]["e"], 16)
            for key in self.pub_keys.keys():
                if key == student_id:
                    continue
                tmp_N2 = int(self.pub_keys[key]["N"], 16)
                if get_private_key.is_waldo(tmp_N1, tmp_N2):
                    waldo = key
                    break
            print("[{} - No. {}] You think your waldo is: {}".format(student_id, tot, waldo))
            if waldo != self.priv_keys[student_id]["waldo"]:
                print("[{} - No. {}] Waldo is wrong!".format(student_id, tot))
                print("[{} - No. {}] Your waldo should be: {}".format(student_id, tot, self.priv_keys[student_id]["waldo"]))
                return 0
            print("[{} - No. {}] Your waldo is correct!".format(student_id, tot))
            tmp_d = get_private_key.get_private_key_from_n1_n2_e(tmp_N1, tmp_N2, tmp_e)
            print("[{} - No. {}] The private key is: {}".format(student_id, tot, tmp_d))
            if tmp_d != int(self.priv_keys[student_id]["d"], 16):
                print("[{} - No. {}] The private key is wrong!".format(student_id, tot))
                print("[{} - No. {}] The correct one should be: {}".format(student_id, tot, self.priv_keys[student_id]["d"]))
                return 1
            print("[{} - No. {}] The private key is correct!\n".format(student_id, tot))
            tot += 1


if __name__ == "__main__":
    test = Tester() 
    test.test_one_student()

