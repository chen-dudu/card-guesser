import subprocess
from random import sample
import time
from numpy import mean

RANKS = "23456789TJQKA"
SUITS = "CDHS"
DEBUG = True
RUNTIME_THRESHOLD = 10

def generateAllSelections(n, deck):
    answers = []
    deck_len = len(deck)

    if n == 2:
        for i in range(0, deck_len):
            for j in range(i+1, deck_len):
                answers.append([deck[i], deck[j]])
    elif n == 3:
        for i in range(0, deck_len):
            for j in range(i+1, deck_len):
                for k in range(j+1, deck_len):
                    answers.append([deck[i], deck[j], deck[k]])
    elif n == 4:
        for i in range(0, deck_len):
            for j in range(i+1, deck_len):
                for k in range(j+1, deck_len):
                    for m in range(k+1, deck_len):
                        answers.append([deck[i], deck[j], deck[k], deck[m]])

    return answers

if __name__ == '__main__':
    deck = []

    for i in SUITS:
        for j in RANKS:
            deck.append(j+i)

    open2 = False
    open3 = False
    open4 = True

    # # n = 2
    if open2:
        num_trial = 100
        average_guess = []
        timeout_count = 0
        for i in range(num_trial):
            print("### trial=" + str(i) + "########################################")
            answers2Cards = sample(generateAllSelections(2, deck), 10)
            count = 0
            passed = 0
            not_passed = 0
            total_guesses = 0
            time_out = 0
            max_time = 0
            min_time = float('Inf')
            total_time = 0
            for test in answers2Cards:
                start = time.time()
                # print("start testing", str(test))
                res = subprocess.run(["./Proj1Test", test[0], test[1]], stdout=subprocess.PIPE) 
                end = time.time()
                
                epsilon = round(end - start, 4)
                total_time += epsilon
                if epsilon > RUNTIME_THRESHOLD:
                    time_out += 1
                if epsilon > max_time:
                    max_time = epsilon
                if epsilon < min_time:
                    min_time = epsilon

                stdout_str = str(res.stdout, 'utf-8').replace("\r", "").split("\n")

                # print(stdout_str[-3].split(" ")[-2])
                num_guess = int(stdout_str[-3].split(" ")[-2])
                # num_guess = int(stdout_str[0])
                # quality = stdout_str[1]
                quality = stdout_str[-2].split(" ")[-1]

                if quality != "100.0%":
                    if DEBUG:
                        print(test, ":", quality, "with", num_guess, "guesses", str(epsilon)+"(s)")
                    not_passed += 1
                else:
                    passed += 1
                
                count += 1
                total_guesses += num_guess
            average_guess.append(total_guesses / count)
            timeout_count += time_out
            print("*** n=2 ****************************************")
            print("{}/{} {:.2f}% passed.".format(passed, count, passed/count * 100))
            print("{}/{} {:.2f}% timeout.".format(time_out, count, time_out/count * 100))
            print("min: {}(s) max: {}(s) avg: {}(s)".format(min_time, max_time, round(total_time/count, 4)))
            print("average {:.4f} guesses per test.".format(total_guesses / count))
            print("************************************************")
            print("")
        print("The average guess for the", num_trial, "trial is", mean(average_guess))
        print("timeout happens", timeout_count, "times")

    # # n = 3
    if open3:
        num_trial = 25
        average_guess = []
        timeout_count = 0
        for i in range(num_trial):
            print("### trial=" + str(i) + "########################################")
            # answers3Cards = generateAllSelections(3, deck)
            answers3Cards = sample(generateAllSelections(3, deck), 10)
            count = 0
            passed = 0
            not_passed = 0
            total_guesses = 0
            time_out = 0
            max_time = 0
            min_time = float('Inf')
            total_time = 0
            for test in answers3Cards:
                start = time.time()
                # print("start testing", str(test))
                res = subprocess.run(["./Proj1Test", test[0], test[1], test[2]], stdout=subprocess.PIPE) 
                end = time.time()
                
                epsilon = round(end - start, 4)
                total_time += epsilon
                if epsilon > RUNTIME_THRESHOLD:
                    time_out += 1
                if epsilon > max_time:
                    max_time = epsilon
                if epsilon < min_time:
                    min_time = epsilon

                stdout_str = str(res.stdout, 'utf-8').replace("\r", "").split("\n")

                # print(stdout_str)

                num_guess = int(stdout_str[-3].split(" ")[-2])
                # num_guess = int(stdout_str[0])
                quality = stdout_str[-2].split(" ")[-1]
                # quality = stdout_str[1]

                if quality != "100.0%":
                    if DEBUG:
                        print(test, ":", quality, "with", num_guess, "guesses", str(epsilon)+"(s)")
                    not_passed += 1
                else:
                    passed += 1
                
                count += 1
                total_guesses += num_guess
            average_guess.append(total_guesses / count) 
            timeout_count += time_out
            print("*** n=3 ****************************************")
            print("{}/{} {:.2f}% passed.".format(passed, count, passed/count * 100))
            print("{}/{} {:.2f}% timeout.".format(time_out, count, time_out/count * 100))
            print("min: {}(s) max: {}(s) avg: {}(s)".format(min_time, max_time, round(total_time/count, 4)))
            print("average {:.4f} guesses per test.".format(total_guesses / count))
            print("************************************************")
            print("")
        print("The average guess for the", num_trial, "trial is", mean(average_guess))
        print("timeout happens", timeout_count, "times")

    # n = 4
    if open4:
        average_guess = []
        num_trial = 10
        timeout_count = 0
        for i in range(num_trial):
            print("### trial=" + str(i) + "########################################")
            answers4Cards = sample(generateAllSelections(4, deck), 10)
            count = 0
            passed = 0
            not_passed = 0
            total_guesses = 0
            time_out = 0
            max_time = 0
            min_time = float('Inf')
            total_time = 0
            for test in answers4Cards:
                start = time.time()
                # print("start testing", str(test))
                res = subprocess.run(["./Proj1Test", test[0], test[1], test[2], test[3]], stdout=subprocess.PIPE) 
                end = time.time()
                
                epsilon = round(end - start, 4)
                total_time += epsilon
                if epsilon > RUNTIME_THRESHOLD:
                    time_out += 1
                if epsilon > max_time:
                    max_time = epsilon
                if epsilon < min_time:
                    min_time = epsilon

                stdout_str = str(res.stdout, 'utf-8').replace("\r", "").split("\n")

                # print(stdout_str)

                num_guess = int(stdout_str[-3].split(" ")[-2])
                # num_guess = int(stdout_str[0])
                quality = stdout_str[-2].split(" ")[-1]
                # quality = stdout_str[1]

                if quality != "100.0%":
                    if DEBUG:
                        print(test, ":", quality, "with", num_guess, "guesses", str(epsilon)+"(s)")
                    not_passed += 1
                else:
                    passed += 1
                
                count += 1
                total_guesses += num_guess
            average_guess.append(total_guesses / count)
            timeout_count += time_out
            print("*** n=4 ****************************************")
            print("{}/{} {:.2f}% passed.".format(passed, count, passed/count * 100))
            print("{}/{} {:.2f}% timeout.".format(time_out, count, time_out/count * 100))
            print("min: {}(s) max: {}(s) avg: {}(s)".format(min_time, max_time, round(total_time/count, 4)))
            print("average {:.4f} guesses per test.".format(total_guesses / count))
            print("************************************************")
            print("")
        print("The average guess for the", num_trial, "trial is", mean(average_guess))
        print("timeout happens", timeout_count, "times")
