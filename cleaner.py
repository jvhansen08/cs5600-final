with open("new_data.csv", "r") as f:
    with open("final_data.csv", "w") as f2:
        counter = 0
        for line in f.readlines():
                if counter % 2 == 0:
                    f2.write(",".join(line) + "\n")
                counter += 1
