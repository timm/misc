import random

hands = ["""
    _______
---'   ____)
      (_____)
      (_____)
      (____)
---.__(___)
""",
"""
     _______
---'    ____)____
           ______)
          _______)
         _______)
---.__________)
""",
"""
    _______
---'   ____)____
          ______)
       __________)
      (____)
---.__(___)
"""]

names = ["rock", "paper", "scissors"]
wins = {0: 2, 1: 0, 2: 1}  # key beats value

while True:
    guess = input("rock(0) paper(1) scissors(2) q=quit: ").strip()
    if guess in ("q", "quit"):
        break
    if guess not in ("0", "1", "2"):
        print("bad input. 0,1,2 only.\n")
        continue
    you = int(guess)
    cpu = random.randint(0, 2)

    print("\nYou:")
    print(hands[you])
    print("CPU:")
    print(hands[cpu])

    if you == cpu:
        print("Tie.\n")
    elif wins[you] == cpu:
        print("You win!\n")
    else:
        print("You lose.\n")
