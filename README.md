# Texas Hold'em Poker

## Objective

The main goal of this project is to compute the empirical probabilities of Texas Hold'em Hand Ranks through a simulation.


## Justification

Over a semester in college, Professor James Sampaio lectioned an unconventional type of course. In the initial class, he selected a few groups and assigned specific subjects for each, with our group main focus being on "Probability Games". 

On later appointments with Professor Sampaio, he suggested multiple games that we could base our study on, so by the end of the semester we could present it to the rest of the class. The one that held the biggest challenge was to develop a R code that could be used to calculate empirical probabilities of Texas Hold'em Poker Hand Ranks.

Therefore, this was the part of the project that required the most effort, and the one that came out as the most impressive.

## Choices and Metodology

Since we are going to simulate probabilities for hand ranks in Poker, the first step is to create a R object that emulates a 52 Card Deck in R. The 52 Card Deck is a simple matrix that contains 52 rows (one for each card) and 2 columns, with the first one representing the card value and the second column regarding the card suit.
It starts at a "2 of Clubs" in the first row and goes sorted all the way to "A of Clubs" in the 13th row; then, the card values simply repeat itself for every suit, respectively Diamonds, Hearts and Spades.

With the Deck ready, the next step was to create a function that could draw a Poker round, with the user providing his hole cards as arguments. In this sense, the "deal\_game" (mao\_aposta) function was created, requiring a input of two cards from the user (X, Y). The values for X and Y must be a number between 1 and 52, respective to it's value in the Deck object.

The third part was an incredibly complex one: create the "qual\_mao" (which\_rank) function that could receive a poker round, and classify it with the highest possible hand rank. Since we had to account to every different possibility, it is a quite large function that contains many "if" clauses. For this same reason, we also had to carefully optimize the whole code, in order to achieve a reasonable performance.

Last but not least, we finally had to create a "simulate" function, which would use the Deck, "deal\_game" and "which\_rank" functions to generate a "N" amount of poker rounds, counting the frequency of each hand rank. With this in mind, we constructed "simulate" with the following arguments:

* N - Number of poker rounds to generate. By default, it runs 10.000 games, but can be set to any integer value;
* n - Number of cards to consider while hand ranking. The default 5 will only consider hole and Flop Cards, 6 will include the Turn and any other number will include all 7 cards;
* X, Y - User's Hole cards. Used to call the "deal\_game" function;
* jogada - Lowest acceptable hand rank. It is used to show only hand ranks which are equal or better than the given rank. By default, it considers every outcome, even a "High Card" one.

After running the "simulate" function, it will return a progress bar during the simulation, and finally return a matrix with the selected hand ranks, containing it's frequency for the desired stage(s).

In the end, the project was a success and the challenge was well worth it. All functions work perfectly, and no clear error was found during tests and the presentation. Happy simulations!


## License and Contact

This project was created by Matheus Erbisti, Ramon Moreira, Bruno Brandao and Rafael Araruna, with guidance from Professor James Sampaio. It falls under the MIT License, which means you are free to use and adapt this code at your will, just make sure to reference us!

If you need to make contact about this project, you can reach out to me on LinkedIn (https://www.linkedin.com/in/matheus-erbisti-b74168172/) or via e-mail at matheuserbisti@hotmail.com.


Thank you for your attention!
