# Assembly Chezz Game

## Table of Contents

* [Project Description](#Project-Description)
* [Required Software](#Required-Software)
* [Usage](#Usage)
* [Gameplay](#Gameplay)
* [References](#References)

<div align="center"><img src="/media/chezz.gif" alt="game logo" width="250px"></div>

# Project Description

Chezz is like any standard chess, each piece moves exactly like standard chess. The main difference is that there are no turns, which means one player can move multiple pieces while the other player didn't move any pieces. There is a 3 second countdown for each piece to be moved again. A bonus item will appear randomly on the board. Any player can pick up the item by moving a piece to the tile having the bonus. The item reduces the countdown timer from 3 seconds to 2 seconds for all the pieces of the player who picked up the item.

# Required Software

- DOSBox (included in Repo)
- masm.exe & link.exe (included in Repo)
- [Hamachi](https://vpn.net/)

# Usage

### Player 1

1. Open Hamachi and create a network.
2. Enter Network Credentials and store them for later.
3. Open "*DOSBox 0.74-3 Options.bat*".
4. Change the path highlighted at the end of the file to the path at which the project was downloaded.
5. Run DOSBox and press enter until the game starts. Wait for player 2 to run the game.

<div align="center"><img src="/media/player1.gif" alt="Player 1 Steps" width="500px"></div>

### Player 2 

1. Open Hamachi and join the network.
2. Enter credentials of the network created by player 1.
3. If the circle next to the network is colored anything but green, check this [link](https://community.logmein.com/t5/Hamachi-Discussions/bd-p/Hamachi) for help in troubleshooting the issue.
4. Copy Player 1's ipv4 address.
5. Open "*DOSBox 0.74-3 Options.bat*".
6. Replace the text hightlighted with the ipv4 address copied previously.
7. Change the path highlighted at the end of the file to the path at which the project was downloaded.
8. Run DOSBox and press enter until the game starts.

<div align="center"><img src="/media/chezz.gif" alt="game logo" width="250px"></div>

# Gameplay

### Game 1

<div align="center"><img src="/media/gameplay1.gif" alt="Gameplay 1" width="700px"></div>

### Game 2

<div align="center"><img src="/media/gameplay2.gif" alt="Gameplay 2" width="700px"></div>

# References

Project Document: [MP_Project_DescriptionA_Fall_2022.pdf](https://github.com/MahmoudSamy1452/Assembly-chess-game/files/10551974/MP_Project_DescriptionA_Fall_2022.pdf)
