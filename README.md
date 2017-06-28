# Ariadne

> Ariadne (/æriˈædniː/; Greek: Ἀριάδνη; Latin: Ariadne), in Greek mythology, was the daughter of Minos, King of Crete, Son of Zeus and his queen Pasiphaë, daughter of Helios. She is mostly associated with mazes and labyrinths because of her involvement in the myths of the Minotaur and Theseus. Her father put her in charge of the labyrinth where sacrifices were made as part of reparations (either to Poseidon or to Athena, depending on the version of the myth); later, she helped Theseus overcome the Minotaur and save the potential sacrificial victims. 

This repo hosts an in-progress experiment that seeks to construct a useful *exocortex*, a system that augments the mind with non-human capabilities of memory formation and information retrieval, using Haskell, PostgreSQL, and more than a little inspiration from `org-mode`.

It also hosts the Edible and Amalgam projects, which will possibly be split into their own repos later.

## Edible

Edible is a database interface for Haskell forked from [tisch](https://github.com/k0001/tisch), [Opaleye](https://github.com/tomjaguarpaw/haskell-opaleye/), and [postgresql-simple](https://github.com/lpsmith/postgresql-simple). The goal is to remove the redundancies between the first two libraries and "inline" as much code and library logic as possible, staying as close to Tisch as possible.

## Amalgam

Amalgam is a logging monad transformer. It's intended to be a thin layer over a fork of [fast-logger](https://www.stackage.org/package/fast-logger) and [monad-logger](https://www.stackage.org/package/monad-logger), providing concurrent and pretty terminal logging.

## Credits

I learned of the *exocortex* neologism from Shrutarshi Basu ([@basus](https://twitter.com/basus)).
