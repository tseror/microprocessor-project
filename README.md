# Projet - Systèmes Numériques 

## Simulateur

Pour compiler le simulateur de netlists : `make simulateur`.

On exécutera le fichier `simulator/simulator.byte` avec les paramètres suivants :

* `(filename)` : Nom du fichier contenant la netlist.
* `-n (number_steps)` : Nombre d'étapes pour la simulation. Par défaut, vaut 0.
* `-print` : Si seul le résultat du scheduling doit être affiché (optionnel)
* `-rom (rom_filename)` : Nom du fichier contenant la ROM. Par défaut aucune ROM n'est chargée.
* `-ram (ram_filename)` : Nom du fichier contenant la RAM. Par défaut aucune RAM n'est chargée.

Le programme est évaluée par la fonction `simulator` de `simulateur/netlist_simulator.ml`. Les variables sont stockées dans un tableau. Par convention, la RAM utilisée est de taille variable : à chaque fois que le programme rencontre un appel RAM, si la taille de RAM appelée est supérieure à celle en mémoire, alors la RAM en mémoire est étendu avec des 0.
