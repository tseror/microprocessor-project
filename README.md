# Projet - Systèmes Numériques 

## Simulateur

Pour compiler le simulateur de netlists : `make simulateur`.

On exécutera le fichier `simulator/simulator.byte` avec les paramètres suivants :

* `(filename)` : Nom du fichier contenant la netlist.
* `-n (number_steps)` : Nombre d'étapes pour la simulation. Par défaut, vaut 0.
* `-print` : Si seul le résultat du scheduling doit être affiché (optionnel)
* `-rom (rom_filename)` : Nom du fichier contenant la ROM. Par défaut aucune ROM n'est chargée.
* `-ram (ram_filename)` : Nom du fichier contenant la RAM. Par défaut aucune RAM n'est chargée.
