# minic-interpreter

## Compiler l'interpréteur

Vous pouvez compiler l'interpréteur avec la commande suivante :
```bash
cabal build
```

## Exécuter l'interpréteur sur un programme

Vous pouvez exécuter un fichier `example.c` avec la commande suivante :
```bash
./mini-c example.c
```
Il est également possible d'exécuter directement avec cabal :
```bash
cabal run Main.hs example.c
```

## Librairies utilisées

- Megaparsec : pour parser le langage

## Tests disponibles

Des tests sont disponibles dans le dossier `examples`.

## Exécution des Tests

Ils est possible de d'exécuter les tests avec TestAll.hs.
Il compare la sortie du chaque programme avec la sortie attendue de `examples_stdout`.

Pour l'exécuter il est nécessaire de faire :

```bash
cabal build 
cabal run TestAll.hs
```

## Bugs rencontrés 

Si le programme commence par un ou plusieurs espaces ou sauts de ligne, alors le parsing ne se fait pas et un arbre vide est retourné.
