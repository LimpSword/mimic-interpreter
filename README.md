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

## Librairies utilisées

- Megaparsec : pour parser le langage

## Tests disponibles

Des tests sont disponibles dans le dossier `examples`.

## Execution des Tests

Ils est possible de d'éxécuter les test avec TestAll.hs
Il compare la sortie du chaque programme avec la sortie attendu de `examples_stdout`

Pour l'éxécuter il est nécessaire de faire :

```bash
cabal build 
cabal run TestAll.hs```

