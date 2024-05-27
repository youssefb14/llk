# Compilateur Pascal vers Assembleur

## Description du Projet

Ce projet est un compilateur qui traduit un langage structuré simplifié de type Pascal en langage Assembleur 64 bits 80x86. Il est basé sur le travail réalisé par Pierre Jourlin en 2019 et complété par BOUDOUNT Youssef.

## Fonctionnalités

- Support des boucles `for`.
- Support des instructions `case`.
- Support des boucles `repeat`.
- Gestion des listes d'étiquettes et des éléments de liste pour les instructions `case`.

## Installation

Pour installer et compiler le projet, suivez les étapes ci-dessous :

1. Clonez le dépôt :

   ```bash
   git clone [https://github.com/youssefb14/llk.git]
   ```
2. Accédez au répertoire du projet

```bash
   cd [llk]
```

3. Compilez le projet en utilisant `make` :

```bash
make
```

## Compilation et test

Pour compiler le projet, utilisez la commande :

```bash
  make test
```

Pour vérifier la sortie :

```bash
gedit test.s
```

Pour déboguer l'exécutable avec DDD (Data Display Debugger), utilisez la commande :

```bash
  ddd ./test
```

Pour installer DDD sur Ubuntu :

```bash
sudo apt install ddd
```


## Licence

Ce programme est un logiciel libre : vous pouvez le redistribuer ou le modifier selon les termes de la licence publique générale GNU telle que publiée par la Free Software Foundation, version 3 ou ultérieure.

## Auteur

Youssef BOUDOUNT

* Licence informatique
* Email : [youssef.boudount@alumni.univ-avignon.fr]()
* GitHub : [@youssefb14](https://github.com/youssefb14)
* LinkedIn : [@youssefb14](https://linkedin.com/in/youssefb14)
