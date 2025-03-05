# Environnement de développement COBOL avec Docker

Ce projet fournit un ensemble de scripts batch pour faciliter le développement COBOL sous Windows en utilisant Docker, sans avoir besoin d'installer un compilateur COBOL directement sur votre système.

## Prérequis

- Docker Desktop doit être installé et en cours d'exécution
- Les scripts batch (.bat) fonctionnent avec CMD, PowerShell ou Git Bash

## Scripts disponibles

### cobol-verify-env.bat

Script utilitaire qui vérifie l'environnement Docker pour COBOL. Ce script est appelé automatiquement par les autres scripts et n'a pas besoin d'être exécuté manuellement.

**Vérifications :**
- Vérifie que Docker est en cours d'exécution
- Vérifie la connectivité réseau Docker
- Vérifie que l'image COBOL est disponible (la télécharge si nécessaire)
- Teste que l'image COBOL est fonctionnelle

### cobol-compile.bat

Ce script compile un programme COBOL sans l'exécuter.

**Usage :**
```
./cobol-compile.bat chemin/vers/mon_programme.cbl
```

**Fonctionnalités :**
- Crée un fichier exécutable (.exe) dans le même répertoire que le fichier source
- Affiche le chemin du fichier exécutable créé

### cobol-execute.bat

Ce script exécute un programme COBOL déjà compilé ou compile et exécute un fichier source.

**Usage :**
```
./cobol-execute.bat chemin/vers/mon_programme.exe
```
ou
```
./cobol-execute.bat chemin/vers/mon_programme.cbl
```

**Fonctionnalités :**
- Si un fichier .cbl est fourni, cherche le fichier .exe correspondant
- Exécute le programme dans l'environnement Docker

### cobol-run.bat

Ce script combine la compilation et l'exécution en une seule commande.

**Usage :**
```
./cobol-run.bat chemin/vers/mon_programme.cbl
```

**Fonctionnalités :**
- Compile le programme COBOL spécifié dans son répertoire d'origine
- Exécute immédiatement le programme compilé
- Idéal pour le développement et les tests rapides

### cobol-shell.bat

Ce script lance un shell interactif dans le conteneur Docker COBOL.

**Usage :**
```
./cobol-shell.bat
```

**Fonctionnalités :**
- Donne accès à un terminal Linux avec le compilateur COBOL installé
- Permet d'exécuter des commandes COBOL et Linux directement
- Utile pour le débogage avancé et l'exploration des fonctionnalités

## Exemples d'utilisation

### Exemple 1 : Compiler et exécuter un programme

```
./cobol-run.bat Projet/HelloWorld/hello.cbl
```

### Exemple 2 : Compiler seulement

```
./cobol-compile.bat Projet/Calculatrice/calculatrice.cbl
```

### Exemple 3 : Exécuter un programme déjà compilé

```
./cobol-execute.bat Projet/HelloWorld/hello.exe
```

### Exemple 4 : Utiliser le shell pour des opérations avancées

```
./cobol-shell.bat
```

Dans le shell :
```
ls /code                         # Liste les fichiers du répertoire courant
cobc -x -o hello.exe hello.cbl   # Compile manuellement un programme
./hello.exe                      # Exécute le programme
```

## Structure des fichiers COBOL

Les fichiers COBOL doivent avoir l'extension `.cbl` et suivre la structure standard COBOL :

```cobol
000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID. HELLO.
000030 PROCEDURE DIVISION.
000040 DISPLAY "Hello, World!".
000050 STOP RUN.
```

Les numéros de séquence en début de ligne (000010, etc.) sont optionnels mais recommandés pour assurer la compatibilité avec d'autres environnements COBOL.

## Organisation du projet

Il est recommandé d'organiser vos programmes COBOL dans des sous-répertoires par projet :

```
cobol-projects/
├── Projet1/
│   ├── programme1.cbl
│   └── programme2.cbl
├── Projet2/
│   └── programme3.cbl
├── cobol-compile.bat
├── cobol-execute.bat
├── cobol-run.bat
├── cobol-shell.bat
└── cobol-verify-env.bat
```

## Dépannage

### Problèmes courants :

1. **Docker n'est pas en cours d'exécution :**
   - Assurez-vous que Docker Desktop est démarré

2. **Problèmes de permission :**
   - Exécutez les scripts en tant qu'administrateur si nécessaire

3. **Erreurs de compilation :**
   - Vérifiez la syntaxe de votre code COBOL
   - Utilisez `./cobol-shell.bat` pour obtenir plus de détails sur les erreurs

4. **Caractères spéciaux incorrects :**
   - Si vous utilisez Git Bash et que vous voyez des caractères étranges, c'est normal en raison de l'encodage
   - Les scripts fonctionnent correctement malgré l'affichage