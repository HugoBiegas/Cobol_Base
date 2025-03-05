# Guide complet d'initiation au COBOL

## 1. Introduction au COBOL

### Historique et importance
- COBOL (Common Business-Oriented Language) a été créé en 1959
- Conçu par Grace Hopper et le comité CODASYL
- Objectif principal : standardiser la programmation d'entreprise
- Reste aujourd'hui essentiel dans les systèmes financiers, bancaires et administratifs
- Selon les estimations, 70-80% des transactions commerciales mondiales s'exécutent sur des systèmes COBOL

### Caractéristiques principales
- Langage verbeux, proche de l'anglais naturel
- Auto-documenté et facile à lire
- Hautement stable et compatible avec les versions antérieures
- Excellente précision pour les calculs financiers
- Structure formelle et rigoureuse

## 2. Structure d'un programme COBOL

Un programme COBOL se compose de quatre divisions principales :

```cobol
IDENTIFICATION DIVISION.
ENVIRONMENT DIVISION.
DATA DIVISION.
PROCEDURE DIVISION.
```

### Règles de base
- Les lignes de code COBOL sont traditionnellement divisées en 80 colonnes
- Colonnes 1-6 : réservées pour les numéros de ligne (optionnels)
- Colonne 7 : indicateur spécial (* pour commentaires, - pour continuation)
- Colonnes 8-72 : code du programme
- Tout mot-clé doit être suivi d'un espace et d'un point pour terminer une instruction

## 3. Les divisions du COBOL en détail

### IDENTIFICATION DIVISION
Identifie le programme et fournit des informations générales.

```cobol
IDENTIFICATION DIVISION.
   PROGRAM-ID. MONPROGRAMME.
   AUTHOR. VOTRE-NOM.
   DATE-WRITTEN. 05/03/2025.
   DATE-COMPILED. 05/03/2025.
```

### ENVIRONMENT DIVISION
Spécifie l'environnement d'exécution du programme.

```cobol
ENVIRONMENT DIVISION.
   CONFIGURATION SECTION.
      SOURCE-COMPUTER. PC.
      OBJECT-COMPUTER. PC.
   
   INPUT-OUTPUT SECTION.
      FILE-CONTROL.
         SELECT FICHIER-CLIENT ASSIGN TO "clients.dat"
         ORGANIZATION IS SEQUENTIAL.
```

### DATA DIVISION
Définit toutes les données utilisées par le programme.

```cobol
DATA DIVISION.
   FILE SECTION.
   FD FICHIER-CLIENT
      RECORD CONTAINS 80 CHARACTERS.
   01 ENREGISTREMENT-CLIENT.
      05 CLIENT-ID       PIC 9(5).
      05 CLIENT-NOM      PIC X(20).
      05 CLIENT-PRENOM   PIC X(15).
      05 CLIENT-SOLDE    PIC 9(7)V99.
      05 FILLER          PIC X(31).
   
   WORKING-STORAGE SECTION.
   01 WS-VARIABLES.
      05 WS-TOTAL        PIC 9(9)V99 VALUE ZEROS.
      05 WS-COMPTEUR     PIC 9(3) VALUE ZERO.
      05 WS-FIN-FICHIER  PIC X(1) VALUE 'N'.
         88 FIN-FICHIER  VALUE 'Y'.
```

### PROCEDURE DIVISION
Contient toutes les instructions exécutables du programme.

```cobol
PROCEDURE DIVISION.
   MAIN-PROCEDURE.
      PERFORM INITIALISATION
      PERFORM TRAITEMENT UNTIL FIN-FICHIER
      PERFORM FINALISATION
      STOP RUN.
   
   INITIALISATION.
      OPEN INPUT FICHIER-CLIENT
      READ FICHIER-CLIENT
         AT END MOVE 'Y' TO WS-FIN-FICHIER
      END-READ.
   
   TRAITEMENT.
      ADD CLIENT-SOLDE TO WS-TOTAL
      ADD 1 TO WS-COMPTEUR
      READ FICHIER-CLIENT
         AT END MOVE 'Y' TO WS-FIN-FICHIER
      END-READ.
   
   FINALISATION.
      DISPLAY "Nombre de clients: " WS-COMPTEUR
      DISPLAY "Total des soldes: " WS-TOTAL
      CLOSE FICHIER-CLIENT.
```

## 4. Types de données et variables

### Définition des données avec PIC (Picture Clause)

Le mot clé PIC définit le format d'une donnée :

- **9** : Caractère numérique
- **X** : Tout caractère alphanumerique
- **A** : Caractère alphabétique uniquement
- **V** : Position décimale implicite (pas de stockage physique)
- **S** : Signe (positif/négatif)

Exemples :
```cobol
05 MONTANT      PIC 9(5)V99.    * 5 chiffres entiers, 2 décimales
05 NOM          PIC X(20).      * 20 caractères alphanumériques
05 CODE         PIC A(3).       * 3 caractères alphabétiques
05 SOLDE        PIC S9(7)V99.   * Nombre signé avec 7 chiffres et 2 décimales
```

### Niveaux de données

Les niveaux définissent la hiérarchie des données :
- **01** : Niveau principal
- **05**, **10**, **15**... : Sous-niveaux
- **66** : Élément de renommage (RENAMES)
- **77** : Élément indépendant
- **88** : Condition (valeurs spécifiques)

Exemple :
```cobol
01 PERSONNE.
   05 IDENTITE.
      10 NOM      PIC X(20).
      10 PRENOM   PIC X(15).
   05 COORDONNEES.
      10 ADRESSE  PIC X(30).
      10 VILLE    PIC X(20).
   05 STATUT      PIC X.
      88 ACTIF    VALUE 'A'.
      88 INACTIF  VALUE 'I'.
```

## 5. Opérations et expressions

### Opérateurs arithmétiques
- ADD : Addition
- SUBTRACT : Soustraction
- MULTIPLY : Multiplication
- DIVIDE : Division
- COMPUTE : Expressions complexes

Exemples :
```cobol
ADD VALEUR1 TO VALEUR2
SUBTRACT COUT FROM PRIX GIVING MARGE
MULTIPLY QUANTITE BY PRIX GIVING TOTAL
DIVIDE TOTAL BY NOMBRE GIVING MOYENNE
COMPUTE RESULTAT = (VALEUR1 + VALEUR2) * TAUX / 100
```

### Opérateurs de comparaison
- EQUAL TO (ou =)
- GREATER THAN (ou >)
- LESS THAN (ou <)
- GREATER THAN OR EQUAL TO (ou >=)
- LESS THAN OR EQUAL TO (ou <=)
- NOT EQUAL TO (ou <>)

## 6. Structures de contrôle

### Conditions IF...ELSE
```cobol
IF AGE > 18
   DISPLAY "Majeur"
ELSE
   DISPLAY "Mineur"
END-IF.

IF STATUT = 'A'
   DISPLAY "Actif"
ELSE IF STATUT = 'S'
   DISPLAY "Suspendu"
ELSE
   DISPLAY "Inactif"
END-IF.
```

### Boucle PERFORM
```cobol
* Boucle simple
PERFORM TRAITEMENT-LIGNE 10 TIMES.

* Boucle conditionnelle
PERFORM TRAITEMENT-DONNEES UNTIL FIN-FICHIER.

* Boucle avec compteur
PERFORM VARYING COMPTEUR FROM 1 BY 1 UNTIL COMPTEUR > 100
   DISPLAY COMPTEUR
END-PERFORM.
```

### EVALUATE (équivalent du CASE/SWITCH)
```cobol
EVALUATE CODE-STATUT
   WHEN 1
      DISPLAY "En cours"
   WHEN 2
      DISPLAY "Terminé"
   WHEN 3
      DISPLAY "Erreur"
   WHEN OTHER
      DISPLAY "Statut inconnu"
END-EVALUATE.
```

## 7. Manipulation de fichiers

### Types de fichiers
- Séquentiel : Lecture/écriture dans l'ordre
- Indexé : Accès direct via une clé
- Relatif : Accès par numéro d'enregistrement

### Opérations de base
```cobol
* Ouverture de fichier
OPEN INPUT FICHIER-ENTREE
OPEN OUTPUT FICHIER-SORTIE
OPEN I-O FICHIER-MAJABLE

* Lecture
READ FICHIER-ENTREE
   AT END SET FIN-FICHIER TO TRUE
END-READ.

* Écriture
WRITE ENREGISTREMENT-SORTIE
   INVALID KEY DISPLAY "Erreur d'écriture"
END-WRITE.

* Mise à jour
REWRITE ENREGISTREMENT
   INVALID KEY DISPLAY "Enregistrement introuvable"
END-REWRITE.

* Suppression
DELETE FICHIER-CLIENT
   INVALID KEY DISPLAY "Suppression impossible"
END-DELETE.

* Fermeture
CLOSE FICHIER-ENTREE
CLOSE FICHIER-SORTIE.
```

## 8. Sous-programmes et modularité

### Sections et paragraphes
```cobol
PROCEDURE DIVISION.
   MAIN-SECTION.
      PERFORM INITIALISATION
      PERFORM TRAITEMENT
      PERFORM FINALISATION
      STOP RUN.
   
   INITIALISATION.
      OPEN INPUT FICHIER-ENTREE.
      OPEN OUTPUT FICHIER-SORTIE.
   
   TRAITEMENT.
      PERFORM UNTIL FIN-FICHIER
         READ FICHIER-ENTREE
            AT END SET FIN-FICHIER TO TRUE
            NOT AT END PERFORM TRAITER-LIGNE
         END-READ
      END-PERFORM.
   
   TRAITER-LIGNE.
      * Traitement spécifique
      WRITE ENREGISTREMENT-SORTIE.
   
   FINALISATION.
      CLOSE FICHIER-ENTREE
      CLOSE FICHIER-SORTIE.
```

### Appel à des programmes externes
```cobol
CALL "SOUS-PROGRAMME" USING PARAMETRE1, PARAMETRE2.
```

## 9. Exemple complet : Programme de gestion de clients

```cobol
IDENTIFICATION DIVISION.
   PROGRAM-ID. GESTION-CLIENTS.
   AUTHOR. VOTRE-NOM.

ENVIRONMENT DIVISION.
   INPUT-OUTPUT SECTION.
      FILE-CONTROL.
         SELECT FICHIER-CLIENTS ASSIGN TO "CLIENTS.DAT"
         ORGANIZATION IS INDEXED
         ACCESS MODE IS DYNAMIC
         RECORD KEY IS CLIENT-ID.

DATA DIVISION.
   FILE SECTION.
   FD FICHIER-CLIENTS.
   01 ENREGISTREMENT-CLIENT.
      05 CLIENT-ID       PIC 9(5).
      05 CLIENT-NOM      PIC X(20).
      05 CLIENT-PRENOM   PIC X(15).
      05 CLIENT-ADRESSE  PIC X(30).
      05 CLIENT-VILLE    PIC X(20).
      05 CLIENT-CP       PIC X(5).
      05 CLIENT-SOLDE    PIC S9(7)V99.
      
   WORKING-STORAGE SECTION.
   01 WS-VARIABLES.
      05 WS-CHOIX        PIC 9.
      05 WS-FIN          PIC X VALUE 'N'.
         88 FIN-PROGRAMME  VALUE 'O'.
      05 WS-EXIST        PIC X.
         88 CLIENT-EXISTE  VALUE 'O'.
      05 WS-ID-RECH      PIC 9(5).

PROCEDURE DIVISION.
   MAIN-PROCEDURE.
      PERFORM INITIALISATION
      PERFORM MENU-PRINCIPAL UNTIL FIN-PROGRAMME
      PERFORM FINALISATION
      STOP RUN.
   
   INITIALISATION.
      OPEN I-O FICHIER-CLIENTS.
   
   MENU-PRINCIPAL.
      DISPLAY "===== GESTION DES CLIENTS ====="
      DISPLAY "1. Ajouter un client"
      DISPLAY "2. Modifier un client"
      DISPLAY "3. Supprimer un client"
      DISPLAY "4. Consulter un client"
      DISPLAY "5. Quitter"
      DISPLAY "Votre choix: " WITH NO ADVANCING
      ACCEPT WS-CHOIX
      
      EVALUATE WS-CHOIX
         WHEN 1
            PERFORM AJOUTER-CLIENT
         WHEN 2
            PERFORM MODIFIER-CLIENT
         WHEN 3
            PERFORM SUPPRIMER-CLIENT
         WHEN 4
            PERFORM CONSULTER-CLIENT
         WHEN 5
            MOVE 'O' TO WS-FIN
         WHEN OTHER
            DISPLAY "Choix invalide!"
      END-EVALUATE.
   
   AJOUTER-CLIENT.
      DISPLAY "ID du client: " WITH NO ADVANCING
      ACCEPT CLIENT-ID
      
      MOVE 'N' TO WS-EXIST
      READ FICHIER-CLIENTS
         INVALID KEY MOVE 'N' TO WS-EXIST
         NOT INVALID KEY MOVE 'O' TO WS-EXIST
      END-READ
      
      IF CLIENT-EXISTE
         DISPLAY "Ce client existe déjà!"
      ELSE
         DISPLAY "Nom: " WITH NO ADVANCING
         ACCEPT CLIENT-NOM
         DISPLAY "Prénom: " WITH NO ADVANCING
         ACCEPT CLIENT-PRENOM
         DISPLAY "Adresse: " WITH NO ADVANCING
         ACCEPT CLIENT-ADRESSE
         DISPLAY "Ville: " WITH NO ADVANCING
         ACCEPT CLIENT-VILLE
         DISPLAY "Code postal: " WITH NO ADVANCING
         ACCEPT CLIENT-CP
         DISPLAY "Solde: " WITH NO ADVANCING
         ACCEPT CLIENT-SOLDE
         
         WRITE ENREGISTREMENT-CLIENT
            INVALID KEY DISPLAY "Erreur d'écriture!"
         END-WRITE
      END-IF.
   
   MODIFIER-CLIENT.
      DISPLAY "ID du client à modifier: " WITH NO ADVANCING
      ACCEPT WS-ID-RECH
      
      MOVE WS-ID-RECH TO CLIENT-ID
      MOVE 'N' TO WS-EXIST
      READ FICHIER-CLIENTS
         INVALID KEY MOVE 'N' TO WS-EXIST
         NOT INVALID KEY MOVE 'O' TO WS-EXIST
      END-READ
      
      IF NOT CLIENT-EXISTE
         DISPLAY "Client non trouvé!"
      ELSE
         DISPLAY "Nom actuel: ", CLIENT-NOM
         DISPLAY "Nouveau nom (Entrée pour conserver): " WITH NO ADVANCING
         ACCEPT CLIENT-NOM
         
         * Répéter pour les autres champs...
         
         REWRITE ENREGISTREMENT-CLIENT
            INVALID KEY DISPLAY "Erreur de mise à jour!"
         END-REWRITE
      END-IF.
   
   SUPPRIMER-CLIENT.
      DISPLAY "ID du client à supprimer: " WITH NO ADVANCING
      ACCEPT WS-ID-RECH
      
      MOVE WS-ID-RECH TO CLIENT-ID
      DELETE FICHIER-CLIENTS
         INVALID KEY DISPLAY "Client non trouvé!"
         NOT INVALID KEY DISPLAY "Client supprimé!"
      END-DELETE.
   
   CONSULTER-CLIENT.
      DISPLAY "ID du client à consulter: " WITH NO ADVANCING
      ACCEPT WS-ID-RECH
      
      MOVE WS-ID-RECH TO CLIENT-ID
      READ FICHIER-CLIENTS
         INVALID KEY DISPLAY "Client non trouvé!"
         NOT INVALID KEY PERFORM AFFICHER-CLIENT
      END-READ.
   
   AFFICHER-CLIENT.
      DISPLAY "===== DÉTAILS DU CLIENT ====="
      DISPLAY "ID     : ", CLIENT-ID
      DISPLAY "Nom    : ", CLIENT-NOM
      DISPLAY "Prénom : ", CLIENT-PRENOM
      DISPLAY "Adresse: ", CLIENT-ADRESSE
      DISPLAY "Ville  : ", CLIENT-VILLE
      DISPLAY "CP     : ", CLIENT-CP
      DISPLAY "Solde  : ", CLIENT-SOLDE.
   
   FINALISATION.
      CLOSE FICHIER-CLIENTS.

```

## 10. Conseils pour bien démarrer avec COBOL

### Bonnes pratiques
1. **Nommage clair** - Utilisez des noms de variables descriptifs
2. **Commentaires** - Commentez généreusement votre code
3. **Indentation** - Maintenez une indentation cohérente
4. **Modularité** - Divisez votre programme en sections logiques
5. **Test rigoureux** - Testez systématiquement vos programmes

### Erreurs courantes à éviter
1. Oublier les points après les instructions
2. Mal aligner le code (colonnes importantes)
3. Négliger la gestion des erreurs
4. Utiliser des variables non initialisées
5. Créer des boucles infinies

## 11. Ressources pour continuer l'apprentissage

### Compilateurs et environnements
- GnuCOBOL (OpenCOBOL) - compilateur gratuit et open source
- IBM Enterprise COBOL - environnement professionnel
- Micro Focus Visual COBOL - IDE moderne pour COBOL

### Livres recommandés
- "Beginning COBOL for Programmers" par Michael Coughlan
- "COBOL: From Micro to Mainframe" par Robert Grauer et al.
- "Murach's Structured COBOL" par Mike Murach et Anne Prince