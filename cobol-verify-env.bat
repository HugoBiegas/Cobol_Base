@echo off
setlocal EnableDelayedExpansion

REM ================================================
REM Script de verification de l'environnement COBOL
REM ================================================

echo ================================================
echo Verification de l'environnement COBOL...
echo ================================================

REM Verification si Docker est en cours d'execution
echo Verification du service Docker...
docker info >nul 2>&1
IF ERRORLEVEL 1 (
    echo [ERREUR] Docker n'est pas demarre!
    echo Veuillez lancer Docker Desktop puis reessayer.
    exit /b 1
)
echo [OK] Service Docker detecte.

REM Verification de la connectivite reseau Docker
echo Verification de la connectivite reseau Docker...
docker network ls >nul 2>&1
IF ERRORLEVEL 1 (
    echo [AVERTISSEMENT] Probleme avec les reseaux Docker.
    echo Le conteneur pourrait avoir des problemes de connectivite.
)
echo [OK] Reseaux Docker fonctionnels.

REM Verification si l'image COBOL est disponible
echo Verification de l'image Docker COBOL...
docker image inspect esolang/cobol >nul 2>&1
IF ERRORLEVEL 1 (
    echo L'image Docker pour COBOL n'est pas disponible. Telechargement en cours...
    docker pull esolang/cobol
    IF ERRORLEVEL 1 (
        echo [ERREUR] Impossible de telecharger l'image Docker pour COBOL.
        exit /b 2
    )
    echo [OK] Image Docker telechargee avec succes.
) else (
    echo [OK] Image Docker COBOL trouvee.
)

REM Verification que l'image est fonctionnelle
echo Test de l'image COBOL...
docker run --rm esolang/cobol echo "Image COBOL operationnelle" >nul 2>&1
IF ERRORLEVEL 1 (
    echo [ERREUR] Impossible d'utiliser l'image COBOL.
    echo Verifiez que Docker a assez de ressources et de permissions.
    exit /b 3
)
echo [OK] Image COBOL prete a etre utilisee.

REM Si on arrive ici, l'environnement est correctement configuré
exit /b 0