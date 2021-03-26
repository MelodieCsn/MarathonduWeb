# MarathonduWeb GROUPE 7

**\/!\\\/!\\\/!\\Attention : DEZIPPER LE CONTENU DE "R_Data/Map.zip" DANS "R_Data/Map" !!!!!!!!!!!!!!!\/!\\\/!\\\/!\\**
**\/!\\\/!\\\/!\\Attention : DEZIPPER LE CONTENU DE "R_Data/Map.zip" DANS "R_Data/Map" !!!!!!!!!!!!!!!\/!\\\/!\\\/!\\**
**\/!\\\/!\\\/!\\Attention : DEZIPPER LE CONTENU DE "R_Data/Map.zip" DANS "R_Data/Map" !!!!!!!!!!!!!!!\/!\\\/!\\\/!\\**

Projet de création d'un site web orienté data visualisation en Rshiny

Un Plus Bio est une association qui, depuis vingt ans, interroge l’évolution des approches et des métiers de la restauration collective, avec pour finalité de changer notre alimentation pour aller vers plus de bio et de local.
Ce projet consite à valoriser ces progrès d'inclusion du bio et du local dans les collectivités territoriales dont l'association possède les données. 
Les élus pourront, à l'issu de notre travail, remplir un questionnaire concernant leur collectivité, mais aussi visualiser 
des graphiques concernant les statistiques d'autres collectivités, pour répondre à leurs questions potentielles sur la démarche d'inclusion d'une
alimentation bio et locale. Enfin, les élus pourront accéder à une carte interactive qui leur permettra de se situer par rapport aux autres collectivités
et comparer les résultats de leur démarche avec des collectivités leur ressemblant.

Données : 
Nous avons accès aux données des questionnaires d'environ 200 questions chacuns remplis par les élus depuis 2017, soit entre 150 et 200 collectivités par année.
Ces questionnaire comprennent notamment des informations comme la fréquence de service de repas végétariens, le pourcentage de produits bio et/ou locaux achetés, le prix du repas, le type de viande bio servie, etc.
Ces données sont des fichiers xlxs de 100 Ko chacun. Nous avons également la liste des participants à ces questionnaires avec d'autres informations les concernant (adresse, type de collectivité, taille de collectivité, nombre de repas servis par jour, mode de production) avec pour seul lien entre
les deux le code postal, ce qui permet de garder l'anonima des collectivités. 
L'association peut accéder aux réponses des questionnaire grâce à un compte Lime Survey, qui gère la sécurité et les questionnaires en général. L'API de Lime survey serait accessible mais c'est à voir.

Ce projet est un partenariat entre les M1 CNO et les M1 MIASHS, les premiers s'occupant de la partie communication autour du projet, 
les seconds s'occupant de la création du site et de la visualisation des données.
Nous communiquons via un serveur discord, nous nous répartissons les tâches en utilisant la partie projet de ce github, avec pour l'instant Emmanuelle pour la carte interactive, Moulika et Mélodie pour les statistique descriptive dynamiques, et Jonathan pour la gestion du serveur pour accéder aux réponses du questionnaire.

Les comptes rendus sont accessible en utilisant le lien ci dessous pour être ajouté à la liste des utilisateurs pour lesquels les dossiers sont partagés :
https://drive.google.com/drive/folders/1PShdJMeQ_EUhR7OVe4taoAZkL8tEcHho?usp=sharing

Pour les M1 MIASHS, nos tâches s'articulerons autour de trois éléments principaux:
 - Premièrement, l'affichage de statistiques descriptives générales sur les données de l'association avec des paramètres personnalisables pour répondre à trois questions principales 
 - Deuxièmement, la réalisation du questionnaire demandé en se basant sur l'ancien questionnaire
 - Troisièmement, la réalisation de la carte interactive permettant d'afficher à l'échelle départementale des statistiques générales en fonction de paramètres réglables

Ces étapes ont été présentées à notre commanditaire et ont été validées par cette dernière. Nous la tenons également à jour de nos avancées et des possibles modifications que nous apportons au projet.
