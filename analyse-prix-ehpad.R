
# entretien CNSA - faire une analyse rapide pour expliquer le prix de "l'hébergement permanent en chambre seule" à partir des données sur l'établissement d'hébergement pour personnes âgées dépendantes (EHPAD)

# packages ----
library(tidyverse)
library(janitor)
library(sf)
library(gt)
library(leaflet) # si j'ai le temps de faire une carte

# import des données ----

## 1) Prix hébergement et tarifs dépendance des EHPAD - données brutes ----
# https://www.data.gouv.fr/fr/datasets/prix-hebergement-et-tarifs-dependance-des-ehpad-donnees-brutes/
# (données retravaillées ailleurs)

# MAD des données au 31/12 de l'année passée, ou au dernier jour du mois précédent pour l'année en cours 
# par curiosité, on va regarder les deux types (a l'air d'y avoir les mêmes colonnes selon préviz)
prix_202405_url <- "https://www.data.gouv.fr/fr/datasets/r/e04c8d5e-9287-4b59-a31a-843729e223b4" # url STABLE

prix_2023_url <- "https://www.data.gouv.fr/fr/datasets/r/909d509b-cb05-40ee-b596-32cea668cd84"

# à quoi ça ressemble
read_csv2(prix_202405_url) |> glimpse() # 4388x40
read_csv2(prix_2023_url) |> glimpse() # 6567x40

# je pars sur l'import des données du 31 décembre 2023 
prix_2023 <- read_csv2(prix_2023_url) |> 
  # nettoyage noms colonnes 
  clean_names()

# à quoi ça ressemble maintenant 
glimpse(prix_2023)

prix_2023 |> 
  count(year(date_maj), month(date_maj))

# identifiant organisme = clé ?
n_distinct(prix_2023$finess_et)
n_distinct(prix_2023$finess_et) == nrow(prix_2023) # oui

## 2) FINESS extraction du fichier des établissements ----
# https://www.data.gouv.fr/fr/datasets/finess-extraction-du-fichier-des-etablissements/
# on a les prix, maintenant il faut les infos sur les organismes

# version avec coord, version sans, version historique 2004-2023
# let's go pour la version standard (au 13/05/2024 à la date de ce prg)
finess_url <- "https://www.data.gouv.fr/fr/datasets/r/2ce43ade-8d2c-4d1d-81da-ca06c82abc68" # url STABLE du CSV

# fichier CSV, données qui commençent à la deuxième ligne, noms de colonnes dans PDF...on adapte 
prep_finess <- read_delim(finess_url, delim = ";", skip = 1, col_names = F) # très étrange quand même, les noms de colonnes à part
glimpse(prep_finess)

# j'ai copié le tableau du PDF avec les noms de colonnes. enregistré dans vecteur char
prep_finess_noms_vec <- "Donnée Balise XML Numéro d’ordre
Section : structureet – 1
Numéro FINESS ET nofinesset 2
Numéro FINESS EJ nofinessej 3
Raison sociale rs 4
Raison sociale longue rslongue 5
Complément de raison sociale complrs 6
Complément de distribution compldistrib 7
Numéro de voie numvoie 8
Type de voie typvoie 9
Libellé de voie voie 10
Complément de voie compvoie 11
Lieu-dit / BP lieuditbp 12
Code Commune commune 13
Département departement 14
Libellé département libdepartement 15
Ligne d’acheminement (CodePostal+Lib commune) ligneacheminement 16
Téléphone telephone 17
Télécopie telecopie 18
Catégorie d’établissement categetab 19
Libelle catégorie d’établissement libcategetab 20
Catégorie d’agrégat d’établissement categagretab 21
Libellé catégorie d’agrégat d’établissement libcategagretab 22
Numéro de SIRET siret 23
Code APE codeape 24
Code MFT codemft 25
Libelle MFT libmft 26
Code SPH codesph 27
Libelle SPH libsph 28
Date d’ouverture dateouv 29
Date d’autorisation dateautor 30
Date de mise à jour sur la structure datemaj 31
Numéro éducation nationale numuai 32"

prep_finess_noms_vec
# \n pour séparer les lignes. du coup je sépare le vecteur avec ça, pour avoir un élément pour chaque ligne du tableau 

vec_cols_sep <- str_split_1(prep_finess_noms_vec, "\n")[-1] # sans l'en-tête du tableau
vec_cols_sep

# faut garder l'avant-dernier mot. 
# on sépare chaque item par l'espace
t1 <- str_split(vec_cols_sep, "\\s")
t1[[1]][length(t1[[1]])-1] # yeees. pour une meilleure compréhension : de t[[1]], je demande le dernier élement du vecteur à partir de la longueur du vecteur (donc "1"), MOINS 1, donc l'avant dernier élement

# je réplique la manip pour chaque item de la liste (normalement 32)
prep_finess_noms <- map_chr(seq_along(t1), \(x) t1[[x]][length(t1[[x]])-1])
prep_finess_noms # reste que premier élément à modif 
prep_finess_noms[1] <- "structureet"

# j'applique le vecteur de nom à la table FINESS
names(prep_finess) <- prep_finess_noms

prep_finess |> glimpse()

# explo rapide
prep_finess |> 
  select(libdepartement, libcategetab, libcategagretab, libmft, libsph) |> 
  map(\(x) table(x, useNA = "ifany"))
# dept 75 +étabs
# commerce de biens à usage médicaux ? 

# code categetab pour EHPAD ?
prep_finess |> 
  filter(str_detect(libcategetab, "dépendantes$")) |> 
  distinct(categetab, libcategetab, categagretab, libcategagretab) |> view()
# categetab == 500, categagregetab == 4401 (plutôt categetab)

# identifiant FINESS = clé ? 
n_distinct(prep_finess$nofinesset) == nrow(prep_finess) # clé unique = nofinesset

# on réduit pour garder l'essentiel : les EHPAD, les identifiants, le département, le libmft, le libsph, et la date d'ouverture
liste_ehpad <- prep_finess |> 
  filter(categetab == 500) |> 
  select(nofinesset, departement, libdepartement, codemft, libmft, codesph, libsph, dateouv) 

# base de travail ----
# prix des EHPAD au 31/12/2023
prix_ehpad_2023 <- prix_2023 |> 
  select(finess_et, prix_heb_perm_cs) |> 
  # jointure liste des ehpad 
  inner_join(liste_ehpad, by = join_by(finess_et == nofinesset)) |> 
  # nettoyage rapide
  mutate(libdepartement = str_to_title(libdepartement))

arrow::write_parquet(prix_ehpad_2023, "prix_ehpad_2023.parquet")
