# lecture du fichier des fonctions
source(file = "R/01_fonctions.R")

# -------------------------------------------------
# Paramétrage
# Liste des départements / année
mes_depts <- c('22', '29', '35', '56')
# mes_depts <- c('49', '44', '53', '72', '85')
# mes_depts <- c('09', '11', '12', '30', '31', '32', '34', '46', '48', '65', '66', '81', '82')

mon_annee <- 2023

# -------------------------------------------------
# organisation des répertoires si nécessaire
# sous-répertoire "rapports_intermediaires"
if (!dir.exists(paths = "rapports_intermediaires")) 
{
  dir.create(path = "rapports_intermediaires")
}

# sous-répertoires par département
for(dept in mes_depts)
  {
if (!dir.exists(paths = paste0("rapports_intermediaires/", dept))) 
    {
  dir.create(path = paste0("rapports_intermediaires/", dept))
    }
  }


# -------------------------------------------------
# prétraitements à la région
rmarkdown::render(
  input = 'scripts/10_regional_data_preprocessing.Rmd',
  output_file = "../rapports_intermediaires/region.docx",
  envir = parent.frame(),
  params = list(mes_depts = mes_depts,
                mon_annee = mon_annee)
)

# prétraitements géographiques
rmarkdown::render(
  input = 'scripts/15_regional_geo_data_preprocessing.Rmd',
  output_file = "../rapports_intermediaires/region_geo.docx",
  envir = parent.frame(),
  params = list(mes_depts = mes_depts,
                mon_annee = mon_annee)
)
# -------------------------------------------------
# production des synthèses par département
for (dept in mes_depts) {
  render_dept(dept = dept,
              annee = mon_annee)
}

# -------------------------------------------------
# production des synthèses par operation

# Chargement des données
load(file = "processed_data/reg_data.RData")
# load(file = "processed_data/dept_data.RData")

df_nommage <- reg_ope %>% 
  filter(annee == mon_annee) %>%
  select(ope_id,
         pop_id,
         pop_libelle,
         dept) %>% 
  distinct() %>% 
  mutate(part = paste(ope_id,
                      pop_libelle,
                      sep = "_"),
         part = paste0("../rapports_intermediaires/",
                       dept,
                       "/",
                       part),
         part = paste0(part, ".pdf"),
         part = stringi::stri_trans_general(str = part,
                                            id = "Latin-ASCII"),
         part = str_to_lower(part),
         part = str_replace_all(part, " ", "_"),
         part = str_replace_all(part, "'", "_"))


# Suppression des points avec une seule opération (fait bugger la fiche opération qui est une synthèse pluri-annuelle).

reg_pops_plus_une_ope <- reg_ope %>% 
  group_by(pop_id) %>% 
  summarise(nb_ope = n_distinct(ope_id)) %>% 
  filter(nb_ope > 1) %>% 
  pull(pop_id)

df_nommage <- df_nommage %>% 
  filter(pop_id %in% reg_pops_plus_une_ope)


# boucle
for (i in (1:nrow(df_nommage))) {
  
  render_ope(ope = df_nommage$ope_id[i],
             nom_fichier_sortie = df_nommage$part[i])
}



# -------------------------------------------------
# Téléchargement, par département, des fiches opérations par défaut ASPE en pdf
# Chaque fichier zip des opérations sur le département pour l'année considéré doit être
# rangé dans le sous-répertoire du département (ex = "rapports_intermediaires/22" pour
# le département des Côtes d'Armor).
# si d'anciens fichiers trainent, il faut les supprimer

# dézippage
for(mon_dept in mes_depts)
  
{ 
  
path <- paste0("rapports_intermediaires/", dept)
mon_zip <- list.files(path = path,
                      pattern = "synthesesOperationsPregenerees.zip$",
                      full.names = TRUE)

unzip(mon_zip, exdir = path)

}

# -------------------------------------------------
# Assemblage des rapports départementaux
for(mon_dept in mes_depts)
  
{

old_names <- list.files(path = paste0("rapports_intermediaires/", mon_dept),
             pattern = "SYNTHESE_OPERATION",
             full.names = TRUE)

new_names <- old_names %>% 
  str_remove_all("SYNTHESE_OPERATION_")

file.rename(from = old_names,
            to = new_names)


pdf_dept <- list.files(path = paste0("rapports_intermediaires/", mon_dept),
                       pattern = "^synthese_.*.pdf",
                       full.names = TRUE)

pdf_intercalaire <- list.files(path = paste0("rapports_intermediaires/"),
                               pattern = "^intercalaire.*.pdf",
                               full.names = TRUE)


pdf_ope <- list.files(path = paste0("rapports_intermediaires/", mon_dept),
                      pattern = "^\\d.*.pdf",
                      full.names = TRUE) %>% 
  sort()

qpdf::pdf_combine(input = c(pdf_dept, pdf_intercalaire, pdf_ope),
                  output = paste0("rapports_finaux/rapport_assemble_",
                  mon_dept,
                  ".pdf"))

}
