#deal with data from TableTidier
#packages----
library(dplyr)
library(readr)
library(descr)
library(stringr)
library(purrr)
library(data.table)
library(tidyverse)
library(readxl)
library(arsenal)
library(stringdist)
#data cleaning and get a list of unique single cui ----
metadata <- read_delim("metadata.csv", 
                        ";", quote = "'", escape_backslash = TRUE, 
                        escape_double = FALSE, trim_ws = TRUE) 
sorted_data <- metadata  %>% arrange(docid,page,concept) ##concept without ";" can be cha name, with can be name and level so here only filter name, some with";" are sub level only, like ";>30" 
char_names <- sorted_data %>% filter(!str_detect(concept,";")) %>% select(concept,cuis_selected) %>% distinct()
## characteristic names with sorted CUIs (1646 unique cui combinations)
char_names <- char_names %>%
  pmap_dfr(function(...) {
    current <- tibble(...)
    current %>% mutate( cuis_selected = cuis_selected %>% strsplit(";") %>% unlist() %>% sort() %>% paste0("",collapse = ";")  ) ##paste0
  }) %>% distinct()
colnames(char_names) <- c( "char_name", "char_name_cuis" ) 
##remove levels after ; , these are also names
data_w_char_names <- sorted_data %>% 
  select (-cuis, -qualifiers, -qualifiers_selected, -user, -istitle, -labeller ) %>% 
  separate(concept, "char_name" ,sep=";" ,remove = FALSE, extra = "drop") %>%  ##remove the contents after ";"  
  left_join(char_names, by = "char_name" ) 
##add a level var named "char_level_cuis", set it as the code appeared in "cuis_selected" while not in "char_name_cuis", otherwise set it as blank
splitNclean <- function (x) {  
  x <- x %>% strsplit(";") %>% unlist() 
  
  x <- x %>% str_trim
  
  x <- x[!(x %in% c("NA",""))]
  
  x <- x %>% unique %>% paste0("",collapse = ";")
  
  x
}

data_w_char_names_level <- data_w_char_names %>%
  pmap_dfr(function(...) {
    current <- tibble(...)
    
    cuis_sel <- splitNclean(current$cuis_selected)
    char_name_cuis <- splitNclean(current$char_name_cuis)
    
    char_level <- cuis_sel[!(cuis_sel %in% char_name_cuis)] %>% sort() %>% paste0("",collapse = ";")
    
    current %>% mutate(cuis_selected = cuis_sel, char_name_cuis = char_name_cuis, char_level_cuis = char_level  ) 
  }) %>% distinct()   
##group by concept and char name, then paste cui_name and cui_level
aggregate_cuis <- data_w_char_names_level %>% select(-docid,-page,-cuis_selected) %>% distinct %>% group_by(concept,char_name) %>%
  summarise(char_name_cuis = paste0(char_name_cuis,"", collapse = ";") %>% splitNclean(), 
            char_level_cuis = paste0(char_level_cuis,"", collapse = ";") %>% splitNclean()
  ) %>% ungroup
aggregate_cuis_agg_concepts <- aggregate_cuis %>% mutate( type = ifelse( str_detect(concept,";"), "level","name" )) %>% 
  select(-char_name) %>% 
  group_by(char_name_cuis,char_level_cuis) %>% 
  summarise(concepts = paste(concept, collapse = " | "), type= paste(type %>% unique(), collapse = ";") )

aggregate_cuis_agg_concepts <- aggregate_cuis_agg_concepts[-1,]
##choose the CUI of char levels when no CUI for char names exist.
aggregate_cuis_agg_concepts <- aggregate_cuis_agg_concepts %>% ungroup() %>% 
  mutate( def_cuis = ifelse( char_name_cuis %>% str_length() > 0, char_name_cuis, char_level_cuis )) %>% 
  select(-char_name_cuis, -char_level_cuis)
aggregate_cuis_agg_concepts <- aggregate_cuis_agg_concepts %>% group_by(def_cuis) %>% 
  summarise(concepts = paste(concepts, collapse = " | "), type= paste(type %>% unique(), collapse = ";") ) 
#get the unique single cuis and then manual harmonise
cuis_index <- read.csv("cuis_index_latest.csv") %>% mutate( cui = str_trim(cui))
singlecui<- aggregate_cuis_agg_concepts %>% 
  mutate(cui=strsplit(def_cuis, ";")) %>%
  unnest() %>%
  distinct(cui) %>% 
  left_join(cuis_index) %>%
  select(1:2) 
#combine metadata with nctid and pmid----
metadata <- metadata %>% mutate ( docid = gsub("[^0-9]", "",  docid) )
nct_id_pmid <- read_csv("nct_id_pmid.csv")
pmid_lkp <- nct_id_pmid %>% filter(reference_type == "reference") %>% select(nct_id, pmid)
pmid_lkp_res_ref <- nct_id_pmid %>% filter(reference_type == "results_reference") %>% select(nct_id, pmid) 
colnames( pmid_lkp ) <- c("nct_id","docid")
colnames( pmid_lkp_res_ref ) <- c("nct_id","docid")
complete_metadata <- metadata %>% 
  inner_join(pmid_lkp)
complete_metadata <- complete_metadata %>% rbind (metadata %>% anti_join(pmid_lkp) %>% distinct %>% inner_join(pmid_lkp_res_ref))
metadata %>% anti_join(pmid_lkp) %>% distinct %>% inner_join(pmid_lkp_res_ref) %>% distinct(docid)
metadata %>% inner_join(pmid_lkp) %>% distinct(docid)
## Missing ones
missing_metadata <- metadata %>% anti_join(complete_metadata %>% distinct(docid,nct_id) )

pubmed_missing_nctids <- read_delim("pubmed_missing_nctids.csv", ";", escape_double = FALSE, trim_ws = TRUE)

missing_nctids_lkp <- pubmed_missing_nctids %>% separate_rows( nct_ids, convert = TRUE)
colnames(missing_nctids_lkp) <- c("docid","nct_id")

missing_nctids_lkp <- missing_nctids_lkp %>% mutate ( docid = as.character(docid))

complete_metadata <- complete_metadata %>% rbind (missing_metadata %>% inner_join(missing_nctids_lkp))

complete_metadata %>% distinct(docid) %>% nrow #625
metadata %>% distinct(docid) %>% nrow #629

final_complete_metadata <- complete_metadata %>% 
  distinct(nct_id, cuis_selected,docid) %>%
  select(nct_id,docid,cuis_selected) %>% 
  filter ( !is.na(cuis_selected) ) 

final_complete_metadata2 <- complete_metadata %>% 
  distinct(nct_id, cuis_selected,docid, page, cuis) %>%
  filter ( !is.na(cuis_selected) ) 
metadata <- final_complete_metadata 
#merge with the manually harmonised single term file----
har_sorted<- read_excel("full_har_terms_sorted.xlsx") %>%
  select(harmonised_cui,ori_cui) %>%
  rename(cuis_selected = ori_cui) 
cuis_term<- read.csv("cuis_index_latest.csv") %>%
  rename(cuis_selected=cui)
everything<- metadata %>% 
  left_join(har_sorted) %>%
  mutate(harmonised_cui=ifelse (is.na(harmonised_cui),cuis_selected,harmonised_cui)) %>%
  select(nct_id,docid,harmonised_cui) %>%
  rename(cuis_selected = harmonised_cui) %>%
  left_join(cuis_term) %>%
  select(nct_id,docid,cuis_selected,preferred) 
# write.csv(everything, "allinone.csv")
#get a unique term list from metadata----
cuis_index <- read.csv("cuis_index_latest.csv") %>% mutate( cui = str_trim(cui))
term<- cuis_index %>%
  rename(cuis_split = cui, termname = preferred)
#tidy combined terms (with 2 more cuis) by splitting into single cui and joining with the harmonised single cui term and then pasting together 
cbd_terms<- everything %>%
  filter(is.na(preferred)) %>%
  group_by(nct_id, docid, cuis_selected) %>%
  distinct() %>%
  mutate(number = group_indices()) %>% #paste the term name together by the cmd cuis_selected
  mutate(cuis_split=strsplit(cuis_selected, ";")) %>%
  unnest(cuis_split) %>%
  left_join(term) %>%
  group_by(number) %>%
  summarise(cbd_term = paste0(termname,",", collapse = " "),
            nct_id = nct_id,
            docid = docid,
            cuis_selected = cuis_selected) %>%
  distinct() %>%
  rename(preferred = cbd_term) %>% #preferred here is the cmd term together
  ungroup() %>%
  select(nct_id,docid,cuis_selected,preferred)
single_terms<- everything %>%
  filter(!is.na(preferred))
#combine single and cmd terms all together 
rd_htmap<- full_join(cbd_terms,single_terms) 
#write.csv(rd_htmap, "prepared_table_for_htmap.csv")
rd_htmap<- read.csv("prepared_table_for_htmap.csv") 
#remove dups for cui and terms and get 2374 unique ones
unique_concept <- rd_htmap %>% 
  select(cuis_selected, preferred) %>%
  distinct() #2374
#write.csv(unique_concept,"test.csv")#2374 uni terms pre manual har
##edit manually and then read 
har_man_table <- read.csv("unique_concept_for_allin_copy.csv",na.strings="")
har_man_table <- har_man_table %>% 
  mutate(final = ifelse(!is.na(X3rd_har), X3rd_har,
                        ifelse(!is.na(X2nd_har), X2nd_har, har_manual))) %>% 
  rename(cuis_selected = cuis_cmd)
#delete some terms with no sense and recycle some after the review----
delete<- har_man_table %>% 
  filter(final == "NA TO BE DELETED") %>%
# mutate(collopse = if_else(cuis_selected %in% c("C0205081", "C5201148", "C4321335", "C2945599", "C0205082", "C0439793", "C3641272", "C4316743", "C045745", "C0457451", "C0521117"), "Severities" , 
#                        if_else(cuis_selected %in% c("C0449238", "C2926735"), "Duration (temporal concept)",
#                                if_else(cuis_selected %in% c("C0332185"), "Recent",
#                                        if_else(cuis_selected %in% c("C0205156", "C0750523", "C3714811", "C1709863", "C2826257", "C0332152", "C0332132", "C0580836", "C0677546", "C1444637"), "Previous", 
#                                                if_else(cuis_selected %in% c("C0521116", "C0150312"), "Current (present time)", 
#                                                        if_else(preferred == "World", "Geographical Locations", 
#                                                                if_else(preferred %in% c("Baseline Severity score", "Moderate (severity modifier) low", "Score Severity score", "Disease Severity score", "Moderate (severity modifier) Severe (severity modifier)", "Very severe Severe (severity modifier) Either"), "Severities", "null")
#                                                        )))))))
  mutate(proceed = if_else(preferred %in% c("year", "Yes - Yes/no indicator", "P-Value","Negative", "Hazard Ratio", "mg/dL", "Median (qualifier value)", "mmHg",	
                                            "% of total", "Numbers", "/hour", "Interquartile Range", 	
                                            "% of total Overall", 	
                                            "/mL", 	
                                            "Amount type - Percentage", 	
                                            "Negative Finding", 	
                                            "millisecond","Overall", "Microgram per Liter", "day", "mL/min", "Total", "Usage", 	
                                            "Total population", 	
                                            "Kilogram", "Statistical mean", "month", "Millimole per Liter", "Gram per Kilogram", "Equal Greater Than", "Per Millimeter", "Less Than Median Statistical Measurement", 
                                            "Less Than mmHg", "Less Than", "milligram/millimole (mg/mmol)", 	
                                            "Kilogram Per Square Meter", "Kilogram Median (qualifier value)", "Kilogram per Cubic Meter", "Quartile", "Cohort", "Quintile", 
                                            "milligram/day", "microgram", "MG", "Pounds", 	
                                            "Millimole per Mole", "Kilogram Per Square Meter", 	
                                            "milligram/day", 	
                                            "Nanogram per Liter", "Unknown", "Study", "Percent (qualifier value)", 	
                                            "Per Minute", 	
                                            "Patients Overall", 	
                                            "Patients Total", "Patients", "Participant", "Negation Use of", "Number of patients",
                                            "Microgram per Kilogram", "Liter per Minute", 	
                                            "Kilogram Per Square Meter", "Full Total population", 	
                                            "Full Population Group", "Full Analysis set (group)", "chi square P-Value"), "NO", "NEXT") )
d_terms_ph<- read.csv("deleted_terms_ph.csv") %>% 
  select(-X)
alldeleted<- delete %>% #alldeleted is all the "NA TO BE DELETED" terms, reviewed by PH and DM and LW again, some terms can be recycled, in line with the ori_data
  left_join(d_terms_ph) %>% 
  select(X, cuis_selected, preferred, proceed, keep, final_keep, standard_term, collopseterm) %>% 
  mutate(final_keep = ifelse(is.na(final_keep), keep, final_keep), 
         final_keep = ifelse(is.na(final_keep), proceed, final_keep), 
         final_keep = ifelse(final_keep == "NO", 0L, final_keep), 
         final_keep = ifelse(final_keep == "NEXT", 1L, final_keep)) 

recycle_delete<- alldeleted %>% 
  filter(final_keep == 1) ##1 means can be recycled

stilldeleted <- alldeleted %>% ##some still can be recycled
  filter(final_keep == 0) ##0, most needs to be deleted but checked a little can still be recycled. 
#write.csv(stilldeleted, "stilldeleted.csv")
#write.csv(recycle_delete, "deleted_man.csv")
recycle_delete<- read.csv("deleted_man.csv", na.strings="") %>% #some contains 2 terms like Exacerbation Previous year were split into 2 rows, so it is 113 obs rather than 102
  select(X, cuis_selected, preferred, standard_term, collopseterm) #standard_terms is not NA, can be reused
stilldeleted<- read.csv("stilldeleted.csv", na.strings = "")%>% 
  select(X, cuis_selected, preferred, standard_term) %>% 
  rename(std = standard_term)
har_man_table<- har_man_table %>% 
  left_join(recycle_delete) %>% 
  left_join(stilldeleted)
har_man_table$final[har_man_table$final == "NA TO BE DELETED"] <- NA

har_man_table<- har_man_table %>% 
  mutate(final= if_else(is.na(final), standard_term, final), ##fill in the harmonised recycled terms in final
         final= if_else(is.na(final), std, final)) %>% 
  select(X, cuis_selected, preferred, final, collopseterm)#collopseterm is colours

#write.csv(har_man_table, "2ndhar_correction.csv")
#then manully check the harmonised terms again, it's the 3rd time har
#tidy terms with count----
har_term_3rd<- read.csv("3rdhar_correction.csv", na.strings="") %>% 
  filter(!is.na(final))
uni_har_term <- har_term_3rd %>% 
  select(X, final) %>% #some final only catch the term without qualifier
  distinct() %>% 
  arrange(X, final) %>% 
  group_by(X) %>%
  mutate(n_terms = seq_along(X), 
         max_terms = length(X)) %>% 
  ungroup() 
twos <- uni_har_term %>% 
  filter(max_terms ==2) %>% 
  spread(n_terms, final, fill = "") %>% 
  arrange(`1`, `2`, X) %>% 
  rename(first = `1`, second = `2`) %>%
  group_by(X) %>% 
  mutate(subgroup = paste0(first, ", ", second)) #from the har_term_3rd to here is to split 2 terms and and re-order them and then bind together, so it could have some duplicates
#write.csv(twos, "twospaste.csv")
complex<- uni_har_term %>% 
  filter(max_terms > 2) %>% 
  select(X, final) %>%
  group_by(X) %>% 
  summarise(final2 = paste(final, collapse=", ")) %>% 
  mutate(man_sub = "complex")
#write.csv(complex,"complex.csv")
single<- uni_har_term %>% 
  filter(max_terms == 1) %>% 
  select(final) %>% #remove indices info X (match with 2374)
  distinct()
twospaste<- read.csv("new_twospaste.csv") %>% 
  select(-max_terms,-first,-second )
newall<- uni_har_term%>% #the 3rd har with seq num and num of max term 
  left_join(har_term_3rd) %>%
  left_join(complex) %>% 
  left_join(twospaste) %>% 
  select(X, cuis_selected, preferred, final, n_terms, max_terms, collopseterm, final2, man_sub, subgroup, man_subgroup, context) %>%
  rename(paste_complex = final2, final_complex = man_sub, paste2 = subgroup, final2= man_subgroup)
newallnocomplex <- newall %>% 
  filter(is.na(final_complex)) %>% 
  mutate(final2 = if_else(!is.na(final2), final2, final)) %>% 
  select(final2) %>% 
  distinct() %>% 
  rename(final = final2) 
#assign closest mesh code for non-complex terms and complex terms----
#firstly assign close mesh for non-complex terms
allterm3<- read.csv("allterm3.csv")
#assign_some_left is the mesh list corrected by David with 180 terms
assign_some_left<- read_excel("assign_close_mesh.xlsx")
assign_left <- assign_some_left %>% 
  rename(closeterm1st = "close term") %>%
  mutate(new_meshcode = if_else(!is.na(new_mesh), new_mesh, #keep the corrected mesh
                                if_else(!is.na(close_mesh_id_david), close_mesh_id_david, close_mesh_id))) %>% 
  mutate(new_termname = if_else(!is.na(new_check), new_check, #keep the corrected term name
                                if_else(!is.na(close_term), close_term, closeterm1st))) %>% 
  mutate(new_atc = if_else(!is.na(new_atc),new_atc, atc)) %>% #keep the corrected atc
  select(final, new_termname, new_meshcode,new_atc,new_note)  
manual_mesh <- read.csv("manual_updated.csv") %>% 
  select(-X, -cuis) %>% 
  rename(preferredterm = final2, mesh = mesh_id)

mesh_tree<- read.csv("mesh_tree_additional.csv") %>%
  rename(mtree = mesh_tree, tree_mesh = mesh_id)
allnocomplex <- newallnocomplex %>% 
  left_join(allterm3) %>% #a general mesh table for newallnocomplex with some that I could not find the closest mesh
  left_join(assign_left) %>% #about 180 assigned mesh
  left_join(manual_mesh) %>% #16
  left_join(mesh_tree) %>% #just to match the tree with their mesh
  mutate(new_termname = if_else(!is.na(new_termname), new_termname, pref_term),#merge the added terms and mesh with allnocomplex
         new_meshcode = if_else(!is.na(new_meshcode), new_meshcode,mesh_id),
         new_atc = if_else(!is.na(new_atc), new_atc,atc),
         new_meshcode = if_else(!is.na(new_meshcode), new_meshcode,mesh),
         new_meshcode = if_else(!is.na(new_meshcode), new_meshcode,tree_mesh)) %>% #preferredterm need to add color identifier
  select(-mesh, -tree_mesh, -mesh_id, -atc)

somemoremesh <- read.csv("mesh_preferred_term_additional.csv") %>%
  rename(new_termname = pref_term)
trd_left<- allnocomplex %>% 
  filter(is.na(new_meshcode)) %>% 
  filter(is.na(new_atc)) %>%
  filter(is.na(mtree)) 

#write.csv(trd_left, "3rd_left.csv")         
third_left <- read.csv("3rd_left.csv") %>% 
  left_join(somemoremesh) %>% 
  mutate(new_meshcode = if_else(!is.na(new_meshcode), new_meshcode, mesh_id)) %>% 
  select(-mesh_id)
#write.csv(third_left, "3rd_left_aftermerge.csv")  #some mesh still need to be added
third_left_manual<- read.csv("3rd_left_aftermerge.csv")
left_3rd <- third_left_manual %>% 
  filter(is.na(new_meshcode)) %>%
  filter(is.na(new_atc))
#write.csv(left_3rd, "left_3rd.csv")#50 left plus some real complex
#manually assign for complex terms
complex_man<- read.csv("complex.csv", na.strings = "") %>% #manually add color and some mesh by harmonising terms and merge 
  rename(new_termname = new_term) %>% 
  left_join(somemoremesh) %>% 
  mutate(new_mesh = if_else(is.na(new_mesh), mesh_id, new_mesh)) %>% 
  select(-mesh_id)
left_term<- left_3rd %>% 
  select(final)
nocomplex_available_mesh <- allnocomplex %>% 
  anti_join(left_term) %>% 
  mutate(preferredterm = if_else(is.na(preferredterm), pref_term, preferredterm)) %>% 
  select(-X.1, -final2,-compressed, -compressed_mesh, -order, -category, -cuis, -cui, -pref_term) %>% 
  mutate(disease_reponse = NA) %>%
  select(X, final, preferredterm, new_termname, new_meshcode, mtree, new_atc, new_note, previous,current,duration,severities,disease_reponse)
com_available_term <- complex_man %>% 
  filter(is.na(man_sub)) %>% 
  rename(final = final2, preferredterm = pref_term, new_meshcode = new_mesh, disease_reponse = disease.reponse) %>% #
  select(-man_sub, -type, -risk) %>% 
  mutate(mtree = NA, new_atc = NA, new_note = NA)
all_available_mesh <- rbind(nocomplex_available_mesh, com_available_term) %>% #these terms have either mesh or mtree or atc
  mutate(family_history = NA,Etiology_aspects = NA, risk = NA, man_sub = NA, type = NA)
#write.csv(all_available_mesh, "all_available.csv")#correct some wider categories, add the mesh code where there only mtree exists
all_avai_edited<- read.csv("all_available.csv")
#next is all unavailable terms 
com_unavailable <- complex_man %>% #after manually fill complex mesh, still some unavailable
  filter(!is.na(man_sub)) %>% 
  rename(final = final2, preferredterm = pref_term, new_meshcode = new_mesh, disease_reponse = disease.reponse) %>% 
  mutate(mtree = NA, new_atc = NA, new_note = NA, family_history = NA,Etiology_aspects = NA) #need to add risk,family_history,Etiology_aspects, type and man_sub in all_available
left_3rd<- left_3rd %>% 
  mutate(man_sub = ifelse(compressed=="1", "complex", ""),
         man_sub = ifelse(is.na(man_sub), "unclear_indicator", man_sub)) %>% 
  select(-X.2, -X.1, -compressed, -compressed_mesh, -order, -category, -cui, -acute) %>% 
  rename(preferredterm = pref_term) %>% 
  mutate(disease_reponse = NA, mtree = NA)
all_unavailable <- rbind(com_unavailable, left_3rd)  

#before rbind, manually add some in all_ava
allterm4<- rbind(all_avai_edited, all_unavailable) # the difference between ava and unava is the man_sub
#write.csv(allterm4, "allterm4.csv")#allterm4 is the unique newall with manual filling of mesh, some are unavail
#merge the uni term list with uni assigned mesh term list (allterm4), to have a mesh for tidied uni term from metadata (about 2798 terms)---- 
allterm4 <- read.csv("allterm4.csv")
#here can filter the complex and unclear indicator, the left are all matched mesh or atc
allwithmesh<- allterm4 %>% 
  filter(is.na(man_sub))

uni_not_sort<- newall %>% #new all is almost the 3rd_har(20 have been removed as dups) with 2798 unique terms
  select(-final) %>%
  mutate(pasted = if_else(is.na(final2), paste_complex, final2)) %>% 
  rename(final = pasted) %>% #final is the pasted har terms 
  filter(is.na(final)) %>% #some unassigned hard term in uni_not_sort can be merged with the 3rd har
  select(X, cuis_selected) %>%
  left_join(har_term_3rd) %>% 
  select(-X) %>%
  left_join(allterm4) %>%#this can both get mesh or atc or man_sub = unclear 
  select(-preferred, -collopseterm)

uni_sorted<- newall %>% 
  select(-final) %>%
  mutate(pasted = if_else(is.na(final2), paste_complex, final2)) %>% 
  rename(final = pasted) %>% 
  select(cuis_selected, final) %>% 
  filter(!is.na(final)) %>% 
  left_join(allterm4) 
##merge all_uni with allterm4 by the pasted terms and then rm dups, then merge all_uni with the ori by cuis_selected and can get the final whole meta data
#rbind uni_sorted and unsorted to get a full list of 2374 (then repeated rows with different terms, might be 2810 rows) unique terms with mesh or atc
alluni_terms<- rbind(uni_sorted, uni_not_sort) %>% 
  select(-X) %>% # 
  distinct()
alluni_terms$previous[alluni_terms$previous=="0"]<- NA
alluni_terms$current[alluni_terms$current=="0"]<- NA
alluni_terms$duration[alluni_terms$duration=="0"]<- NA
alluni_terms$severities[alluni_terms$severities=="0"]<- NA
#merge unique terms with the original metadata by cuis_selected----
metadata2 <- read_delim("metadata.csv", ";", 
                        quote = "'", escape_double = FALSE, trim_ws = TRUE) %>% 
  select(docid, page, concept, cuis_selected, cuis)#add cuis
rd_map<- rd_htmap %>% 
  mutate(docid = as.character(docid)) %>% 
  left_join(metadata2) %>% #merge with the original string catched from tt, rd_htmap from final_complete_meta with the link to nctid, use the original metadata to match the strings, cuis, so here is correct
  select(-page) %>% 
  distinct()
rd_map_withpagecuis<- rd_htmap %>% 
  mutate(docid = as.character(docid)) %>%
  left_join(final_complete_metadata2) %>% #merge with the original string catched from tt
  distinct()
ori_na<- rd_map %>% #these terms were deleted 
  left_join(alluni_terms) %>% 
  filter(is.na(new_meshcode)) %>% 
  filter(is.na(mtree)) %>% 
  filter(is.na(new_atc)) %>% 
  filter(is.na(man_sub)) %>% 
  select(preferred) %>% 
  distinct()

ori_data<- rd_map %>% #rm duplicates and terms alone with no sense (NA TO BE DELETED)
  left_join(alluni_terms) %>% #merge with meshterm and mesh by cuis
  distinct() %>% 
  anti_join(ori_na)#3714 NAs if not remove duplicates
#write.csv(ori_data, "original_with_mesh_2nd.csv")
unclear<- ori_data %>% 
  filter(man_sub== "unclear_indicator")#need to rbind later
#write.csv(unclear, "unclear.csv")#then manual check some of this, and add "not_subgroup" if they are not
unclear <- read.csv("unclear.csv")#not sub or unclassible or complex
ori_clear<- ori_data %>% 
  filter(is.na(man_sub) | !man_sub== "unclear_indicator" ) %>% 
  select(-X.1, -cuis, -X)
#remove some that are not sub
ori<- rbind(ori_clear, unclear) %>% 
  filter(!man_sub %in% c("not_subgroup")) %>% 
  select(-preferredterm, -mtree) %>%
  rename(cuis_caught_from_tt = cuis_selected, cuis_term_name = preferred, original_strings_caught_from_tt = concept, harmonised_term_for_cuis = final, mesh_term = new_termname, mesh_code = new_meshcode, atc_code = new_atc, note_from_paper = new_note, note= man_sub) %>% 
  mutate(mesh_term = ifelse(note %in%c("complex"), NA, mesh_term)) 
#deal with some complex terms 
ori_complex <- ori %>% 
  filter(note == "complex") 
ori_nocomplex<- ori %>% 
  filter(!note %in% c("complex"))
# #write.csv(ori_complex, "ori_complex.csv") #manual edited
ori_complex2 <- read.csv("ori_complex.csv") #%>% select(1:5) %>% mutate(id = 1)
ori_2nd<- rbind(ori_nocomplex, ori_complex2) %>% 
  mutate(note_from_paper = ifelse(note_from_paper %in% c("a lot", "a lot, need another look", "download paper need to send to David", "download the paper"), NA, note_from_paper))
#a little modification
ori_2nd$mesh_term[ori_2nd$harmonised_term_for_cuis == "Anticholinergic Agents, Duration"] <- "Cholinergic Antagonists"
ori_2nd$mesh_term[ori_2nd$mesh_term == "Antidiabetics"] <- "Hypoglycemic Agents"
ori_2nd$mesh_term[ori_2nd$mesh_term == "Antiplatelet Agents"] <- "Platelet Aggregation Inhibitors"
ori_2nd$mesh_term[ori_2nd$mesh_term == "Hip structure"] <- "Hip"
ori_2nd$severities[ori_2nd$harmonised_term_for_cuis == "Bleeding risk"] <- NA #correct bleeding risk
ori_2nd$risk[ori_2nd$harmonised_term_for_cuis == "Bleeding risk"] <- 1
#after some manual editions, read in the 4th version----
prev<- paste(c("previous", "prior", "medical history", "past", "before"), collapse = "|")
ori_4th<- read.csv("ori_data_3rd.csv") %>% 
  mutate(risk = if_else(str_detect(original_strings_caught_from_tt, "risk") | str_detect(cuis_term_name, "Risk") | str_detect(harmonised_term_for_cuis, "Risk"), 1L, risk)) %>% 
  mutate(previous = if_else(str_detect(original_strings_caught_from_tt, prev) | str_detect(cuis_term_name, prev) | str_detect(harmonised_term_for_cuis, prev), 1L, previous)) %>% 
  mutate(duration = if_else(str_detect(original_strings_caught_from_tt, "duration") | str_detect(cuis_term_name, "Duration") | str_detect(harmonised_term_for_cuis, "Duration"), 1L, duration)) %>%
  mutate(type = if_else(str_detect(original_strings_caught_from_tt, "type") | str_detect(cuis_term_name, "Type") | str_detect(harmonised_term_for_cuis, "Type"), 1L, type)) %>%
  select(-cuis_caught_from_tt)
#write.csv(ori_4th, "ori_4th.csv")
ori_4th_edited<- read.csv("ori_4th.csv") %>% 
  mutate(severities = if_else(str_detect(original_strings_caught_from_tt, "severities") | str_detect(cuis_term_name, "Severities") | str_detect(harmonised_term_for_cuis, "Severities"), 1L, severities)) %>% 
  mutate(note = if_else(harmonised_term_for_cuis == "complex", "complex", note)) %>% 
  mutate(risk = if_else(str_detect(original_strings_caught_from_tt, "risk") | str_detect(cuis_term_name, "Risk") | str_detect(harmonised_term_for_cuis, "Risk"), 1L, risk)) 
#manually check the original strings match with mesh (as some auto captured cuis based on the strings were not correct)----
ori4th_not_done <- ori_4th_edited %>%
  slice(2601:9596)#the 1st 2600 have been checked
ori4th_not_done_dstnct <- ori4th_not_done %>%
  group_by(original_strings_caught_from_tt, cuis_caught_from_tt, cuis_term_name, harmonised_term_for_cuis, mesh_term,
           mesh_code, atc_code, note_from_paper, previous,
           current, duration, severities, disease_reponse, family_history, Etiology_aspects, risk, type, location, note) %>%
  summarise(nct_id = unique(nct_id) %>% paste(collapse = ";"),
            docid = unique(docid) %>% paste(collapse = ";")) %>%
  ungroup() %>%
  mutate(rejoinid = seq_along(mesh_term))
#write_csv(ori4th_not_done_dstnct, "david_lili_compact_pre_manual.csv")
ori4th_done_check <- ori_4th_edited %>%
  slice(1:2600)
ori4th_done_check_dstnct <- ori4th_done_check%>%
  group_by(original_strings_caught_from_tt, cuis_caught_from_tt, cuis_term_name, harmonised_term_for_cuis, mesh_term,
           mesh_code, atc_code, note_from_paper, previous,
           current, duration, severities, disease_reponse, family_history, Etiology_aspects, risk, type, location, note) %>%
  summarise(nct_id = unique(nct_id) %>% paste(collapse = ";"),
            docid = unique(docid) %>% paste(collapse = ";")) %>%
  ungroup() %>%
  mutate(rejoinid = seq_along(mesh_term))
#write_csv(ori4th_done_check_dstnct, "2600check_pre_manual.csv")
left_post_done <- read.csv("david_lili_compact_post_manual.csv")%>% 
  ungroup() 

done_2600_id<- read.csv("2600check_post_manual.csv") %>% 
  ungroup() 
post_all_distinct <- rbind(done_2600_id, left_post_done) #3786
post_all_distinct$original_strings_caught_from_tt[post_all_distinct$original_strings_caught_from_tt == "index diagnosis;ÃŸ-blocker"] <- "index diagnosis;ß-blocker"
#write_csv(post_all_distinct, "corrected_distinct.csv") 
post_all_distinct2 <- read.csv("corrected_distinct.csv")
post_all_distinct2$original_strings_caught_from_tt[post_all_distinct2$original_strings_caught_from_tt == "index diagnosis;ÃŸ-blocker"] <- "index diagnosis;ß-blocker"
post_all_distinct2 <- post_all_distinct2 %>%
  select(-rejoinid) %>%
  arrange(nct_id, docid, original_strings_caught_from_tt)
ori_4th_edited_distinct<- ori_4th_edited %>%
  group_by(original_strings_caught_from_tt, cuis_caught_from_tt, cuis_term_name, harmonised_term_for_cuis, mesh_term, 
           mesh_code, atc_code, note_from_paper, previous, 
           current, duration, severities, disease_reponse, family_history, Etiology_aspects, risk, type, location, note) %>% 
  summarise(nct_id = unique(nct_id) %>% paste(collapse = ";"),
            docid = unique(docid) %>% paste(collapse = ";")) %>% 
  ungroup() %>% 
  mutate(rejoinid = seq_along(mesh_term)) %>% 
  arrange(nct_id, docid, original_strings_caught_from_tt)
somecolumn_4th_ori<- ori_4th_edited_distinct %>% 
  select(original_strings_caught_from_tt, nct_id,docid, rejoinid) %>%
  rename(ori_strings = original_strings_caught_from_tt, nct_ori = nct_id, doc_ori = docid)
corrected_dis_merge<- cbind(somecolumn_4th_ori, post_all_distinct2) %>% 
  select(ori_strings,original_strings_caught_from_tt,nct_ori, nct_id,doc_ori, docid, everything())%>% 
  mutate(same_str_nct_docid = if_else(doc_ori == docid & nct_ori == nct_id & ori_strings == original_strings_caught_from_tt, 1L, 0L),#some 0 here may due to that reading the strings in excel and then into R, some special characters changed
         same_nct_docid = if_else(doc_ori == docid & nct_ori == nct_id, 1L, 0L)) %>% #this is to see the nct and docid for 92 NAs of strings
  select(ori_strings,original_strings_caught_from_tt,same_str_nct_docid, same_nct_docid, nct_ori, nct_id, doc_ori, docid, everything())
freq(corrected_dis_merge$same_str_nct_docid)
freq(corrected_dis_merge$same_nct_docid)
#fill the missing ori strings
miss_ori_str<- corrected_dis_merge %>% 
  filter(is.na(ori_strings))
no_miss <- corrected_dis_merge %>% 
  filter(!is.na(ori_strings))
#write_csv(miss_ori_str,"blank_str.csv")#then manually go through 
miss_ori_str_filled <- read.csv("blank_str.csv")
corrected_dis_merge<- rbind(miss_ori_str_filled, no_miss) 
##merge original data with its distinct by all common vars 
ori_4th_edited2 <- ori_4th_edited %>% 
  inner_join(ori_4th_edited_distinct %>% rename(docids = docid, nct_ids = nct_id))
##join together full set, by = c("original_strings_caught_from_tt", "nct_ids", "docids", "rejoinid")
ori_4th_edited3 <- ori_4th_edited2 %>%
  select(1:3, nct_ids:rejoinid) %>%
  inner_join(corrected_dis_merge %>%
               rename(str_excel=original_strings_caught_from_tt) %>% #-strings from excel with wrong symbols
               rename(original_strings_caught_from_tt = ori_strings, docids = docid, nct_ids = nct_id)) %>% 
  mutate(original_strings_caught_from_tt = if_else(is.na(original_strings_caught_from_tt), str_excel, original_strings_caught_from_tt)) %>% 
  select(-str_excel)
ori_5th<- ori_4th_edited3
##double check colors
previous_color<- paste(c("previous","Previous","prior","Prior", "medical history","Medical history", "past", "Past", "before","Before"), collapse = "|")
risk_color <- paste(c("risk", "Risk"), collapse = "|")
severity_color <- paste(c("severities", "Severities", "severity", "Severity", "exacerbation", "Exacerbation", "Class", "class", "Score", "score", "classification", "Classification"), collapse = "|")
type_color <- paste(c("type", "Type"), collapse = "|")
location_color <- paste(c("site", "Site"), collapse = "|") #location can be geographic
duration_color <- paste(c("duration", "Duration"), collapse = "|")
current_color <- paste(c("Current", "current"), collapse = "|")
ori_5th_color<- ori_5th %>% 
  mutate(risk = if_else(str_detect(original_strings_caught_from_tt, risk_color) | str_detect(cuis_term_name, risk_color) | str_detect(harmonised_term_for_cuis, risk_color), 1L, risk)) %>% 
  mutate(previous = if_else(str_detect(original_strings_caught_from_tt, previous_color) | str_detect(cuis_term_name, previous_color) | str_detect(harmonised_term_for_cuis, previous_color), 1L, previous)) %>% 
  mutate(duration = if_else(str_detect(original_strings_caught_from_tt, duration_color) | str_detect(cuis_term_name, duration_color) | str_detect(harmonised_term_for_cuis, duration_color), 1L, duration)) %>%
  mutate(type = if_else(str_detect(original_strings_caught_from_tt, type_color) | str_detect(cuis_term_name, type_color) | str_detect(harmonised_term_for_cuis, type_color), 1L, type)) %>%
  mutate(location = if_else(str_detect(original_strings_caught_from_tt, location_color) | str_detect(cuis_term_name, location_color) | str_detect(harmonised_term_for_cuis, location_color), 1L, location)) %>%
  mutate(current = if_else(str_detect(original_strings_caught_from_tt, current_color) | str_detect(cuis_term_name, current_color) | str_detect(harmonised_term_for_cuis, current_color), 1L, current)) %>%
  mutate(severities = if_else(str_detect(original_strings_caught_from_tt, severity_color) | str_detect(cuis_term_name, severity_color) | str_detect(harmonised_term_for_cuis, severity_color), 1L, severities)) 
#write_csv(ori_5th_color, "ori_5th_data.csv")
##check the differences between 2nd and 5th version, see the missing terms
ori_5th_color <-read.csv("ori_5th_data.csv")
ori2nd_vs_5th_diff<- ori_2nd %>% 
  select(1:9) %>%
  mutate(docid = as.integer(docid)) %>% 
  anti_join(ori_5th_color %>% select(nct_id, docid, original_strings_caught_from_tt)) 
#write_csv(ori2nd_vs_5th_diff, "diff_in2nd_vs_5th.csv")
##corrected this 90 strings and read
ori2vs5_post<- read.csv("diff_in2nd_vs_5th.csv")
ori_6th<- ori_5th_color %>% 
  select(-nct_ids, -docids, -rejoinid, -same_str_nct_docid, -same_nct_docid, -nct_ori, -doc_ori) %>% 
  rbind(ori2vs5_post)
ori_6th_with_color<- ori_6th %>% 
  mutate(risk = if_else(str_detect(original_strings_caught_from_tt, risk_color) | str_detect(cuis_term_name, risk_color) | str_detect(harmonised_term_for_cuis, risk_color), 1L, risk)) %>% 
  mutate(previous = if_else(str_detect(original_strings_caught_from_tt, previous_color) | str_detect(cuis_term_name, previous_color) | str_detect(harmonised_term_for_cuis, previous_color), 1L, previous)) %>% 
  mutate(duration = if_else(str_detect(original_strings_caught_from_tt, duration_color) | str_detect(cuis_term_name, duration_color) | str_detect(harmonised_term_for_cuis, duration_color), 1L, duration)) %>%
  mutate(type = if_else(str_detect(original_strings_caught_from_tt, type_color) | str_detect(cuis_term_name, type_color) | str_detect(harmonised_term_for_cuis, type_color), 1L, type)) %>%
  mutate(location = if_else(str_detect(original_strings_caught_from_tt, location_color) | str_detect(cuis_term_name, location_color) | str_detect(harmonised_term_for_cuis, location_color), 1L, location)) %>%
  mutate(current = if_else(str_detect(original_strings_caught_from_tt, current_color) | str_detect(cuis_term_name, current_color) | str_detect(harmonised_term_for_cuis, current_color), 1L, current)) %>%
  mutate(severities = if_else(str_detect(original_strings_caught_from_tt, severity_color) | str_detect(cuis_term_name, severity_color) | str_detect(harmonised_term_for_cuis, severity_color), 1L, severities)) %>% 
  filter(is.na(note) | !note == "not subgroup")
thrombosis <- paste(c("Thrombosis", "Venous Thrombosis", "Venous Thromboembolism", "Thromboembolism"), collapse = "|")
throm_meshcode<- paste(c("D013927", "D020246", "D054556", "D013923", "D016770"), collapse = "|") 
ori_6th_with_color$mesh_term[str_detect(ori_6th_with_color$mesh_term, thrombosis)]<- "Embolism and Thrombosis"
ori_6th_with_color$mesh_code[str_detect(ori_6th_with_color$mesh_code, throm_meshcode)]<- "D016769"

ori_6th2<- ori_6th_with_color %>% 
  mutate(atc_code = ifelse(is.na(mesh_code), atc_code, NA),
         mesh_term = if_else(!is.na(note), note, mesh_term))
ori_6th2$mesh_term[ori_6th2$note_from_paper == "not give the full term of TTR"]<- "unclassifiable"
#write_csv(ori_6th2, "ori_6th.csv") #19/10, filled with all missing strs
#heatmap preparation----
#link nct with mesh_broad
nct_mesh<- readRDS("conditions_lkp.rds") %>% 
  select(nct_id, mesh_broad, mesh_broad_label) %>%
  group_by(nct_id) %>% 
  slice(1) ##pick the first nct, no duplicates 
ori_data<- ori_6th2 %>% 
  left_join(nct_mesh) %>% 
  arrange(mesh_broad, nct_id) %>% 
  select(nct_id, docid, mesh_broad, mesh_broad_label, everything())
#write_csv(ori_data, "ori_7th_with_mesh_broad.csv")
#try to link page below----
df7<- read.csv("ori_7th_with_mesh_broad.csv") %>% 
  rename(cuis_selected = cuis_caught_from_tt) %>% 
  mutate(id = 1:9647,
         cuis2 = cuis_selected) 

final_complete_metadata2<- read.csv("final_complete_metadata2.csv")
df7_page <- df7 %>% 
  left_join(final_complete_metadata2) %>% 
  select(-mesh_broad, -mesh_broad_label) %>%
  distinct() %>% 
  select(nct_id, docid, page, cuis, cuis_selected, everything()) %>% 
  #  select(-page, -cuis) %>% 
  distinct()
changed <- df7_page %>% 
  filter(is.na(page)) %>% 
  select(-nct_id, -id) %>%
  distinct()
#write.csv(changed, "no_page.csv")

df7_page_matched<- df7_page %>% 
  filter(!is.na(page)) %>% 
  select(-nct_id, -id) %>%
  distinct() %>% 
  select(-cuis2, -X, -cuis) %>% 
  mutate(pagehar_term_3rd = 1)
#add page number (see fuzzy matching)----
addpage<- read.csv("addpage.csv") %>% 
  select(-X, -pmid)
df7_page_matched <- df7_page_matched %>%
  rename(pageyesno = pagehar_term_3rd)
ori_8th_with_page <- rbind(df7_page_matched, addpage) %>% distinct()#merging my ori_7th with the original metadata, addpage is by fuzzy match + manual added
ori_8th_with_page$pageyesno[ori_8th_with_page$pageyesno == 100 ] <- 1
ori_8th<- ori_8th_with_page %>% 
  arrange(docid,page, original_strings_caught_from_tt, pageyesno) %>% 
  select(docid,page, original_strings_caught_from_tt, pageyesno, everything())

ori8_ck_withpage <- ori_8th %>% 
  filter(pageyesno == 1) 
ori8_ck_nopage<- ori_8th %>% 
  filter(pageyesno == 0) %>% #filter no page
  select(-page, -pageyesno) %>% 
  left_join(ori8_ck_withpage) %>% 
  distinct()
#small number not found, manual check again
small_no_page<- ori8_ck_nopage %>% 
  filter(is.na(page))
small_no_page_from_the_begin<- ori_8th %>% 
  filter(is.na(page))
sm_no_page_total <- rbind(small_no_page,small_no_page_from_the_begin) %>% 
  arrange(docid) %>% 
  select(docid, original_strings_caught_from_tt, page, everything())
#write.csv(sm_no_page_total, "still_nopage.csv")
add_2nd_page<- read.csv("still_nopage.csv") %>% #need to merge back with ori_8th
  filter(!page %in% c("none")) %>% 
  mutate(pageyesno = 1)
ori_8th$page <- as.character(ori_8th$page)
ori_8<- ori_8th %>% 
  filter(!is.na(page)) %>% 
  filter(pageyesno == 1)
ori_8th_final_with_page <- rbind(add_2nd_page, ori_8)
#write.csv(ori_8th_final_with_page, "ori_8th_final_with_page.csv")
df_8th<- read_csv("ori_8th_final_with_page.csv") %>% 
  mutate(docid = as.character(docid)) %>% 
  select(-pageyesno)
#write_rds(df_8th, "ori_8th.rds")
df_8 <- df_8th %>%
  mutate(mesh_term2 = if_else(mesh_term == "Unified Parkinson's Disease Rating Scale", "Mental Status and Dementia Tests",
                              if_else(mesh_term == "Rheumatoid Arthritis", "Arthritis, Rheumatoid",
                                      if_else(mesh_term =="Transient Ischemic Attack", "Ischemic Attack, Transient",
                                              if_else(mesh_term == "Ulcerative Colitis", "Colitis, Ulcerative",
                                                      if_else(mesh_term== "Valvular heart disease", "Heart Valve Diseases",
                                                              if_else(mesh_term== "Electrocardiogram: left ventricle hypertrophy (finding)", "Hypertrophy, Left Ventricular",
                                                                      if_else(mesh_term== "Impaired fasting glycaemia", "Glucose Intolerance",
                                                                              if_else(mesh_term== "Antirheumatic Drugs, Disease-Modifying", "Antirheumatic Agents",
                                                                                      if_else(mesh_term== "Peripheral Arterial Diseases", "Peripheral Arterial Disease", mesh_term)))))))))) %>%
  mutate(mesh_code2 = if_else(mesh_term %in% c("complex", "unclassifiable"), "none", mesh_code)) %>% 
  select(docid, page, original_strings_caught_from_tt, cuis_selected, cuis_term_name, harmonised_term_for_cuis, mesh_term, mesh_code, mesh_term2, mesh_code2, everything() )

df<- df_8 %>%
  distinct(mesh_term2, mesh_code2) %>%
  group_by(mesh_code2) %>%
  mutate(n = length(mesh_term2)) %>%
  ungroup() %>%
  filter(n==2)
#write_rds(df_8, "ori_8th.rds")
#write_csv(df_8, "ori_9_sometermsneedtobechanged.csv")
#65 changes have been made after clinical review, then it became ori_9th.rds
miss<- readRDS("trials_wout_mesh.Rds") %>%
  mutate(id = 1:689)
ori_9th<- readRDS("ori_9th.rds")
hmnsd<- ori_9th %>% 
  select(cuis_selected:atc_code) %>% 
  distinct() %>% 
  group_by(cuis_term_name) %>% 
  slice(1) %>% 
  ungroup() %>% 
  group_by(cuis_selected) %>% 
  slice(1) %>% 
  ungroup()
fill1 <- miss %>% 
  filter(!is.na(cuis_selected)) %>% 
  filter(!cuis_selected %in% c("null")) %>% 
  left_join(hmnsd) %>% 
  filter(!is.na(cuis_term_name)) %>% 
  select(nct_id, docid, val, concept_source, concept_root, concept, cuis_term_name, harmonised_term_for_cuis, mesh_term2, mesh_code2, everything())%>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup()
left<- miss %>%
  anti_join(fill1)
stringsin9<- ori_9th %>% 
  select(original_strings_caught_from_tt:atc_code, -cuis_selected) %>% 
  distinct() %>% 
  rename(concept_source = original_strings_caught_from_tt) # merge by concept_source
fill2<- left %>% 
  left_join(stringsin9) %>% 
  filter(!is.na(cuis_term_name)) %>% 
  group_by(id) %>% 
  slice(1) %>% 
  ungroup()
left2<- left %>% 
  anti_join(fill2)
str92<- stringsin9 %>% 
  rename(concept_root = concept_source)
fill3 <- left2 %>% 
  left_join(str92) %>% 
  filter(!is.na(cuis_term_name)) %>%
  group_by(id) %>% 
  slice(1) %>% 
  ungroup()
left3<- left2 %>% 
  anti_join(fill3, by = "id") %>% 
  mutate(concept_source = if_else(is.na(concept_source), val, concept_source))
fill4<- left3 %>% 
  mutate(har = if_else(str_detect(concept_source, "male|men$|women|gender|sex"), "Gender", "notyet")) %>%
  filter(!har == "notyet")
left4<- left3 %>% 
  anti_join(fill4, by = "id")
fill5<- left4 %>% 
  mutate(har =  if_else(str_detect(concept_source, "diabetes|dm"), "Diabetes Mellitus", 
                        if_else(str_detect(concept_source, "^age"), "Age-Years",
                                if_else(str_detect(concept_source, "country|america$|europe|region|united states|location|japan"), "Geographic Locations",
                                        if_else(str_detect(concept_source, "smok|cigarette"), "Smoking Status", if_else(str_detect(concept_source, "white$|black|american|asian|race|hispanic"), "Racial group",
                                                                                                                        if_else(str_detect(concept_root, "cholesterol|ldl|hdl|triglycerides|hdl-c"), "lipoprotein cholesterol", "notyet"
                                                                                                                        ))))
                        ))) %>% 
  filter(!har == "notyet")
left5<- left4 %>% 
  anti_join(fill5, by = "id")
fill6<- left5 %>% 
  mutate(har = if_else(str_detect(concept_source, "bmi|body mass index"), "Body mass index", 
                       if_else(str_detect(concept_source, "egfr"), "Glomerular Filtration Rate", 
                               if_else(str_detect(concept_source, "a1c"), "Hemoglobin, Glycosylated", 
                                       if_else(str_detect(concept_source, "weight"), "Body Weight", 
                                               if_else(str_detect(concept_source, "apolipoprotein"), "Apolipoprotein A-I",
                                                       if_else(str_detect(concept_source, "mi$|myocardial infarction"), "Myocardial Infarction", "notyet")
                                               )))))) %>% 
  filter(!har == "notyet")
left6 <- left5 %>% 
  anti_join(fill6, by = "id")
fill7<- left6 %>% 
  mutate(har = if_else(str_detect(concept_source, "crp"), "C-reactive protein", 
                       if_else(str_detect(concept_source, "clopidogrel"), "Antithrombotic Agents", 
                               if_else(str_detect(concept_source, "^ace"), "Angiotensin-Converting Enzyme Inhibitors", 
                                       if_else(str_detect(concept_source, "atorvastatin"), "atorvastatin", 
                                               if_else(str_detect(concept_source, "statin"), "Hydroxymethylglutaryl-CoA Reductase Inhibitors", 
                                                       if_else(str_detect(concept_source, "infliximab"), "Immunosuppressive Agents", 
                                                               if_else(str_detect(concept_source, "pci"), "Percutaneous Coronary Intervention", 
                                                                       if_else(str_detect(concept_source, "hypertension"), "Hypertension, CTCAE", 
                                                                               if_else(str_detect(concept_source, "ocs"), "Anti-Inflammatory Agents", "notyet")))))))))) %>% 
  filter(!har == "notyet")
left7 <- left6 %>% 
  anti_join(fill7, by = "id")
fill1to3<- rbind(fill1, fill2, fill3)         

fill4to7<- rbind(fill4, fill5, fill6, fill7) %>% 
  rename(harmonised_term_for_cuis = har) 
str93<- str92 %>% 
  select(-concept_root) %>% 
  group_by(harmonised_term_for_cuis) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(harmonised_term_for_cuis = as.character(harmonised_term_for_cuis))
f4to7 <- fill4to7 %>%
  left_join(str93, by = "harmonised_term_for_cuis")
f1to7<- rbind(fill1to3, f4to7)

left7<- left7 %>% 
  mutate(har = "") %>%
  select(nct_id, docid,page, concept_source:concept,har, everything())
#write.csv(left7,"left_needtobefilled.csv" )
manual_fill <- read.csv("left_needtobefilled.csv") %>% 
  rename(harmonised_term_for_cuis = har) %>% 
  select(id, harmonised_term_for_cuis, severity, note_inpaper) %>% 
  left_join(str93, by = "harmonised_term_for_cuis")  
man_fill_failed<- manual_fill %>% 
  filter(is.na(mesh_term)) %>%
  group_by(harmonised_term_for_cuis) %>%
  mutate(id2 = group_indices())  %>%
  ungroup()
man_notfail<- manual_fill %>% 
  anti_join(man_fill_failed, by = "id")
uni_fail <- man_fill_failed %>%
  select(id2, harmonised_term_for_cuis, mesh_code) %>% 
  unique()
#write.csv(uni_fail, "uni_fail.csv")
str94<- str93 %>% 
  group_by(mesh_code2) %>% 
  slice(1) %>% 
  ungroup() %>% 
  filter(!is.na(mesh_code2))
fail_filled<- read.csv("uni_fail.csv")
man_fill_failed2 <- man_fill_failed %>%
  select(-mesh_code2) %>%
  left_join(fail_filled , by = "id2") %>% 
  select(id, severity, note_inpaper, id2, har2, mesh_code2) %>% 
  left_join(str94, by = "mesh_code2") %>% 
  select(-harmonised_term_for_cuis, -id2) %>% 
  rename(harmonised_term_for_cuis = har2)
man_fill_done<- rbind(man_notfail, man_fill_failed2)
f1to7 <- f1to7 %>% 
  mutate(severity = "",
         note_inpaper = "")

left_manual<- left7 %>% 
  left_join(man_fill_done, by = "id") %>%
  mutate(note_inpaper = as.character(note_inpaper), mesh_term2 = as.character(mesh_term2), mesh_code2= as.character(mesh_code2)) %>%
  mutate(note_inpaper = if_else(is.na(harmonised_term_for_cuis)|id %in% c("189", "331", "332"), "not_a_subgroup", note_inpaper)) %>% 
  select(-har) %>% 
  mutate()
fill_all<- rbind(f1to7, left_manual) %>% 
  filter(!note_inpaper == "not_a_subgroup") %>% 
  mutate(harmonised_term_for_cuis = as.character(harmonised_term_for_cuis), mesh_term2 = as.character(mesh_term2), mesh_code2= as.character(mesh_code2),atc_code = as.character(atc_code),
         mesh_term2 = if_else(is.na(mesh_term2), harmonised_term_for_cuis, mesh_term2),
         mesh_code2 = if_else(mesh_term2 == "unclassifiable", "none", mesh_code2),
         atc_code = if_else(mesh_term2 == "corticosteroids", "S02B", atc_code),
         atc_code = if_else(str_detect(mesh_term2, "beta-Adrenergic"), "C07", atc_code))
fill_all$mesh_term2[fill_all$atc_code == "C07"]<- "BETA BLOCKING AGENTS"


fill_to_ori<- fill_all %>% 
  rename(original_strings_caught_from_tt = concept_source, note = note_inpaper) %>% # use string to replace c_source
  select(-val, -concept_root, -concept, -tid, -cui, -lbl, -qualifiers_selected, -id) %>% 
  mutate(risk = if_else(str_detect(original_strings_caught_from_tt, "risk|Risk") | str_detect(note, "risk|Risk") | str_detect(harmonised_term_for_cuis, "risk|Risk"), 1L, 0L)) %>%
  mutate(previous = if_else(str_detect(original_strings_caught_from_tt, "previous|prior|history|former") | str_detect(note, "previous|prior|history|former") | str_detect(harmonised_term_for_cuis, "previous|prior|history|former"), 1L, 0L),
         family_history = if_else(str_detect(original_strings_caught_from_tt, "family") , 1L, 0L),
         previous = if_else(family_history== "1", 0L, previous)) %>%
  mutate(duration = if_else(str_detect(original_strings_caught_from_tt, "duration") | str_detect(note, "duration") | str_detect(harmonised_term_for_cuis, "duration"), 1L, 0L)) %>%
  mutate(type = if_else(str_detect(original_strings_caught_from_tt, "type") | str_detect(note, "type") | str_detect(harmonised_term_for_cuis, "type"), 1L, 0L)) %>%
  mutate(current = if_else(str_detect(original_strings_caught_from_tt, "current") | str_detect(note, "current") | str_detect(harmonised_term_for_cuis, "current"), 1L, 0L)) %>%
  mutate(severities = if_else(str_detect(original_strings_caught_from_tt, "severity|class|Severities|extent|Class") | str_detect(note, "severity|class|Severities|extent|Class") | str_detect(harmonised_term_for_cuis, "severity|class|Severities|extent|Class")| severity == "1", 1L,0L),
         severities = if_else(is.na(severities), 0L, severities))%>%
  mutate(Etiology_aspects = if_else(str_detect(original_strings_caught_from_tt, "etiology") | str_detect(note, "etiology") | str_detect(harmonised_term_for_cuis, "severity"), 1L, 0L)) %>% 
  select(-severity, -nct_id, -pmid) %>% 
  rename(note_from_paper = note) %>%
  mutate(risk_score = 0L, location = 0L,disease_reponse = 0L,
         note = if_else(note_from_paper == "unclassifiable", note_from_paper, "")) 

ori_10th<- rbind(ori_9th, fill_to_ori)
#write.csv(ori_10th, "ori_10th.csv")
ori_10th2<- read.csv("ori_10th.csv") %>% select(-X)
#write_rds(ori_10th2, "ori_10th.rds")
#ori_10 to ori_11
#incorporate changes after clinical review
ori_11<- read.csv("ori_11th.csv")
ori11_noncomplex<- ori_11 %>% 
  filter(!mesh_term2 == "complex")
ori11_complex <- ori_11 %>% 
  anti_join(ori11_noncomplex)

ori11_noncomplex2<- ori11_noncomplex %>% 
  mutate(mesh_term2 = as.character(mesh_term2), mesh_term = as.character(mesh_term), mesh_code2 = as.character(mesh_code2)) %>%
  mutate(mesh_term2 = if_else(str_detect(original_strings_caught_from_tt, "corticosteroid") | str_detect(cuis_term_name, "corticosteroid") | str_detect(harmonised_term_for_cuis, "corticosteroid"), "Adrenal Cortex Hormones", mesh_term2)) %>% 
  mutate(mesh_term2 = if_else(mesh_term == "Immunosuppressive Agents" | mesh_term == "Immunotherapy", mesh_term, mesh_term2),
         mesh_term2 = if_else(str_detect(original_strings_caught_from_tt, "^ics|;ics|ICS"), "Adrenal Cortex Hormones", mesh_term2),
         mesh_term2 = if_else(mesh_term2 == "Steroids", "Adrenal Cortex Hormones", mesh_term2),
         #        mesh_term2 = if_else(str_detect(original_strings_caught_from_tt, "^steroid" | str_detect(cuis_term_name, "^steroid"), "Steroids", mesh_term2)))#some needs to be changed back to steroid manually
         mesh_code2 = if_else(mesh_term2 == "Adrenal Cortex Hormones", "D000305", mesh_code2)) %>% 
  mutate(mesh_term2 = if_else(str_detect(cuis_term_name, "Amiodarone"), "Amiodarone", mesh_term2), 
         mesh_code2 = if_else(mesh_term2 == "Amiodarone", "D000638", mesh_code2)) %>% #atc change into NA
  mutate(mesh_term2 = if_else(mesh_term2 == "Eosinophils", "Eosinophilia", mesh_term2),
         mesh_code2 = if_else(mesh_term2 == "Eosinophilia", "D004802", mesh_code2)) %>% 
  mutate(mesh_term2 = if_else(str_detect(cuis_term_name, "adalimumab|infliximab|Etanercept") | str_detect(original_strings_caught_from_tt, "infliximab") | str_detect(mesh_term2, "Tumor necrosis|Tumor Necrosis"), "Tumor Necrosis Factor Inhibitors", mesh_term2),
         mesh_code2 = if_else(mesh_term2 == "Tumor Necrosis Factor Inhibitors", "D000079424", mesh_code2),
         mesh_term2 = if_else(mesh_term2 == "Peripheral Arterial Disease", "Peripheral Vascular Diseases", mesh_term2),
         mesh_code2 = if_else(mesh_term2 == "Peripheral Vascular Diseases", "D016491", mesh_code2)) %>% #atc change into NA
  mutate(mesh_term2 = if_else(str_detect(mesh_term2, "Embolism and Thrombosis|Thromboembolism"), "Venous Thromboembolism", mesh_term2),
         mesh_code2 = if_else(mesh_term2 == "Venous Thromboembolism", "D054556", mesh_code2),
         mesh_term2 = if_else(mesh_term2 == "Hyperlipidemias", "Hypercholesterolemia", mesh_term2),
         mesh_code2 = if_else(mesh_term2 == "Hypercholesterolemia", "D006937", mesh_code2))
ori_11_pre<- rbind(ori11_complex, ori11_noncomplex2)
#write.csv(ori_11_pre, "ori_11pre.csv")
#rename as ori_11 and get ready for the further step
