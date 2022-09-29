library(metacore)
library(metatools)
library(haven)
library(dplyr)

# Read in the metadata 
metacore<- spec_to_metacore("specs.xlsx", where_sep_sheet = FALSE)

# Read in datasets as normal
dm <- read_xpt("SDTM/dm.xpt")

print(metacore)

metacore$ds_spec
# It has the 3 datasets we will be using for our tables 
metacore$ds_spec
# We can see the metadata from the spec document is now in this structure in R 
head(metacore$var_spec)

# Now to start building ADSL we are going to subset the object to only that
adsl_spec <- metacore %>% 
  select_dataset("ADSL")

# the next step is to pull in the predecessor variables 
adsl_pred <- build_from_derived(adsl_spec, 
                                ds_list = list("dm" = dm), 
                                keep = TRUE) %>% 
  filter(ARMCD %in% c("A", "P"))

# Using the metacore object we can make the decodes of control terminology 
get_control_term(adsl_spec, variable = SEXN)

adsl_pred %>% 
  create_var_from_codelist(adsl_spec, input_var = SEX, out_var = SEXN) %>% 
  select(USUBJID, SEX, SEXN)

# Now we can repeat the process for all our code/decode pairs 
adsl_decode<- adsl_pred %>% 
  create_var_from_codelist(adsl_spec, input_var = SEX, out_var = SEXN)%>% 
  create_var_from_codelist(adsl_spec, input_var = ETHNIC, out_var = ETHNICN) %>% 
  create_var_from_codelist(adsl_spec, input_var = ARMCD, out_var = TRT01PN) %>% 
  create_var_from_codelist(adsl_spec, input_var = ACTARMCD, out_var = TRT01AN) %>%
  create_var_from_codelist(adsl_spec, input_var = ARMCD, out_var = TRT01P) %>% 
  create_var_from_codelist(adsl_spec, input_var = ACTARMCD, out_var = TRT01A) %>% 
  create_var_from_codelist(adsl_spec, input_var = COUNTRY, out_var = ACOUNTRY) %>% 
  create_var_from_codelist(adsl_spec, input_var = RACE, out_var = RACEN)

# Additional metatools can help create subgroups such as AGE categories 
get_control_term(adsl_spec, variable = AGEGR1)

adsl_decode %>% 
  create_cat_var(adsl_spec, ref_var = AGE, grp_var = AGEGR1) %>% 
  select(USUBJID, AGE, AGEGR1)

adsl_grouped <- adsl_decode %>%
  create_cat_var(adsl_spec, ref_var = AGE, grp_var = AGEGR1, num_grp_var = AGEGR1N) 


# Once you have created all the variables will want to add label 
adsl <- adsl_grouped %>% 
  order_cols(adsl_spec) %>%
  set_variable_labels(adsl_spec)


# Finally, you can then check against the specification that all is well 
adsl %>% 
  check_ct_data(adsl_spec) %>%  
  check_variables(adsl_spec)
