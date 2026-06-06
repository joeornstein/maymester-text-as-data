library(tidyverse)
library(quallmer)

# load our cleaned up ANES open-ended responses
load('data/anes/anes_2024_open-ended-cleaned.RData')

# keep only rows who responded to the "most important problem" question
df <- oe_wide |>
  filter(!is.na(post_mip_most_important),
         post_mip_most_important != 'SK')


## 1. Create Codebook ----------------------

# We want to label the most-important problem
# responses into the following mutually exclusive categories:
# 1A. Economic Issues (generally, excluding inflation or cost of living)
# 1B. Cost of living / inflation
# 1C. Poverty or Inequality
# 1D. Trade or Tariffs
# 1E. Government Finance / National Debt
# 2A. Health of Democracy
# 2B. Political Divisions / Polarization
# 2C. Concerns About Joe Biden, Kamala Harris, or other Democratic Politicians
# 2D. Concerns About Donald Trump, JD Vance, or other Republican Politicians
# 3A. Violence and Crime
# 3B. Women's Rights / Abortion
# 3C. Immigration (including concerns about crime by immigrants)
# 3D. Climate Change
# 3E. Education
# 3F. Other Social Issues Not Listed Above
# 4A. Foreign Affairs / War
# 5A. Other Concerns Not Listed Above

mip_codebook <- qlm_codebook(

  name = 'Codebook for ANES open-ended Most Important Problem responses',

  instructions = paste(
    "Please read the following statement from a respondent in the 2024 ANES survey about the most important problem facing the United States.",
    "Assign the response to a category. When a response could belong to multiple categories, make your best judgment based on which concern seems to predominate.",
    "",
    "DOMAIN 1. ECONOMY",
    "1A. Cost of Living",
    "Concerns about costs, prices, inflation, and affordability.",
    "1B. Poverty and Inequality",
    "Concerns about poverty, economic inequality, and wealth gaps. May also include homelessness, excluding concerns about violence and crime.",
    "1C. Trade and Tariffs",
    "Concerns about international trade, tariffs, and economic disruptions due to foreign competition.",
    "1D. Government Finance and Debt",
    "Concerns about government revenues, spending, debt, or deficits.",
    "1E. Economic Issues (Other)",
    "Any concerns about economic issues that do not belong in other categories, including general concerns about the economy.",
    "",
    "DOMAIN 2. POLITICS",
    "2A. Health of Democracy",
    "Concerns about the functioning of the government, elections, or democratic norms. May include concerns about misinformation and media bias.",
    "2B. Political Divisions and Polarization",
    "Concerns about political divisions, polarization, and gridlock. May include concerns about political violence.",
    "2C. Democrats",
    "Concerns about Joe Biden, Kamala Harris, or other Democratic politicians. May also include concerns about Democrats or liberals generally.",
    "2D. Republicans",
    "Concerns about the quality, personality, or policy goals of political figures within the Republican Party, and the party itself. May include concerns about conservatives generally.",
    "",
    "DOMAIN 3. SOCIAL ISSUES",
    "3A. Crime and Violence",
    "Concerns about crime, particularly violent crime that does not mention who it is perpetrated by. Issues with the way crime is prosecuted and punished - law and order. Must exclude mentions of context related to immigration and/or border security.",
    "3B. Women's Rights and Abortion",
    "Concerns about Women’s Rights broadly, and specifically, their access to abortion care and other forms of reproductive healthcare. Both positive and negative.",
    "3C. Immigration",
    "Concerns  about immigration, refugee or migrant itself, It may include problems of immigration such as illegal immigration and crimes of immigration, and approaches to address immigration issues such as border control.",
    "3D. Climate Change",
    "Concerns about climate change, climate crisis or global warming. It may include the causes, the impacts of or the approaches to address climate change.",
    "3E. Education",
    "Concerns about education across all levels, such as kids education, and college education. It may include the issue with education system, such as education inequality, quality of education, lack of education, and costs of education, and approaches to address education issues, such as loans and debt.",
    "3F. Other Social Issues",
    "Concerns that are not about crime, women’s rights, immigration, or education, but otherwise touch upon some social concepts regarding to power, civil rights, group status, etc.",
    "",
    "DOMAIN 4. FOREIGN AFFAIRS",
    "4A. Foreign Affairs and War",
    "Concerns that discuss the role of the United States in international organizations (NATO, UN), diplomatic relations between the United States and other countries, and the possible role of the United States in armed conflict.",
    "",
    "DOMAIN 5. OTHER",
    "5A. Other concerns not listed above",
    "5B. I don't know"
  ),

  schema = type_object(
    category = type_enum(
      values = c('cost_of_living', 'poverty_inequality', 'trade_tariffs',
                 'gov_finance_debt', 'economic_issues_general',
                 'health_of_democracy', 'divisions_polarization',
                 'democrats', 'republicans', 'crime_violence', 'womens_rights_abortion',
                 'immigration', 'climate_change', 'education', 'other_social',
                 'foreign_affairs', 'other', 'unknown'),
      description = 'Assigned category for most important problem'
    )
  ),

  role = "You are an expert political science coder trained in coding ANES open-ended responses."


)

## 2. Code a subset of the MIP responses using our codebook ------------

set.seed(1541)
df_subset <- df |>
  slice_sample(n = 100)


coded_responses_anthropic <- qlm_code(
  df_subset$post_mip_most_important,
  codebook = mip_codebook,
  model = 'anthropic',
  name = 'anthropic_mip_coding_subset',
  notes = 'Claude Sonnet coding of a random sample of 100 responses'
)

coded_responses_gpt <- qlm_code(
  df_subset$post_mip_most_important,
  codebook = mip_codebook,
  model = 'openai/gpt-5.4-mini',
  name = 'gpt_mip_coding_subset',
  notes = 'GPT-5.4-mini coding of a random sample of 100 responses'
)

# coding responses with local open-source LLM
# see 'llama.R' in this folder for setup instructions
coded_responses_llama <- qlm_code(
  df_subset$post_mip_most_important,
  codebook = mip_codebook,
  model = 'ollama/llama3.2:1b',
  name = 'llama_mip_coding_subset',
  notes = 'Llama 3.2:1B coding of a random sample of 100 responses'
)


# assess intercoder reliability
qlm_compare(coded_responses_gpt, coded_responses_anthropic,
            coded_responses_llama)


df_subset$anthropic_label <- coded_responses_anthropic$category
df_subset$gpt_label <- coded_responses_gpt$category
df_subset$llama_label <- coded_responses_llama$category

df_subset |>
  select(post_mip_most_important, anthropic_label,
         gpt_label, llama_label) |>
  View()

save(coded_responses_gpt, coded_responses_anthropic,
     coded_responses_llama,
     file = "data/anes/quallmer-subset.RData")



# qlm_validate()
# this one should be classified as cost of living: "Economy.  How to pay the bills and eat.  I am on Social Security"
# "murdering babies" should be women's rights / abortion