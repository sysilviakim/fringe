
# Data import for all three platforms and all three federal races ==============
data/tidy/president_2020.fst : R/01_data_import.R
		Rscript R/01_data_import.R
		
# Portfolio summaries for all three platforms ==================================
data/tidy/portfolio_summ_actblue_incomplete.Rda : R/02_portfolio_freq_actblue.R
		Rscript R/02_portfolio_freq_actblue.R
		
data/tidy/portfolio_summ_rightus.Rda : R/02_portfolio_freq_rightus.R
		Rscript R/02_portfolio_freq_rightus.R
		
data/tidy/portfolio_summ_winred.Rda : R/02_portfolio_freq_winred.R
		Rscript R/02_portfolio_freq_winred.R

# ActBlue needs more work to match with fundraising entities ===================
data/tidy/actblue_fundraisers_full.fst : R/03_actblue_entities_aug.R
		RScript R/03_actblue_entities_aug.R
		
data/tidy/portfolio_summ_actblue.Rda : R/03_actblue_fundraisers_match.R
		RScript R/03_actblue_fundraisers_match.R
		
# Finalize platforms data as list with classification ==========================
data/tidy/portfolio_summ_platforms.Rda : R/04_portfolio_platform_wrangle.R
		RScript R/04_portfolio_platform_wrangle.R
		
