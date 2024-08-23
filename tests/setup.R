# 
# 
# 
library(haven)

nssrn_tf <- tempfile()

nssrn_url <-
	"https://data.hrsa.gov/DataDownload/NSSRN/GeneralPUF22/2022_NSSRN_PUF_Stata_Package.zip"
	
download.file( nssrn_url , nssrn_tf , mode = 'wb' )

nssrn_files <- unzip( nssrn_tf , exdir = tempdir() )

nssrn_dta <- grep( "\\.dta$" , nssrn_files , ignore.case = TRUE , value = TRUE )

nssrn_tbl <- read_dta( nssrn_dta )

nssrn_df <- data.frame( nssrn_tbl )

names( nssrn_df ) <- tolower( names( nssrn_df ) )

nssrn_df[ , 'one' ] <- 1
# nssrn_fn <- file.path( path.expand( "~" ) , "NSSRN" , "this_file.rds" )
# saveRDS( nssrn_df , file = nssrn_fn , compress = FALSE )
# nssrn_df <- readRDS( nssrn_fn )
library(survey)

nssrn_design <- 
	svrepdesign(
		weight = ~rkrnwgta ,
		repweights = 'rkrnwgta[0-9]+' ,
		scale = 4 / 80 ,
		rscales = rep( 1 , 80 ) ,
		mse = TRUE ,
		type = 'JK1' ,
		data = nssrn_df
	)
nssrn_design <- 
	update( 
		nssrn_design , 
		
		# all advanced practice registered nurses
		# (including nurse practitioners)
		all_aprn = as.numeric( ed_lcrn == 2 ) ,
		
		age_group =
			factor(
				findInterval( age_gp_puf , c( 0 , 3 , 5 , 7 , 9 ) ) ,
				levels = 1:5 ,
				labels = 
					c( 
						'34 or younger' ,
						'35 to 44' ,
						'45 to 54' ,
						'55 to 64' ,
						'65 or older'
					)
			) ,
			
		primary_position_state =
			factor(
				as.numeric( pn_loc_code_puf ) ,
				levels = 
					c(1L, 2L, 4L, 5L, 6L, 8L, 9L, 10L, 
					11L, 12L, 13L, 15L, 16L, 17L, 18L, 
					19L, 20L, 21L, 22L, 23L, 24L, 25L, 
					26L, 27L, 28L, 29L, 30L, 31L, 32L, 
					33L, 34L, 35L, 36L, 37L, 38L, 39L, 
					40L, 41L, 42L, 44L, 45L, 46L, 47L, 
					48L, 49L, 50L, 51L, 53L, 54L, 55L, 
					56L, 72L,
					# note collapsed geographies from codebook
					500L, 800L) ,
				labels =
					c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
					"Colorado", "Connecticut", "Delaware", "District of Columbia", 
					"Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
					"Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
					"Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", 
					"Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
					"New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", 
					"Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", 
					"South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", 
					"Washington", "West Virginia", "Wisconsin", "Wyoming", "Puerto Rico",
					# note collapsed geographies from codebook
					"District of Columbia & Delaware", "Montana & Wyoming")
			)
	)
sum( weights( nssrn_design , "sampling" ) != 0 )

svyby( ~ one , ~ age_group , nssrn_design , unwtd.count )
svytotal( ~ one , nssrn_design )

svyby( ~ one , ~ age_group , nssrn_design , svytotal )
svymean( ~ pn_earn_puf , nssrn_design , na.rm = TRUE )

svyby( ~ pn_earn_puf , ~ age_group , nssrn_design , svymean , na.rm = TRUE )
svymean( ~ primary_position_state , nssrn_design , na.rm = TRUE )

svyby( ~ primary_position_state , ~ age_group , nssrn_design , svymean , na.rm = TRUE )
svytotal( ~ pn_earn_puf , nssrn_design , na.rm = TRUE )

svyby( ~ pn_earn_puf , ~ age_group , nssrn_design , svytotal , na.rm = TRUE )
svytotal( ~ primary_position_state , nssrn_design , na.rm = TRUE )

svyby( ~ primary_position_state , ~ age_group , nssrn_design , svytotal , na.rm = TRUE )
svyquantile( ~ pn_earn_puf , nssrn_design , 0.5 , na.rm = TRUE )

svyby( 
	~ pn_earn_puf , 
	~ age_group , 
	nssrn_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE , na.rm = TRUE
)
svyratio( 
	numerator = ~ pn_earn_puf , 
	denominator = ~ hrs_yr_puf , 
	nssrn_design ,
	na.rm = TRUE
)
sub_nssrn_design <- subset( nssrn_design , pn_lcreq_none == 2 )
svymean( ~ pn_earn_puf , sub_nssrn_design , na.rm = TRUE )
this_result <- svymean( ~ pn_earn_puf , nssrn_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ pn_earn_puf , 
		~ age_group , 
		nssrn_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( nssrn_design )
svyvar( ~ pn_earn_puf , nssrn_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ pn_earn_puf , nssrn_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ pn_earn_puf , nssrn_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ all_aprn , nssrn_design ,
	method = "likelihood" )
svyttest( pn_earn_puf ~ all_aprn , nssrn_design )
svychisq( 
	~ all_aprn + primary_position_state , 
	nssrn_design 
)
glm_result <- 
	svyglm( 
		pn_earn_puf ~ all_aprn + primary_position_state , 
		nssrn_design 
	)

summary( glm_result )

unwtd_count_result <- svyby( ~ one , ~ age_group , nssrn_design , unwtd.count )

# cells L398 thru L402
stopifnot( coef( unwtd_count_result ) == c( 6693 , 12268 , 10804 , 10538 , 8811 ) )

wtd_n_result <- svytotal( ~ age_group , nssrn_design )

# cells J398 thru J402
stopifnot( round( coef( wtd_n_result ) , 0 ) == c( 861060 , 1078187 , 935778 , 834939 , 639412 ) )

share_result <- svymean( ~ age_group , nssrn_design )

# cells K398 thru K402
stopifnot( round( coef( share_result ) , 3 ) == c( 0.198 , 0.248 , 0.215 , 0.192 , 0.147 ) )

# cells M398 thru M402
stopifnot( 
	round( SE( share_result ) / coef( share_result ) , 4 ) == 
	c( 0.0206 , 0.0155 , 0.0192 , 0.0187 , 0.0125 )
)
library(srvyr)
nssrn_srvyr_design <- as_survey( nssrn_design )
nssrn_srvyr_design %>%
	summarize( mean = survey_mean( pn_earn_puf , na.rm = TRUE ) )

nssrn_srvyr_design %>%
	group_by( age_group ) %>%
	summarize( mean = survey_mean( pn_earn_puf , na.rm = TRUE ) )
