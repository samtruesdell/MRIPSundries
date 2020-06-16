

#' Print variable definitions
#'
#' Print definitions for submited arguments if they exist. Arguments are
#' (case-insensitive) column names from the trip, catch or size data frames.
#' Documentation based on EXCEL workbook that can be found here:
#' <https://www.st.nmfs.noaa.gov/st1/recreational/MRIP Survey Data/>
#' under MRIP survey variables.xlsx
#'
#' @param colName Name of an MRIP column
#'
#' @return printed output to the console defining the variable
#'
#' @examples
#'
#' # Print definition for single argument
#' printColDef('MODE_FX')
#'
#' # Print definition for multiple arguments (case-insensitive)
#' printColDef(c('Year', 'reLEase'))
#'
#' # Return when argument not found
#' printColDef('dummyArg')
#'
#' @export


printColDef <- function(colName){


  trip <- c(

    prim1common = 'Common name of primary species targeted.',

    prim2common = 'Common name of secondary species targeted.',

    strat_id = 'Survey design stratum identifier., Concatenation of year, month, state, region, mode_fx, kind of day and strat_interval.',

    psu_id = 'Primary sampling unit., Concatenation of year, wave, state, region, mode_fx and asg_code.',

    Year = 'Year of sample record.',

    REG_RES = 'Interviewee region of residence. See Excel file for designations.',

    ST_RES = 'Interviewee state of residence (FIPS code).',

    CNTY_RES = 'Interviewee country of residence (FIPS code).',

    ST = 'State of intercept.',

    CNTY = 'Country of intercept.',

    INTSITE = 'Four digit site code. These identifiers can be matched to sites on the MRIP site register. The register can be found at https://www.st.nmfs.noaa.gov/msd/html/siteRegister.jsp. All site IDs for a particular state can be found by selecting the state from the dropdown menu and then clicking the button Export.',

    MODE_F = 'One version of categories for fishing mode. This includes more categories than MODE_FX and is used less frequently.',

    MODE_FX = 'Collapsed fishing mode where certain categories of fishing have been collapsed to *shore* Typically used to query fishing modes.\n  1 Man-made\n  2 Beach/bank\n  3 Shore\n  4 Headboat\n  5 CHarter boat\n  7 Private/renal boat\n',

    Area = 'Fishing area for trip., See Excel document for full list of elements.',

    Area_X = 'Collapsed fishing area.\n  1 Ocean <= 3 miles from shore\n  2 Ocean > 3 miles from shore\n  3 Ocean <= 10 miles from shore\n  4 Ocean > 10 miles from shore\n  5 Inland\n',

    HRSF = 'Hours fished.',

    ADD_HRS = 'Additional hours expected to be fishing (relevant for incomplete shore trips).',

    FFDAYS12 = 'Days saltwater fin-fishing in last 12 months.',

    FFDAYS2 = 'Days saltwater fin-fishing in last 2 months.',

    CNTRBTRS = 'Number of contributors to the available catch., Sometimes, when more than one angler fishes together, the catch cannot be separated out by angler. In those cases a single catch total is assigned to the primary interviewee but the contributors are noted so that expansions to total harvest can be calculated accordingly. This is important because it complicates analyses for bag limits since the distribution was not necessarily identical across the fishing group.',

    NUM_TYP2_NUM_TYP6 = 'Number of type 2, 3, 6 records. Type 4 records are listed as a 0 or 1 flag for whether or not they exist. Unclear what this is referring to.',

    PARTY = 'Number of anglers in fishing party.',

    FIRST = 'Flag for whether record is first person in party to be interviewed (1=year, 2=no).',

    ID_CODE = 'Reference code for record. Concatenation of assignment number (1 digit), interviewer code (4 digit), date (YYYYMMDD) and interview number (3 digit).',

    SUB_REG = 'Sub-region of trip\n  4 = North Atlantic (ME; NH; MA; RI; CT)\n  5 = Mid-Atlantic (NY; NJ; DE; MD; VA)\n  6 = South Atlantic (NC; SC; GA; EFL)\n  7 = Gulf of Mexico (WFL; AL; MS; LA)\n  8 = West Pacific (HI)\n  11 = U.S., Caribbean (Puerto Rico and Virgin Islands)\n',

    WAVE = 'Wave of sampling event\n  1 = January/February\n  2 = March/April\n  3 = May/June\n  4 = July/August\n  5 = September/October\n  6 = November/December',

    CATCH = 'Were any fish caught (1=yes, 2=no, 3=catch on another interviewee*s form).',

    ON_LIST = 'Is a for-hire boat on FHS sample frame for the wave (1=yes, 2=no).',

    BOAT_HRS = 'Hours on boat.',

    NUM_TYP9 = 'Number of type-9 records (?).',

    telefon = 'Has home telephone.',

    COASTAL = 'Specifies whether interviewee is coastal county resident (N=non-coastal county; O=out-of-state; Y=coastal county).',

    kod = 'Kind of day (wd=weekday, we=weekend).',

    ASG_CODE = 'Assignment code.,  Concatenation of assignment number (1 digit), interviewer code (4 digits) and date (YYYYMMDD).',

    MODE_ASG = 'assignment mode for MRIP sampler (codes look like shore, charter, private/rental, etc., but meaning is unclear).  Different for North Carolina.\n  1 = SH\n  2 = PC\n  3 = PR\n  5 = CH\n  6 = HB\n',

    new_list = 'is the vessel in the VTR or FHS vessel directory? (1=yes, 2=no).',

    PRT_CODE = 'ID code for first member of fishing party.',

    CELLTYPE = 'Type of cell.  Can be used along with add_hrs to identify incomplete trips.\n  1 = Only complete trips\n  2 = Mixed trips\n  3 = Only incomplete trips\n  4 = Trips of unknown nature\n',

    region = 'Sub-state geographic area stratum in which sampling assignment was conducted., MA has 3 regions:\n Barnstable, Dukes, Nantucket\n Plymouth, Bristol\n Norfolk, Suffolk, Essex\n',


  	strat_interval = 'Subregion code for region of trip\n  4 = North Atlantic (ME; NH; MA; RI; CT)\n  5 = Mid-Atlantic (NY; NJ; DE; MD; VA)\n  6 = South Atlantic (NC; SC; GA; EFL)\n  7 = Gulf of Mexico (WFL; AL; MS; LA)\n  8 = West Pacific (HI)\n  11 = U.S. Caribbean (Puerto Rico and Virgin Islands)\n',

  	month = 'Month of intercept.',

  	fshinsp_a = 'Number of fish available for inspection during interview.',

  	num_fish_a = 'Number of fish not available for inspection during interview.',

  	COUNTY = 'Interviewee county of residence.',

  	TIME = 'Time of intercept.',

  	tsn1 = 'TSN (taxonomic serial number) of primary species caught.',

  	tsn2 = 'TSN of secondary species caught.',

  	DISTKEYS = 'Distance from shore\n  1 = <= 3 miles\n  2 = > 3 miles\n  3 = <= 10 miles\n  4 = > 10 miles\n  8 = not applicable or not reported\n',


  	GEAR = 'Type of gear used.\n  1 = hook & line\n  2 = dip net, A-frame net\n  3 = cast net\n  4 = Gill net\n  5 = Seine\n  6 = Trawl\n  7 = Trap\n  8 = Spear\n  9 = Hand\n  10 = Other\n  11 = YoYo (Puerto Rico only)\n  98 = Unknown\n  99 = Refused\n',

  	PVT_RES = 'Residence type.',

  	SEP_FISH = 'Can grouped catch be separated (1=yes, 2=no, 8=NA).',

  	ADD_PH = 'Whether phone number was provided.',

  	F_BY_P = 'Were all fish caught by the individual being interviewed or were there other contributers (1=yes, 2=others, 8=NA).',

  	date1 = 'File creation date.',

  	DIST = 'Distance from shore.\n 1 = <= 3 miles\n 2 = > 3 miles\n 3 = <= 10 miles\n 4 = > 10 miles\n 8 = not applicable or not reported\n',

    prim1 = 'Primary target species (species code).',

    prim2 = 'Secondary target species (species code).',

    MODE2001 = 'A different version of ACCSP fishing mode., Details in Excel file.',

    TURTLE = 'Was a turtle seen while fishing (1=yes/alive, 2=yes/dead, 3=no)',

    GENDER = 'Angler gender (1=male, 2=female, 9=missing)',

    REEFCODE = 'Code for artificial reef fished at (if applicable). See Excel file.',

    AGE = 'Angler age.',

    AREA_NC = 'Special area codes for North Carolina.',

    ART_REEF = 'Did the angler fish near an artificial reef.',

    muni_res = 'Municipality or island of residence.',

    muni_trp = 'Municipality or island of trip.',

    wp_int = 'Statistical weight associated with intercept.',

    VAR_ID = 'Not available in variable descriptions. Looks to be a concatenation of intercept-related fields.',

    ARX_METHOD = 'Not available in variable descriptions.',

    ALT_FLAG = 'Not available in variable descriptions.',

    LEADER = 'ID code for group leader.'


  )


  catch <- c(

    common = 'Common name of species.',

    strat_id = 'See definition in trip variables.',

    psu_id = 'See definition in trip variables.',

    YEAR = 'See definition in trip variables.',

    ST = 'See definition in trip variables.',

    MODE_FX = 'See definition in trip variables.',

    AREA_X = 'See definition in trip variables.',

    ID_CODE = 'See definition in trip variables.',

    SUB_REG = 'See definition in trip variables.',

    WAVE = 'See definition in trip variables.',

    kod = 'See definition in trip variables.',

    SP_CODE = 'Species code of fish.',

    CLAIM = 'Count of fish that were caught, landed whole and available for ID and weighing/measuring by the interviewer.',

    RELEASE = 'Number of fish that were caught and released alive.',

    HARVEST = 'Number of fish that were caught, not released alive, but also not available in whole for for the interviewer (e.g., filleted). All harvested fish are dead.',

    CLAIM_UNADJUSTED = 'Not listed in variable descriptions. Assume this is the raw value.',

    HARVES_UNADJUSTED = 'Not listed in variable descriptions. Assume this is the raw value.',

    RELEASE_UNADJUSTED = 'Not listed in variable descriptions. Assume this is the raw value.',

    tot_len_a = 'Total fork length for all landed fish. Note this is not very useful because it is the sum over all records. Use the size data for lengths instead.',

    wgt_a = 'Total weight of all observed harvest (claim, type A).',

    tot_len_b1 = 'Total fork length for all reported harvest (again, not very useful).',

    wgt_b1 = 'Total weight of all reported harvest (HARVEST, type B1) from trip.',

    region = 'See definition in trip variables.',

    month = 'See definition in trip variables.',

    wp_int = 'See definition in trip variables.',

    fl_reg = 'Florida region. Full details in Excel spreadsheet.',

    wp_catch = 'Statistical weight associated with record and used in expansion. Often the same as wp_int, but includes a correction for small sample sizes, meaning that sometimes these two fields differ.',

    tot_cat = 'Number of fish caught but not necessarily brought ashore. Sum of A (CLAIM), B1 (HARVEST) and B2 (RELEASE) variables.',

    wgt_ab1 = 'Total weight (kg) of fish removed from the fishery resource (LANDING, A+B1) on trip.',

    tot_len = 'Total fork length for all landings (A+B1). Not necessarily useful as it is the sum.',

    landing = 'Total number of individuals removed from resource -- sum of A (CLAIM) and B1 (HARVEST).',

    var_id = 'See definition in trip variables.',

    arx_method = 'See definition in trip variables.',

    alt_flag = 'See definition in trip variables.'

  )





  size <- c(

    YEAR = 'See definition in trip variables.',

    ST = 'See definition in trip variables.',

    MODE_FX = 'See definition in trip variables.',

    AREA_X = 'See definition in trip variables.',

    ID_CODE = 'See definition in trip variables.',

    SUB_REG = 'See definition in trip variables.',

    WAVE = 'See definition in trip variables.',

    kod = 'See definition in trip variables.',

    SP_CODE = 'See definition in trip variables.',

    LNGTH = 'Fork length of fish (mm).',

    WGT = 'Round weight of fish (kg).',

    lngth_imp = 'Indicator for whether length was imputed (i.e., estimated based on similar data; 1=yes, 0=no). See https://www.fisheries.noaa.gov/recreational-fishing-data/recreational-fishing-data-glossary#weight-data for more information on how impusions works.',

    wgt_imp = 'Indicator for whether weight was imputed.',

    month = 'See definition in trip variables.',

    strat_id = 'See definition in trip variables.',

    psu_id = 'See definition in trip variables.',

    common = 'See definition in trip variables.',

    wp_size = 'Statistical weighting for records in the size data set.',

    l_in_bin = 'Length bin in inches (i.e., floor of length measurement converted to inches).',

    l_cm_bin = 'Length bin in cm (i.e., floor of length measurement).',

    VAR_ID = 'See definition in trip variables.',

    ARX_METHOD = 'See definition in trip variables.',

    ALT_FLAG = 'See definition in trip variables.'

  )


  getName <- function(name, tab){
    flg <- toupper(name) %in% toupper(names(tab))
    if(flg){
      mch <- match(toupper(name), toupper(names(tab)))
      return(list(nm = names(tab)[mch],
                  def = tab[mch]))
    }else{
      return(list(nm = name,
                  def = 'not found in data set names'))
    }
  }

  for(i in 1:length(colName)){

    tr <- getName(colName[i], trip)
    ca <- getName(colName[i], catch)
    si <- getName(colName[i], size)

    cat(rep('=', 50), '\n', sep='')
    cat('trip data: ', tr$nm, '\n ', tr$def, '\n', rep('=', 50), '\n', sep='')
    cat('catch data: ', ca$nm, '\n ', ca$def, '\n', rep('=', 50), '\n', sep='')
    cat('size data: ', si$nm, '\n ', si$def, '\n', rep('=', 50), '\n', sep='')

  }


}




