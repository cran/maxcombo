#the below is not strictly needed since it is included in packagesetupandimportsandexamples.R, but I am putting it here to remind you of what namespaces are available in this package
#' @importFrom magrittr %>% %<>% %$% %T>%
#' @import rlang
NULL

#### EXAMPLES ADDED TO DOCUMENTATION FOR oogetdoublemaxcomboteststatistic ####
#' @rdname oogetdoublemaxcomboteststatistic
#' @name oogetdoublemaxcomboteststatistic
#' @examples
#' \donttest{
#' 
#' # -------------------------------------------------------------------------------
#' # Example 1: Usage on a single deterministic dataset in which the drug halves
#' # the hazard at all times (i.e., a proportional hazards situation)
#' # -------------------------------------------------------------------------------
#' 
#' oointnparticipants=100L
#' oointnparticipantsplacebo=oointnparticipants/2L
#' oointnparticipantsactive=oointnparticipants/2L
#' 
#' oodoublerateplacebo=0.250
#' oodoublerateactive=0.125
#' 
#' oovecinttreated=c(
#'   base::rep(0L,length.out=oointnparticipantsplacebo),
#'   base::rep(1L,length.out=oointnparticipantsactive)
#' )
#' oovecdoubletAabsolute=c( #the start time, i.e., when the subject enters the study.
#'   base::seq(from=0.0,to=2.0,length.out=oointnparticipantsplacebo),
#'   base::seq(from=0.0,to=2.0,length.out=oointnparticipantsactive)
#' )
#' #the duration of time from when the subject enters the study until the subject experiences the event
#' oovecdoubletAtoB=c(
#'   stats::qexp(
#'     base::seq(from=0.0,to=0.98,length.out=oointnparticipantsplacebo),
#'     rate=oodoublerateplacebo
#'   ),
#'   stats::qexp(
#'     base::seq(from=0.0,to=0.98,length.out=oointnparticipantsactive),
#'     rate=oodoublerateactive
#'   )
#' )
#' oovecdoubletBabsolute=oovecdoubletAabsolute + oovecdoubletAtoB
#' #the analysis takes place at absolute time 6.0 months, and no other censoring (e.g., dropout) occurs
#' oovecdoubletCabsolute=6.0
#' oovecdoubletminBvsC=base::pmin(oovecdoubletBabsolute,oovecdoubletCabsolute)
#' oovecboolobservedB=(oovecdoubletBabsolute < oovecdoubletCabsolute)
#' oovecboolobservedC=(oovecdoubletCabsolute <= oovecdoubletBabsolute)
#' 
#' oodataframe=dplyr::tibble(id=1L:oointnparticipants,
#'                           treated=oovecinttreated,
#'                           Atime=oovecdoubletAabsolute,
#'                           Btime=oovecdoubletminBvsC,
#'                           Bobserved=oovecboolobservedB,
#'                           Ctime=oovecdoubletminBvsC,
#'                           Cobserved=oovecboolobservedC)
#' 
#' #standardized log-rank test statistic
#' oolistweightingfunctionsJustLogrank=base::list(
#'   logrank=function(stminus){ base::return(1.0) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustLogrank
#' ) #2.92
#' 
#' #standardized weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function,
#' #which places greater weight on later times
#' oolistweightingfunctionsJustFlemingHarrington01=base::list(
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington01
#' ) #2.83
#' 
#' #standardized weighted log-rank test statistic with Fleming-Harrington 1-0 weighting function,
#' #which places greater weight on earlier times
#' oolistweightingfunctionsJustFlemingHarrington10=base::list(
#'   flemingharrington10=function(stminus){ base::return(stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington10
#' ) #2.71
#' 
#' #the max-combo test statistic based on the first two of the above
#' oolistweightingfunctionsLogrankAndFlemingHarrington01=base::list(
#'   logrank=function(stminus){ base::return(1.0) },
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsLogrankAndFlemingHarrington01
#' ) #2.92, i.e., just the maximum of 2.92 (from the log-rank test statistic) and 2.83 (from the
#' # weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function)
#' 
#' #the max-combo test statistic based on the first three of the above
#' oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10=base::list(
#'   logrank=function(stminus){ base::return(1.0) },
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) },
#'   flemingharrington10=function(stminus){ base::return(stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = 
#'   oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10
#' ) #2.92, i.e., just the maximum of 2.92 (from the log-rank test statistic), 2.83 (from the
#' # weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function), and
#' # 2.71 (from the weighted log-rank test statistic with Fleming-Harrington 1-0 weighting function)
#' 
#' 
#' # --------------------------------------------------------------------------------------------
#' # Example 2: Usage on a single deterministic dataset in which the drug delays
#' # the event by exactly one month for each subject (i.e., an early treatment effect situation)
#' # --------------------------------------------------------------------------------------------
#' 
#' oointnparticipants=100L
#' oointnparticipantsplacebo=oointnparticipants/2L
#' oointnparticipantsactive=oointnparticipants/2L
#' 
#' oodoublerateplacebo=0.250
#' 
#' oovecinttreated=c(
#'   base::rep(0L,length.out=oointnparticipantsplacebo),
#'   base::rep(1L,length.out=oointnparticipantsactive)
#' )
#' oovecdoubletAabsolute=c( #the start time, i.e., when the subject enters the study.
#'   base::seq(from=0.0,to=2.0,length.out=oointnparticipantsplacebo),
#'   base::seq(from=0.0,to=2.0,length.out=oointnparticipantsactive)
#' )
#' #the duration of time from when the subject enters the study until the subject experiences the event
#' oovecdoubletAtoB=c(
#'   stats::qexp(
#'     base::seq(from=0.0,to=0.98,length.out=oointnparticipantsplacebo),
#'     rate=oodoublerateplacebo
#'   ),
#'   stats::qexp(
#'     base::seq(from=0.0,to=0.98,length.out=oointnparticipantsactive),
#'     rate=oodoublerateplacebo
#'   ) + 1.0 #note the addition of 1.0 month time to event here for the active arm
#' )
#' oovecdoubletBabsolute=oovecdoubletAabsolute + oovecdoubletAtoB
#' #the analysis takes place at absolute time 6.0 months, and no other censoring (e.g., dropout) occurs
#' oovecdoubletCabsolute=6.0
#' oovecdoubletminBvsC=base::pmin(oovecdoubletBabsolute,oovecdoubletCabsolute)
#' oovecboolobservedB=(oovecdoubletBabsolute < oovecdoubletCabsolute)
#' oovecboolobservedC=(oovecdoubletCabsolute <= oovecdoubletBabsolute)
#' 
#' oodataframe=dplyr::tibble(id=1L:oointnparticipants,
#'                           treated=oovecinttreated,
#'                           Atime=oovecdoubletAabsolute,
#'                           Btime=oovecdoubletminBvsC,
#'                           Bobserved=oovecboolobservedB,
#'                           Ctime=oovecdoubletminBvsC,
#'                           Cobserved=oovecboolobservedC)
#' 
#' #standardized log-rank test statistic
#' oolistweightingfunctionsJustLogrank=base::list(
#'   logrank=function(stminus){ base::return(1.0) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustLogrank
#' ) #1.66
#' 
#' #standardized weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function,
#' #which places greater weight on later times
#' oolistweightingfunctionsJustFlemingHarrington01=base::list(
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington01
#' ) #0.53
#' 
#' #standardized weighted log-rank test statistic with Fleming-Harrington 1-0 weighting function,
#' #which places greater weight on earlier times
#' oolistweightingfunctionsJustFlemingHarrington10=base::list(
#'   flemingharrington10=function(stminus){ base::return(stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington10
#' ) #2.07
#' 
#' #the max-combo test statistic based on the first two of the above
#' oolistweightingfunctionsLogrankAndFlemingHarrington01=base::list(
#'   logrank=function(stminus){ base::return(1.0) },
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsLogrankAndFlemingHarrington01
#' ) #1.66, i.e., just the maximum of 1.66 (from the log-rank test statistic) and 0.53 (from the
#' # weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function)
#' 
#' #the max-combo test statistic based on the first three of the above
#' oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10=base::list(
#'   logrank=function(stminus){ base::return(1.0) },
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) },
#'   flemingharrington10=function(stminus){ base::return(stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus =
#'   oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10
#' ) #2.07, i.e., just the maximum of 1.66 (from the log-rank test statistic), 0.53 (from the
#' # weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function), and
#' # 2.07 (from the weighted log-rank test statistic with Fleming-Harrington 1-0 weighting function)
#' 
#' 
#' # -------------------------------------------------------------------------------------------------
#' # Example 3: Usage on a single deterministic dataset in which subjects in the placebo arm all have
#' # the event after being on the study for 1.2 months (i.e., a delayed treatment effect situation)
#' # -------------------------------------------------------------------------------------------------
#' 
#' oointnparticipants=100L
#' oointnparticipantsplacebo=oointnparticipants/2L
#' oointnparticipantsactive=oointnparticipants/2L
#' 
#' oodoublerateactive=0.250
#' 
#' oovecinttreated=c(
#'   base::rep(0L,length.out=oointnparticipantsplacebo),
#'   base::rep(1L,length.out=oointnparticipantsactive)
#' )
#' oovecdoubletAabsolute=c( #the start time, i.e., when the subject enters the study.
#'   base::seq(from=0.0,to=2.0,length.out=oointnparticipantsplacebo),
#'   base::seq(from=0.0,to=2.0,length.out=oointnparticipantsactive)
#' )
#' #the duration of time from when the subject enters the study until the subject experiences the event
#' oovecdoubletAtoB=c(
#'   base::ifelse(
#'     stats::qexp(
#'       base::seq(from=0.0,to=0.98,length.out=oointnparticipantsplacebo),
#'       rate=oodoublerateactive
#'     ) <= 1.2,
#'     stats::qexp(
#'       base::seq(from=0.0,to=0.98,length.out=oointnparticipantsplacebo),
#'       rate=oodoublerateactive
#'     ),
#'     1.2
#'   ),
#'   stats::qexp(
#'     base::seq(from=0.0,to=0.98,length.out=oointnparticipantsactive),
#'     rate=oodoublerateactive
#'   )
#' )
#' oovecdoubletBabsolute=oovecdoubletAabsolute + oovecdoubletAtoB
#' #the analysis takes place at absolute time 6.0 months, and no other censoring (e.g., dropout) occurs
#' oovecdoubletCabsolute=6.0
#' oovecdoubletminBvsC=base::pmin(oovecdoubletBabsolute,oovecdoubletCabsolute)
#' oovecboolobservedB=(oovecdoubletBabsolute < oovecdoubletCabsolute)
#' oovecboolobservedC=(oovecdoubletCabsolute <= oovecdoubletBabsolute)
#' 
#' oodataframe=dplyr::tibble(id=1L:oointnparticipants,
#'                           treated=oovecinttreated,
#'                           Atime=oovecdoubletAabsolute,
#'                           Btime=oovecdoubletminBvsC,
#'                           Bobserved=oovecboolobservedB,
#'                           Ctime=oovecdoubletminBvsC,
#'                           Cobserved=oovecboolobservedC)
#' 
#' #standardized log-rank test statistic
#' oolistweightingfunctionsJustLogrank=base::list(
#'   logrank=function(stminus){ base::return(1.0) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustLogrank
#' ) #1.55
#' 
#' #standardized weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function,
#' #which places greater weight on later times
#' oolistweightingfunctionsJustFlemingHarrington01=base::list(
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington01
#' ) #2.28
#' 
#' #standardized weighted log-rank test statistic with Fleming-Harrington 1-0 weighting function,
#' #which places greater weight on earlier times
#' oolistweightingfunctionsJustFlemingHarrington10=base::list(
#'   flemingharrington10=function(stminus){ base::return(stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington10
#' ) #1.35
#' 
#' #the max-combo test statistic based on the first two of the above
#' oolistweightingfunctionsLogrankAndFlemingHarrington01=base::list(
#'   logrank=function(stminus){ base::return(1.0) },
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsLogrankAndFlemingHarrington01
#' ) #2.28, i.e., just the maximum of 1.55 (from the log-rank test statistic) and 2.28 (from the
#' # weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function)
#' 
#' #the max-combo test statistic based on the first three of the above
#' oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10=base::list(
#'   logrank=function(stminus){ base::return(1.0) },
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) },
#'   flemingharrington10=function(stminus){ base::return(stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus =
#'   oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10
#' ) #2.28, i.e., just the maximum of 1.55 (from the log-rank test statistic), 2.28 (from the
#' # weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function), and
#' # 1.35 (from the weighted log-rank test statistic with Fleming-Harrington 1-0 weighting function)
#' }
NULL








#### EXAMPLES ADDED TO DOCUMENTATION FOR oogetdoublemaxcombotestpvalue ####
#' @rdname oogetdoublemaxcombotestpvalue
#' @name oogetdoublemaxcombotestpvalue
#' @examples
#' \donttest{
#' 
#' # -------------------------------------------------------------------------------
#' # Example 1: Usage on a single deterministic dataset in which the drug halves
#' # the hazard at all times (i.e., a proportional hazards situation)
#' # -------------------------------------------------------------------------------
#' 
#' oointnparticipants=100L
#' oointnparticipantsplacebo=oointnparticipants/2L
#' oointnparticipantsactive=oointnparticipants/2L
#' 
#' oodoublerateplacebo=0.250
#' oodoublerateactive=0.125
#' 
#' oovecinttreated=c(
#'   base::rep(0L,length.out=oointnparticipantsplacebo),
#'   base::rep(1L,length.out=oointnparticipantsactive)
#' )
#' oovecdoubletAabsolute=c( #the start time, i.e., when the subject enters the study.
#'   base::seq(from=0.0,to=2.0,length.out=oointnparticipantsplacebo),
#'   base::seq(from=0.0,to=2.0,length.out=oointnparticipantsactive)
#' )
#' #the duration of time from when the subject enters the study until the subject experiences the event
#' oovecdoubletAtoB=c(
#'   stats::qexp(
#'     base::seq(from=0.0,to=0.98,length.out=oointnparticipantsplacebo),
#'     rate=oodoublerateplacebo
#'   ),
#'   stats::qexp(
#'     base::seq(from=0.0,to=0.98,length.out=oointnparticipantsactive),
#'     rate=oodoublerateactive
#'   )
#' )
#' oovecdoubletBabsolute=oovecdoubletAabsolute + oovecdoubletAtoB
#' #the analysis takes place at absolute time 6.0 months, and no other censoring (e.g., dropout) occurs
#' oovecdoubletCabsolute=6.0
#' oovecdoubletminBvsC=base::pmin(oovecdoubletBabsolute,oovecdoubletCabsolute)
#' oovecboolobservedB=(oovecdoubletBabsolute < oovecdoubletCabsolute)
#' oovecboolobservedC=(oovecdoubletCabsolute <= oovecdoubletBabsolute)
#' 
#' oodataframe=dplyr::tibble(id=1L:oointnparticipants,
#'                           treated=oovecinttreated,
#'                           Atime=oovecdoubletAabsolute,
#'                           Btime=oovecdoubletminBvsC,
#'                           Bobserved=oovecboolobservedB,
#'                           Ctime=oovecdoubletminBvsC,
#'                           Cobserved=oovecboolobservedC)
#' 
#' #standardized log-rank test statistic
#' oolistweightingfunctionsJustLogrank=base::list(
#'   logrank=function(stminus){ base::return(1.0) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustLogrank
#' ) #test statistic 2.92
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustLogrank
#' ) #p-value 0.0017
#' 
#' #standardized weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function,
#' #which places greater weight on later times
#' oolistweightingfunctionsJustFlemingHarrington01=base::list(
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington01
#' ) #test statistic 2.83
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington01
#' ) #p-value 0.0023
#' 
#' #standardized weighted log-rank test statistic with Fleming-Harrington 1-0 weighting function,
#' #which places greater weight on earlier times
#' oolistweightingfunctionsJustFlemingHarrington10=base::list(
#'   flemingharrington10=function(stminus){ base::return(stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington10
#' ) #test statistic 2.71
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington10
#' ) #p-value 0.0033
#' 
#' #the max-combo test statistic based on the first two of the above
#' oolistweightingfunctionsLogrankAndFlemingHarrington01=base::list(
#'   logrank=function(stminus){ base::return(1.0) },
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsLogrankAndFlemingHarrington01
#' ) #test statistic 2.92, i.e., just the maximum of 2.92 (from the log-rank test statistic) and 2.83
#' # (from the weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function)
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsLogrankAndFlemingHarrington01
#' ) #p-value 0.0028
#' 
#' #the max-combo test statistic based on the first three of the above
#' oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10=base::list(
#'   logrank=function(stminus){ base::return(1.0) },
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) },
#'   flemingharrington10=function(stminus){ base::return(stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = 
#'   oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10
#' ) #test statistic 2.92, i.e., just the maximum of 2.92 (from the log-rank test statistic), 2.83
#' # (from the weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function), and
#' # 2.71 (from the weighted log-rank test statistic with Fleming-Harrington 1-0 weighting function)
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = 
#'   oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10
#' ) #p-value 0.0032
#' 
#' 
#' # --------------------------------------------------------------------------------------------
#' # Example 2: Usage on a single deterministic dataset in which the drug delays
#' # the event by exactly one month for each subject (i.e., an early treatment effect situation)
#' # --------------------------------------------------------------------------------------------
#' 
#' oointnparticipants=100L
#' oointnparticipantsplacebo=oointnparticipants/2L
#' oointnparticipantsactive=oointnparticipants/2L
#' 
#' oodoublerateplacebo=0.250
#' 
#' oovecinttreated=c(
#'   base::rep(0L,length.out=oointnparticipantsplacebo),
#'   base::rep(1L,length.out=oointnparticipantsactive)
#' )
#' oovecdoubletAabsolute=c( #the start time, i.e., when the subject enters the study.
#'   base::seq(from=0.0,to=2.0,length.out=oointnparticipantsplacebo),
#'   base::seq(from=0.0,to=2.0,length.out=oointnparticipantsactive)
#' )
#' #the duration of time from when the subject enters the study until the subject experiences the event
#' oovecdoubletAtoB=c(
#'   stats::qexp(
#'     base::seq(from=0.0,to=0.98,length.out=oointnparticipantsplacebo),
#'     rate=oodoublerateplacebo
#'   ),
#'   stats::qexp(
#'     base::seq(from=0.0,to=0.98,length.out=oointnparticipantsactive),
#'     rate=oodoublerateplacebo
#'   ) + 1.0 #note the addition of 1.0 month time to event here for the active arm
#' )
#' oovecdoubletBabsolute=oovecdoubletAabsolute + oovecdoubletAtoB
#' #the analysis takes place at absolute time 6.0 months, and no other censoring (e.g., dropout) occurs
#' oovecdoubletCabsolute=6.0
#' oovecdoubletminBvsC=base::pmin(oovecdoubletBabsolute,oovecdoubletCabsolute)
#' oovecboolobservedB=(oovecdoubletBabsolute < oovecdoubletCabsolute)
#' oovecboolobservedC=(oovecdoubletCabsolute <= oovecdoubletBabsolute)
#' 
#' oodataframe=dplyr::tibble(id=1L:oointnparticipants,
#'                           treated=oovecinttreated,
#'                           Atime=oovecdoubletAabsolute,
#'                           Btime=oovecdoubletminBvsC,
#'                           Bobserved=oovecboolobservedB,
#'                           Ctime=oovecdoubletminBvsC,
#'                           Cobserved=oovecboolobservedC)
#' 
#' #standardized log-rank test statistic
#' oolistweightingfunctionsJustLogrank=base::list(
#'   logrank=function(stminus){ base::return(1.0) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustLogrank
#' ) #test statistic 1.66
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustLogrank
#' ) #p-value 0.05
#' 
#' #standardized weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function,
#' #which places greater weight on later times
#' oolistweightingfunctionsJustFlemingHarrington01=base::list(
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington01
#' ) #test statistic 0.53
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington01
#' ) #p-value 0.30
#' 
#' #standardized weighted log-rank test statistic with Fleming-Harrington 1-0 weighting function,
#' #which places greater weight on earlier times
#' oolistweightingfunctionsJustFlemingHarrington10=base::list(
#'   flemingharrington10=function(stminus){ base::return(stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington10
#' ) #test statistic 2.07
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington10
#' ) #p-value 0.02
#' 
#' #the max-combo test statistic based on the first two of the above
#' oolistweightingfunctionsLogrankAndFlemingHarrington01=base::list(
#'   logrank=function(stminus){ base::return(1.0) },
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsLogrankAndFlemingHarrington01
#' ) #test statistic 1.66, i.e., just the maximum of 1.66 (from the log-rank test statistic) and 0.53
#' # (from the weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function)
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsLogrankAndFlemingHarrington01
#' ) #p-value 0.07
#' 
#' #the max-combo test statistic based on the first three of the above
#' oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10=base::list(
#'   logrank=function(stminus){ base::return(1.0) },
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) },
#'   flemingharrington10=function(stminus){ base::return(stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus =
#'   oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10
#' ) #test statistic 2.07, i.e., just the maximum of 1.66 (from the log-rank test statistic), 0.53
#' # (from the weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function), and
#' # 2.07 (from the weighted log-rank test statistic with Fleming-Harrington 1-0 weighting function)
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = 
#'   oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10
#' ) #p-value 0.03
#' 
#'
#' # -------------------------------------------------------------------------------------------------
#' # Example 3: Usage on a single deterministic dataset in which subjects in the placebo arm all have
#' # the event after being on the study for 1.2 months (i.e., a delayed treatment effect situation)
#' # -------------------------------------------------------------------------------------------------
#'  
#' oointnparticipants=100L
#' oointnparticipantsplacebo=oointnparticipants/2L
#' oointnparticipantsactive=oointnparticipants/2L
#' 
#' oodoublerateactive=0.250
#' 
#' oovecinttreated=c(
#'   base::rep(0L,length.out=oointnparticipantsplacebo),
#'   base::rep(1L,length.out=oointnparticipantsactive)
#' )
#' oovecdoubletAabsolute=c( #the start time, i.e., when the subject enters the study.
#'   base::seq(from=0.0,to=2.0,length.out=oointnparticipantsplacebo),
#'   base::seq(from=0.0,to=2.0,length.out=oointnparticipantsactive)
#' )
#' #the duration of time from when the subject enters the study until the subject experiences the event
#' oovecdoubletAtoB=c(
#'   base::ifelse(
#'     stats::qexp(
#'       base::seq(from=0.0,to=0.98,length.out=oointnparticipantsplacebo),
#'       rate=oodoublerateactive
#'     ) <= 1.2,
#'     stats::qexp(
#'       base::seq(from=0.0,to=0.98,length.out=oointnparticipantsplacebo),
#'       rate=oodoublerateactive
#'     ),
#'     1.2
#'   ),
#'   stats::qexp(
#'     base::seq(from=0.0,to=0.98,length.out=oointnparticipantsactive),
#'     rate=oodoublerateactive
#'   )
#' )
#' oovecdoubletBabsolute=oovecdoubletAabsolute + oovecdoubletAtoB
#' #the analysis takes place at absolute time 6.0 months, and no other censoring (e.g., dropout) occurs
#' oovecdoubletCabsolute=6.0
#' oovecdoubletminBvsC=base::pmin(oovecdoubletBabsolute,oovecdoubletCabsolute)
#' oovecboolobservedB=(oovecdoubletBabsolute < oovecdoubletCabsolute)
#' oovecboolobservedC=(oovecdoubletCabsolute <= oovecdoubletBabsolute)
#' 
#' oodataframe=dplyr::tibble(id=1L:oointnparticipants,
#'                           treated=oovecinttreated,
#'                           Atime=oovecdoubletAabsolute,
#'                           Btime=oovecdoubletminBvsC,
#'                           Bobserved=oovecboolobservedB,
#'                           Ctime=oovecdoubletminBvsC,
#'                           Cobserved=oovecboolobservedC)
#' 
#' #standardized log-rank test statistic
#' oolistweightingfunctionsJustLogrank=base::list(
#'   logrank=function(stminus){ base::return(1.0) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustLogrank
#' ) #test statistic 1.55
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustLogrank
#' ) #p-value 0.06
#' 
#' #standardized weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function,
#' #which places greater weight on later times
#' oolistweightingfunctionsJustFlemingHarrington01=base::list(
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington01
#' ) #test statistic 2.28
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington01
#' ) #p-value 0.01
#' 
#' #standardized weighted log-rank test statistic with Fleming-Harrington 1-0 weighting function,
#' #which places greater weight on earlier times
#' oolistweightingfunctionsJustFlemingHarrington10=base::list(
#'   flemingharrington10=function(stminus){ base::return(stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington10
#' ) #test statistic 1.35
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington10
#' ) #p-value 0.09
#' 
#' #the max-combo test statistic based on the first two of the above
#' oolistweightingfunctionsLogrankAndFlemingHarrington01=base::list(
#'   logrank=function(stminus){ base::return(1.0) },
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsLogrankAndFlemingHarrington01
#' ) #test statistic 2.28, i.e., just the maximum of 1.55 (from the log-rank test statistic) and 2.28
#' # (from the weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function)
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsLogrankAndFlemingHarrington01
#' ) #p-value 0.02
#' 
#' #the max-combo test statistic based on the first three of the above
#' oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10=base::list(
#'   logrank=function(stminus){ base::return(1.0) },
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) },
#'   flemingharrington10=function(stminus){ base::return(stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus =
#'   oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10
#' ) #test statistic 2.28, i.e., just the maximum of 1.55 (from the log-rank test statistic), 2.28
#' # (from the weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function), and
#' # 1.35 (from the weighted log-rank test statistic with Fleming-Harrington 1-0 weighting function)
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = 
#'   oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10
#' ) #p-value 0.02
#' }
NULL








#### EXAMPLES ADDED TO DOCUMENTATION FOR oogetdoublemaxcombocutoff ####
#' @rdname oogetdoublemaxcombocutoff
#' @name oogetdoublemaxcombocutoff
#' @examples
#' \donttest{
#' 
#' # -------------------------------------------------------------------------------
#' # Example 1: Usage on a single deterministic dataset in which the drug halves
#' # the hazard at all times (i.e., a proportional hazards situation)
#' # -------------------------------------------------------------------------------
#' 
#' oointnparticipants=100L
#' oointnparticipantsplacebo=oointnparticipants/2L
#' oointnparticipantsactive=oointnparticipants/2L
#' 
#' oodoublerateplacebo=0.250
#' oodoublerateactive=0.125
#' 
#' oovecinttreated=c(
#'   base::rep(0L,length.out=oointnparticipantsplacebo),
#'   base::rep(1L,length.out=oointnparticipantsactive)
#' )
#' oovecdoubletAabsolute=c( #the start time, i.e., when the subject enters the study.
#'   base::seq(from=0.0,to=2.0,length.out=oointnparticipantsplacebo),
#'   base::seq(from=0.0,to=2.0,length.out=oointnparticipantsactive)
#' )
#' #the duration of time from when the subject enters the study until the subject experiences the event
#' oovecdoubletAtoB=c(
#'   stats::qexp(
#'     base::seq(from=0.0,to=0.98,length.out=oointnparticipantsplacebo),
#'     rate=oodoublerateplacebo
#'   ),
#'   stats::qexp(
#'     base::seq(from=0.0,to=0.98,length.out=oointnparticipantsactive),
#'     rate=oodoublerateactive
#'   )
#' )
#' oovecdoubletBabsolute=oovecdoubletAabsolute + oovecdoubletAtoB
#' #the analysis takes place at absolute time 6.0 months, and no other censoring (e.g., dropout) occurs
#' oovecdoubletCabsolute=6.0
#' oovecdoubletminBvsC=base::pmin(oovecdoubletBabsolute,oovecdoubletCabsolute)
#' oovecboolobservedB=(oovecdoubletBabsolute < oovecdoubletCabsolute)
#' oovecboolobservedC=(oovecdoubletCabsolute <= oovecdoubletBabsolute)
#' 
#' oodataframe=dplyr::tibble(id=1L:oointnparticipants,
#'                           treated=oovecinttreated,
#'                           Atime=oovecdoubletAabsolute,
#'                           Btime=oovecdoubletminBvsC,
#'                           Bobserved=oovecboolobservedB,
#'                           Ctime=oovecdoubletminBvsC,
#'                           Cobserved=oovecboolobservedC)
#'  
#' #standardized log-rank test statistic
#' oolistweightingfunctionsJustLogrank=base::list(
#'   logrank=function(stminus){ base::return(1.0) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustLogrank
#' ) #test statistic 2.92
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustLogrank
#' ) #p-value 0.0017
#' maxcombo::oogetdoublemaxcombocutoff(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustLogrank,
#'   oodoublealpha = 0.025
#' ) #cutoff of 1.96 for the max-combo test statistic
#' #the max-combo test statistic exceeds the cutoff (since 2.92 > 1.96), so you can declare that
#' #the survival curves in the two arms are statistically significantly different at the 0.025 level
#' 
#' #standardized weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function,
#' #which places greater weight on later times
#' oolistweightingfunctionsJustFlemingHarrington01=base::list(
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington01
#' ) #test statistic 2.83
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington01
#' ) #p-value 0.0023
#' maxcombo::oogetdoublemaxcombocutoff(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington01,
#'   oodoublealpha = 0.025
#' ) #cutoff of 1.96 for the max-combo test statistic
#' #the max-combo test statistic exceeds the cutoff (since 2.83 > 1.96), so you can declare that
#' #the survival curves in the two arms are statistically significantly different at the 0.025 level
#' 
#' #standardized weighted log-rank test statistic with Fleming-Harrington 1-0 weighting function,
#' #which places greater weight on earlier times
#' oolistweightingfunctionsJustFlemingHarrington10=base::list(
#'   flemingharrington10=function(stminus){ base::return(stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington10
#' ) #test statistic 2.71
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington10
#' ) #p-value 0.0033
#' maxcombo::oogetdoublemaxcombocutoff(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington10,
#'   oodoublealpha = 0.025
#' ) #cutoff of 1.96 for the max-combo test statistic
#' #the max-combo test statistic exceeds the cutoff (since 2.71 > 1.96), so you can declare that
#' #the survival curves in the two arms are statistically significantly different at the 0.025 level
#'  
#' #the max-combo test statistic based on the first two of the above
#' oolistweightingfunctionsLogrankAndFlemingHarrington01=base::list(
#'   logrank=function(stminus){ base::return(1.0) },
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsLogrankAndFlemingHarrington01
#' ) #test statistic 2.92, i.e., just the maximum of 2.92 (from the log-rank test statistic) and 2.83
#' # (from the weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function)
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsLogrankAndFlemingHarrington01
#' ) #p-value 0.0028
#' maxcombo::oogetdoublemaxcombocutoff(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsLogrankAndFlemingHarrington01,
#'   oodoublealpha = 0.025
#' ) #cutoff of 2.13 for the max-combo test statistic
#' #the max-combo test statistic exceeds the cutoff (since 2.92 > 2.13), so you can declare that
#' #the survival curves in the two arms are statistically significantly different at the 0.025 level
#' 
#' #the max-combo test statistic based on the first three of the above
#' oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10=base::list(
#'   logrank=function(stminus){ base::return(1.0) },
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) },
#'   flemingharrington10=function(stminus){ base::return(stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = 
#'   oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10
#' ) #test statistic 2.92, i.e., just the maximum of 2.92 (from the log-rank test statistic), 2.83
#' # (from the weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function), and
#' # 2.71 (from the weighted log-rank test statistic with Fleming-Harrington 1-0 weighting function)
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = 
#'   oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10
#' ) #p-value 0.0032
#' maxcombo::oogetdoublemaxcombocutoff(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = 
#'   oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10,
#'   oodoublealpha = 0.025,
#'   oointnmaxiter = 100L
#' ) #cutoff of 2.19 for the max-combo test statistic
#' #the max-combo test statistic exceeds the cutoff (since 2.92 > 2.19), so you can declare that
#' #the survival curves in the two arms are statistically significantly different at the 0.025 level
#' 
#' 
#' # --------------------------------------------------------------------------------------------
#' # Example 2: Usage on a single deterministic dataset in which the drug delays
#' # the event by exactly one month for each subject (i.e., an early treatment effect situation)
#' # --------------------------------------------------------------------------------------------
#' 
#' oointnparticipants=100L
#' oointnparticipantsplacebo=oointnparticipants/2L
#' oointnparticipantsactive=oointnparticipants/2L
#' 
#' oodoublerateplacebo=0.250
#' 
#' oovecinttreated=c(
#'   base::rep(0L,length.out=oointnparticipantsplacebo),
#'   base::rep(1L,length.out=oointnparticipantsactive)
#' )
#' oovecdoubletAabsolute=c( #the start time, i.e., when the subject enters the study.
#'   base::seq(from=0.0,to=2.0,length.out=oointnparticipantsplacebo),
#'   base::seq(from=0.0,to=2.0,length.out=oointnparticipantsactive)
#' )
#' #the duration of time from when the subject enters the study until the subject experiences the event
#' oovecdoubletAtoB=c(
#'   stats::qexp(
#'     base::seq(from=0.0,to=0.98,length.out=oointnparticipantsplacebo),
#'     rate=oodoublerateplacebo
#'   ),
#'   stats::qexp(
#'     base::seq(from=0.0,to=0.98,length.out=oointnparticipantsactive),
#'     rate=oodoublerateplacebo
#'   ) + 1.0 #note the addition of 1.0 month time to event here for the active arm
#' )
#' oovecdoubletBabsolute=oovecdoubletAabsolute + oovecdoubletAtoB
#' #the analysis takes place at absolute time 6.0 months, and no other censoring (e.g., dropout) occurs
#' oovecdoubletCabsolute=6.0
#' oovecdoubletminBvsC=base::pmin(oovecdoubletBabsolute,oovecdoubletCabsolute)
#' oovecboolobservedB=(oovecdoubletBabsolute < oovecdoubletCabsolute)
#' oovecboolobservedC=(oovecdoubletCabsolute <= oovecdoubletBabsolute)
#' 
#' oodataframe=dplyr::tibble(id=1L:oointnparticipants,
#'                           treated=oovecinttreated,
#'                           Atime=oovecdoubletAabsolute,
#'                           Btime=oovecdoubletminBvsC,
#'                           Bobserved=oovecboolobservedB,
#'                           Ctime=oovecdoubletminBvsC,
#'                           Cobserved=oovecboolobservedC)
#' 
#' #standardized log-rank test statistic
#' oolistweightingfunctionsJustLogrank=base::list(
#'   logrank=function(stminus){ base::return(1.0) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustLogrank
#' ) #test statistic 1.66
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustLogrank
#' ) #p-value 0.05
#' maxcombo::oogetdoublemaxcombocutoff(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustLogrank,
#'   oodoublealpha = 0.025
#' ) #cutoff of 1.96 for the max-combo test statistic
#' #the max-combo test statistic does not exceed the cutoff (since 1.66 < 1.96), so you fail to reject
#' #that the survival curves in the two arms are the same at the 0.025 level
#' 
#' #standardized weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function,
#' #which places greater weight on later times
#' oolistweightingfunctionsJustFlemingHarrington01=base::list(
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington01
#' ) #test statistic 0.53
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington01
#' ) #p-value 0.30
#' maxcombo::oogetdoublemaxcombocutoff(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington01,
#'   oodoublealpha = 0.025
#' ) #cutoff of 1.96 for the max-combo test statistic
#' #the max-combo test statistic does not exceed the cutoff (since 0.53 < 1.96), so you fail to reject
#' #that the survival curves in the two arms are the same at the 0.025 level
#' 
#' #standardized weighted log-rank test statistic with Fleming-Harrington 1-0 weighting function,
#' #which places greater weight on earlier times
#' oolistweightingfunctionsJustFlemingHarrington10=base::list(
#'   flemingharrington10=function(stminus){ base::return(stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington10
#' ) #test statistic 2.07
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington10
#' ) #p-value 0.02
#' maxcombo::oogetdoublemaxcombocutoff(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington10,
#'   oodoublealpha = 0.025
#' ) #cutoff of 1.96 for the max-combo test statistic
#' #the max-combo test statistic exceeds the cutoff (since 2.07 > 1.96), so you can declare that
#' #the survival curves in the two arms are statistically significantly different at the 0.025 level
#' 
#' #the max-combo test statistic based on the first two of the above
#' oolistweightingfunctionsLogrankAndFlemingHarrington01=base::list(
#'   logrank=function(stminus){ base::return(1.0) },
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsLogrankAndFlemingHarrington01
#' ) #test statistic 1.66, i.e., just the maximum of 1.66 (from the log-rank test statistic) and 0.53
#' # (from the weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function)
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsLogrankAndFlemingHarrington01
#' ) #p-value 0.07
#' maxcombo::oogetdoublemaxcombocutoff(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsLogrankAndFlemingHarrington01,
#'   oodoublealpha = 0.025
#' ) #cutoff of 2.13 for the max-combo test statistic
#' #the max-combo test statistic does not exceed the cutoff (since 1.66 < 2.13), so you fail to reject
#' #that the survival curves in the two arms are the same at the 0.025 level
#' 
#' #the max-combo test statistic based on the first three of the above
#' oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10=base::list(
#'   logrank=function(stminus){ base::return(1.0) },
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) },
#'   flemingharrington10=function(stminus){ base::return(stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus =
#'   oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10
#' ) #test statistic 2.07, i.e., just the maximum of 1.66 (from the log-rank test statistic), 0.53
#' # (from the weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function), and
#' # 2.07 (from the weighted log-rank test statistic with Fleming-Harrington 1-0 weighting function)
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = 
#'   oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10
#' ) #p-value 0.03
#' maxcombo::oogetdoublemaxcombocutoff(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = 
#'   oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10,
#'   oodoublealpha = 0.025,
#'   oointnmaxiter = 100L
#' ) #cutoff of 2.20 for the max-combo test statistic
#' #the max-combo test statistic does not exceed the cutoff (since 2.06 < 2.20), so you fail to reject
#' #that the survival curves in the two arms are the same at the 0.025 level
#' 
#'
#' # -------------------------------------------------------------------------------------------------
#' # Example 3: Usage on a single deterministic dataset in which subjects in the placebo arm all have
#' # the event after being on the study for 1.2 months (i.e., a delayed treatment effect situation)
#' # -------------------------------------------------------------------------------------------------
#'  
#' oointnparticipants=100L
#' oointnparticipantsplacebo=oointnparticipants/2L
#' oointnparticipantsactive=oointnparticipants/2L
#' 
#' oodoublerateactive=0.250
#' 
#' oovecinttreated=c(
#'   base::rep(0L,length.out=oointnparticipantsplacebo),
#'   base::rep(1L,length.out=oointnparticipantsactive)
#' )
#' oovecdoubletAabsolute=c( #the start time, i.e., when the subject enters the study.
#'   base::seq(from=0.0,to=2.0,length.out=oointnparticipantsplacebo),
#'   base::seq(from=0.0,to=2.0,length.out=oointnparticipantsactive)
#' )
#' #the duration of time from when the subject enters the study until the subject experiences the event
#' oovecdoubletAtoB=c(
#'   base::ifelse(
#'     stats::qexp(
#'       base::seq(from=0.0,to=0.98,length.out=oointnparticipantsplacebo),
#'       rate=oodoublerateactive
#'     ) <= 1.2,
#'     stats::qexp(
#'       base::seq(from=0.0,to=0.98,length.out=oointnparticipantsplacebo),
#'       rate=oodoublerateactive
#'     ),
#'     1.2
#'   ),
#'   stats::qexp(
#'     base::seq(from=0.0,to=0.98,length.out=oointnparticipantsactive),
#'     rate=oodoublerateactive
#'   )
#' )
#' oovecdoubletBabsolute=oovecdoubletAabsolute + oovecdoubletAtoB
#' #the analysis takes place at absolute time 6.0 months, and no other censoring (e.g., dropout) occurs
#' oovecdoubletCabsolute=6.0
#' oovecdoubletminBvsC=base::pmin(oovecdoubletBabsolute,oovecdoubletCabsolute)
#' oovecboolobservedB=(oovecdoubletBabsolute < oovecdoubletCabsolute)
#' oovecboolobservedC=(oovecdoubletCabsolute <= oovecdoubletBabsolute)
#' 
#' oodataframe=dplyr::tibble(id=1L:oointnparticipants,
#'                           treated=oovecinttreated,
#'                           Atime=oovecdoubletAabsolute,
#'                           Btime=oovecdoubletminBvsC,
#'                           Bobserved=oovecboolobservedB,
#'                           Ctime=oovecdoubletminBvsC,
#'                           Cobserved=oovecboolobservedC)
#' 
#' #standardized log-rank test statistic
#' oolistweightingfunctionsJustLogrank=base::list(
#'   logrank=function(stminus){ base::return(1.0) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustLogrank
#' ) #test statistic 1.55
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustLogrank
#' ) #p-value 0.06
#' maxcombo::oogetdoublemaxcombocutoff(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustLogrank,
#'   oodoublealpha = 0.025
#' ) #cutoff of 1.96 for the max-combo test statistic
#' #the max-combo test statistic does not exceed the cutoff (since 1.55 < 1.96), so you fail to reject
#' #that the survival curves in the two arms are the same at the 0.025 level
#' 
#' #standardized weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function,
#' #which places greater weight on later times
#' oolistweightingfunctionsJustFlemingHarrington01=base::list(
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington01
#' ) #test statistic 2.28
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington01
#' ) #p-value 0.01
#' maxcombo::oogetdoublemaxcombocutoff(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington01,
#'   oodoublealpha = 0.025
#' ) #cutoff of 1.96 for the max-combo test statistic
#' #the max-combo test statistic exceeds the cutoff (since 2.28 > 1.96), so you can declare that
#' #the survival curves in the two arms are statistically significantly different at the 0.025 level
#' 
#' #standardized weighted log-rank test statistic with Fleming-Harrington 1-0 weighting function,
#' #which places greater weight on earlier times
#' oolistweightingfunctionsJustFlemingHarrington10=base::list(
#'   flemingharrington10=function(stminus){ base::return(stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington10
#' ) #test statistic 1.35
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington10
#' ) #p-value 0.09
#' maxcombo::oogetdoublemaxcombocutoff(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsJustFlemingHarrington10,
#'   oodoublealpha = 0.025
#' ) #cutoff of 1.96 for the max-combo test statistic
#' #the max-combo test statistic does not exceed the cutoff (since 1.35 < 1.96), so you fail to reject
#' #that the survival curves in the two arms are the same at the 0.025 level
#' 
#' #the max-combo test statistic based on the first two of the above
#' oolistweightingfunctionsLogrankAndFlemingHarrington01=base::list(
#'   logrank=function(stminus){ base::return(1.0) },
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsLogrankAndFlemingHarrington01
#' ) #test statistic 2.28, i.e., just the maximum of 1.55 (from the log-rank test statistic) and 2.28
#' # (from the weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function)
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsLogrankAndFlemingHarrington01
#' ) #p-value 0.02
#' maxcombo::oogetdoublemaxcombocutoff(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = oolistweightingfunctionsLogrankAndFlemingHarrington01,
#'   oodoublealpha = 0.025
#' ) #cutoff of 2.13 for the max-combo test statistic
#' #the max-combo test statistic exceeds the cutoff (since 2.28 > 2.13), so you can declare that
#' #the survival curves in the two arms are statistically significantly different at the 0.025 level
#' 
#' #the max-combo test statistic based on the first three of the above
#' oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10=base::list(
#'   logrank=function(stminus){ base::return(1.0) },
#'   flemingharrington01=function(stminus){ base::return(1.0 - stminus) },
#'   flemingharrington10=function(stminus){ base::return(stminus) }
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus =
#'   oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10
#' ) #test statistic 2.28, i.e., just the maximum of 1.55 (from the log-rank test statistic), 2.28
#' # (from the weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function), and
#' # 1.35 (from the weighted log-rank test statistic with Fleming-Harrington 1-0 weighting function)
#' maxcombo::oogetdoublemaxcombotestpvalue(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = 
#'   oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10
#' ) #p-value 0.02
#' maxcombo::oogetdoublemaxcombocutoff(
#'   oodataframe = oodataframe,
#'   oolistfunctionweightasafunctionofstminus = 
#'   oolistweightingfunctionsLogrankAndFlemingHarrington01AndFlemingHarrington10,
#'   oodoublealpha = 0.025,
#'   oointnmaxiter = 200L
#' ) #cutoff of 2.15 for the max-combo test statistic
#' #the max-combo test statistic exceeds the cutoff (since 2.28 > 2.15), so you can declare that
#' #the survival curves in the two arms are statistically significantly different at the 0.025 level
#' }
NULL








#### EXAMPLES ADDED TO DOCUMENTATION FOR oogetdoublemaxcombocutoffgroupsequential ####
#' @rdname oogetdoublemaxcombocutoffgroupsequential
#' @name oogetdoublemaxcombocutoffgroupsequential
#' @examples
#' \donttest{
#' 
#' #this function is used in the examples below to create synthetic earlier looks
#' #at the simulated datasets.
#' oogetdataframeearlierlookattime=function(oodataframe,oodoubletime)
#' {
#'   dplyr::mutate(
#'     dplyr::filter(oodataframe,Atime <= oodoubletime),
#'     Bobserved = base::ifelse(Btime <= oodoubletime,Bobserved,FALSE),
#'     Cobserved = base::ifelse(Ctime <= oodoubletime,Cobserved,FALSE),
#'     Btime = base::pmin(Btime,oodoubletime),
#'     Ctime = base::pmin(Ctime,oodoubletime)
#'   )
#' }
#' 
#' # -------------------------------------------------------------------------------
#' # Example 1: Usage on a single deterministic dataset in which the drug halves
#' # the hazard at all times (i.e., a proportional hazards situation)
#' # -------------------------------------------------------------------------------
#' 
#' oointnparticipants=100L
#' oointnparticipantsplacebo=oointnparticipants/2L
#' oointnparticipantsactive=oointnparticipants/2L
#' 
#' oodoublerateplacebo=0.250
#' oodoublerateactive=0.125
#' 
#' oovecinttreated=c(
#'   base::rep(0L,length.out=oointnparticipantsplacebo),
#'   base::rep(1L,length.out=oointnparticipantsactive)
#' )
#' oovecdoubletAabsolute=c( #the start time, i.e., when the subject enters the study.
#'   base::seq(from=0.0,to=2.0,length.out=oointnparticipantsplacebo),
#'   base::seq(from=0.0,to=2.0,length.out=oointnparticipantsactive)
#' )
#' #the duration of time from when the subject enters the study until the subject experiences the event
#' oovecdoubletAtoB=c(
#'   stats::qexp(
#'     base::seq(from=0.0,to=0.98,length.out=oointnparticipantsplacebo),
#'     rate=oodoublerateplacebo
#'   ),
#'   stats::qexp(
#'     base::seq(from=0.0,to=0.98,length.out=oointnparticipantsactive),
#'     rate=oodoublerateactive
#'   )
#' )
#' oovecdoubletBabsolute=oovecdoubletAabsolute + oovecdoubletAtoB
#' #the final analysis takes place at absolute time 6.0 months, and no other
#' #censoring (e.g., dropout) occurs
#' oovecdoubletCabsolute=6.0
#' oovecdoubletminBvsC=base::pmin(oovecdoubletBabsolute,oovecdoubletCabsolute)
#' oovecboolobservedB=(oovecdoubletBabsolute < oovecdoubletCabsolute)
#' oovecboolobservedC=(oovecdoubletCabsolute <= oovecdoubletBabsolute)
#' 
#' oodataframe=dplyr::tibble(id=1L:oointnparticipants,
#'                           treated=oovecinttreated,
#'                           Atime=oovecdoubletAabsolute,
#'                           Btime=oovecdoubletminBvsC,
#'                           Bobserved=oovecboolobservedB,
#'                           Ctime=oovecdoubletminBvsC,
#'                           Cobserved=oovecboolobservedC)
#' 
#' oodataframeinterimanalysis=oogetdataframeearlierlookattime(
#'   oodataframe = oodataframe,
#'   oodoubletime = 3.0 #the interim analysis takes place at absolute time 3.0 months
#' )
#' oodataframefinalanalysis=oodataframe #the final analysis takes place at absolute time 6.0 months
#' 
#' 
#' 
#' #plan to use the standardized log-rank test statistic at both the interim and the final analysis
#' oolistlistweightingfunctionsbytimepoint=base::list(
#'   interim=base::list(
#'     logrank=function(stminus){ base::return(1.0) }
#'   ),
#'   final=base::list(
#'     logrank=function(stminus){ base::return(1.0) }
#'   )
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframeinterimanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[1L]]
#' ) #max-combo test statistic 2.36 at the interim analysis
#' oodoublecutoffforinterimanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:1L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = base::vector(mode="double",length=0L),
#'   oodoublealphaincrement = 0.06*0.025
#' )
#' oodoublecutoffforinterimanalysis #cutoff of 2.97 for the max-combo test statistic at
#' #the interim analysis
#' #the max-combo test statistic at the interim analysis does not exceed the cutoff (since 2.36 < 2.97)
#' #so the experiment continues until the final analysis.
#' 
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframefinalanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[2L]]
#' ) #max-combo test statistic 2.92 at the final analysis
#' oodoublecutoffforfinalanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis,oodataframefinalanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:2L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = c(oodoublecutoffforinterimanalysis),
#'   oodoublealphaincrement = 0.025 - 0.06*0.025
#' )
#' oodoublecutoffforfinalanalysis #cutoff of 1.96 for the max-combo test statistic at
#' #the final analysis
#' #the max-combo test statistic at the final analysis exceeds the cutoff (since 2.92 > 1.96),
#' #so you can declare that the survival curves in the two arms are statistically significantly
#' #different at the 0.025 level
#' 
#' 
#' 
#' #plan to use the standardized log-rank test statistic at the interim analysis and to use the
#' #max-combo test statistic based on the standardized log-rank test statistic and the weighted
#' #log-rank test statistic with Fleming-Harrington 0-1 weighting function, which places greater
#' #weight on later times, at the final analysis
#' oolistlistweightingfunctionsbytimepoint=base::list(
#'   interim=base::list(
#'     logrank=function(stminus){ base::return(1.0) }
#'   ),
#'   final=base::list(
#'     logrank=function(stminus){ base::return(1.0) },
#'     flemingharrington01=function(stminus){ base::return(1.0 - stminus) }
#'   )
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframeinterimanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[1L]]
#' ) #max-combo test statistic 2.36 at the interim analysis
#' oodoublecutoffforinterimanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:1L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = base::vector(mode="double",length=0L),
#'   oodoublealphaincrement = 0.06*0.025
#' )
#' oodoublecutoffforinterimanalysis #cutoff of 2.97 for the max-combo test statistic at
#' #the interim analysis
#' #the max-combo test statistic at the interim analysis does not exceed the cutoff (since 2.36 < 2.97)
#' #so the experiment continues until the final analysis.
#' 
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframefinalanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[2L]]
#' ) #max-combo test statistic 2.92 at the final analysis
#' oodoublecutoffforfinalanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis,oodataframefinalanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:2L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = c(oodoublecutoffforinterimanalysis),
#'   oodoublealphaincrement = 0.025 - 0.06*0.025
#' )
#' oodoublecutoffforfinalanalysis #cutoff of 2.12 for the max-combo test statistic at
#' #the final analysis
#' #the max-combo test statistic at the final analysis exceeds the cutoff (since 2.92 > 2.12),
#' #so you can declare that the survival curves in the two arms are statistically significantly
#' #different at the 0.025 level
#' 
#' 
#' 
#' #plan to use the standardized log-rank test statistic at the interim analysis and to use the
#' #max-combo test statistic based on the standardized log-rank test statistic, the weighted
#' #log-rank test statistic with Fleming-Harrington 0-1 weighting function, which places greater
#' #weight on later times, and the weighted log-rank test statistic with Fleming-Harrington 1-0
#' #weighting function, which places greater weight on earlier times, at the final analysis
#' oolistlistweightingfunctionsbytimepoint=base::list(
#'   interim=base::list(
#'     logrank=function(stminus){ base::return(1.0) }
#'   ),
#'   final=base::list(
#'     logrank=function(stminus){ base::return(1.0) },
#'     flemingharrington01=function(stminus){ base::return(1.0 - stminus) },
#'     flemingharrington10=function(stminus){ base::return(stminus) }
#'   )
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframeinterimanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[1L]]
#' ) #max-combo test statistic 2.36 at the interim analysis
#' oodoublecutoffforinterimanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:1L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = base::vector(mode="double",length=0L),
#'   oodoublealphaincrement = 0.06*0.025
#' )
#' oodoublecutoffforinterimanalysis #cutoff of 2.97 for the max-combo test statistic at
#' #the interim analysis
#' #the max-combo test statistic at the interim analysis does not exceed the cutoff (since 2.36 < 2.97)
#' #so the experiment continues until the final analysis.
#' 
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframefinalanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[2L]]
#' ) #max-combo test statistic 2.92 at the final analysis
#' oodoublecutoffforfinalanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis,oodataframefinalanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:2L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = c(oodoublecutoffforinterimanalysis),
#'   oodoublealphaincrement = 0.025 - 0.06*0.025
#' )
#' oodoublecutoffforfinalanalysis #cutoff of 2.19 for the max-combo test statistic at
#' #the final analysis
#' #the max-combo test statistic at the final analysis exceeds the cutoff (since 2.92 > 2.19),
#' #so you can declare that the survival curves in the two arms are statistically significantly
#' #different at the 0.025 level
#' 
#' 
#' 
#' #plan to use the standardized log-rank test statistic and the weighted log-rank test
#' #statistic with Fleming-Harrington 0-1 weighting function at the interim analysis and to use
#' #the max-combo test statistic based on the standardized log-rank test statistic, the weighted
#' #log-rank test statistic with Fleming-Harrington 0-1 weighting function, which places greater weight
#' #on later times, and the weighted log-rank test statistic with Fleming-Harrington 1-0 weighting
#' #function, which places greater weight on earlier times, at the final analysis
#' oolistlistweightingfunctionsbytimepoint=base::list(
#'   interim=base::list(
#'     logrank=function(stminus){ base::return(1.0) },
#'     flemingharrington01=function(stminus){ base::return(1.0 - stminus) }
#'   ),
#'   final=base::list(
#'     logrank=function(stminus){ base::return(1.0) },
#'     flemingharrington01=function(stminus){ base::return(1.0 - stminus) },
#'     flemingharrington10=function(stminus){ base::return(stminus) }
#'   )
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframeinterimanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[1L]]
#' ) #max-combo test statistic 2.56 at the interim analysis
#' oodoublecutoffforinterimanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:1L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = base::vector(mode="double",length=0L),
#'   oodoublealphaincrement = 0.06*0.025
#' )
#' oodoublecutoffforinterimanalysis #cutoff of 3.13 for the max-combo test statistic at
#' #the interim analysis
#' #the max-combo test statistic at the interim analysis does not exceed the cutoff (since 2.56 < 3.13)
#' #so the experiment continues until the final analysis.
#' 
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframefinalanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[2L]]
#' ) #max-combo test statistic 2.92 at the final analysis
#' oodoublecutoffforfinalanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis,oodataframefinalanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:2L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = c(oodoublecutoffforinterimanalysis),
#'   oodoublealphaincrement = 0.025 - 0.06*0.025
#' )
#' oodoublecutoffforfinalanalysis #cutoff of 2.20 for the max-combo test statistic at
#' #the final analysis
#' #the max-combo test statistic at the final analysis exceeds the cutoff (since 2.92 > 2.20),
#' #so you can declare that the survival curves in the two arms are statistically significantly
#' #different at the 0.025 level
#' 
#' 
#' # --------------------------------------------------------------------------------------------
#' # Example 2: Usage on a single deterministic dataset in which the drug delays
#' # the event by exactly one month for each subject (i.e., an early treatment effect situation)
#' # --------------------------------------------------------------------------------------------
#' 
#' oointnparticipants=100L
#' oointnparticipantsplacebo=oointnparticipants/2L
#' oointnparticipantsactive=oointnparticipants/2L
#' 
#' oodoublerateplacebo=0.250
#' 
#' oovecinttreated=c(
#'   base::rep(0L,length.out=oointnparticipantsplacebo),
#'   base::rep(1L,length.out=oointnparticipantsactive)
#' )
#' oovecdoubletAabsolute=c( #the start time, i.e., when the subject enters the study.
#'   base::seq(from=0.0,to=2.0,length.out=oointnparticipantsplacebo),
#'   base::seq(from=0.0,to=2.0,length.out=oointnparticipantsactive)
#' )
#' #the duration of time from when the subject enters the study until the subject experiences the event
#' oovecdoubletAtoB=c(
#'   stats::qexp(
#'     base::seq(from=0.0,to=0.98,length.out=oointnparticipantsplacebo),
#'     rate=oodoublerateplacebo
#'   ),
#'   stats::qexp(
#'     base::seq(from=0.0,to=0.98,length.out=oointnparticipantsactive),
#'     rate=oodoublerateplacebo
#'   ) + 1.0 #note the addition of 1.0 month time to event here for the active arm
#' )
#' oovecdoubletBabsolute=oovecdoubletAabsolute + oovecdoubletAtoB
#' #the analysis takes place at absolute time 6.0 months, and no other censoring (e.g., dropout) occurs
#' oovecdoubletCabsolute=6.0
#' oovecdoubletminBvsC=base::pmin(oovecdoubletBabsolute,oovecdoubletCabsolute)
#' oovecboolobservedB=(oovecdoubletBabsolute < oovecdoubletCabsolute)
#' oovecboolobservedC=(oovecdoubletCabsolute <= oovecdoubletBabsolute)
#' 
#' oodataframe=dplyr::tibble(id=1L:oointnparticipants,
#'                           treated=oovecinttreated,
#'                           Atime=oovecdoubletAabsolute,
#'                           Btime=oovecdoubletminBvsC,
#'                           Bobserved=oovecboolobservedB,
#'                           Ctime=oovecdoubletminBvsC,
#'                           Cobserved=oovecboolobservedC)
#' 
#' oodataframeinterimanalysis=oogetdataframeearlierlookattime(
#'   oodataframe = oodataframe,
#'   oodoubletime = 3.0 #the interim analysis takes place at absolute time 3.0 months
#' )
#' oodataframefinalanalysis=oodataframe #the final analysis takes place at absolute time 6.0 months
#' 
#' 
#' 
#' #plan to use the standardized log-rank test statistic at both the interim and the final analysis
#' oolistlistweightingfunctionsbytimepoint=base::list(
#'   interim=base::list(
#'     logrank=function(stminus){ base::return(1.0) }
#'   ),
#'   final=base::list(
#'     logrank=function(stminus){ base::return(1.0) }
#'   )
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframeinterimanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[1L]]
#' ) #max-combo test statistic 2.51 at the interim analysis
#' oodoublecutoffforinterimanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:1L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = base::vector(mode="double",length=0L),
#'   oodoublealphaincrement = 0.06*0.025
#' )
#' oodoublecutoffforinterimanalysis #cutoff of 2.97 for the max-combo test statistic at
#' #the interim analysis
#' #the max-combo test statistic at the interim analysis does not exceed the cutoff (since 2.51 < 2.97)
#' #so the experiment continues until the final analysis.
#' 
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframefinalanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[2L]]
#' ) #max-combo test statistic 1.66 at the final analysis
#' oodoublecutoffforfinalanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis,oodataframefinalanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:2L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = c(oodoublecutoffforinterimanalysis),
#'   oodoublealphaincrement = 0.025 - 0.06*0.025
#' )
#' oodoublecutoffforfinalanalysis #cutoff of 1.96 for the max-combo test statistic at
#' #the final analysis
#' #the max-combo test statistic at the final analysis does not exceed the cutoff
#' #(since 1.66 < 1.96), so you fail to reject that the survival curves in the two arms are
#' #the same at the 0.025 level
#' 
#' 
#' 
#' #plan to use the standardized log-rank test statistic at the interim analysis and to use the
#' #max-combo test statistic based on the standardized log-rank test statistic and the weighted
#' #log-rank test statistic with Fleming-Harrington 0-1 weighting function, which places greater
#' #weight on later times, at the final analysis
#' oolistlistweightingfunctionsbytimepoint=base::list(
#'   interim=base::list(
#'     logrank=function(stminus){ base::return(1.0) }
#'   ),
#'   final=base::list(
#'     logrank=function(stminus){ base::return(1.0) },
#'     flemingharrington01=function(stminus){ base::return(1.0 - stminus) }
#'   )
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframeinterimanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[1L]]
#' ) #max-combo test statistic 2.51 at the interim analysis
#' oodoublecutoffforinterimanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:1L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = base::vector(mode="double",length=0L),
#'   oodoublealphaincrement = 0.06*0.025
#' )
#' oodoublecutoffforinterimanalysis #cutoff of 2.97 for the max-combo test statistic at
#' #the interim analysis
#' #the max-combo test statistic at the interim analysis does not exceed the cutoff (since 2.51 < 2.97)
#' #so the experiment continues until the final analysis.
#' 
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframefinalanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[2L]]
#' ) #max-combo test statistic 1.66 at the final analysis
#' oodoublecutoffforfinalanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis,oodataframefinalanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:2L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = c(oodoublecutoffforinterimanalysis),
#'   oodoublealphaincrement = 0.025 - 0.06*0.025
#' )
#' oodoublecutoffforfinalanalysis #cutoff of 2.12 for the max-combo test statistic at
#' #the final analysis
#' #the max-combo test statistic at the final analysis does not exceed the cutoff
#' #(since 1.66 < 2.12), so you fail to reject that the survival curves in the two arms are
#' #the same at the 0.025 level
#' 
#' 
#' 
#' #plan to use the standardized log-rank test statistic at the interim analysis and to use the
#' #max-combo test statistic based on the standardized log-rank test statistic, the weighted
#' #log-rank test statistic with Fleming-Harrington 0-1 weighting function, which places greater
#' #weight on later times, and the weighted log-rank test statistic with Fleming-Harrington 1-0
#' #weighting function, which places greater weight on earlier times, at the final analysis
#' oolistlistweightingfunctionsbytimepoint=base::list(
#'   interim=base::list(
#'     logrank=function(stminus){ base::return(1.0) }
#'   ),
#'   final=base::list(
#'     logrank=function(stminus){ base::return(1.0) },
#'     flemingharrington01=function(stminus){ base::return(1.0 - stminus) },
#'     flemingharrington10=function(stminus){ base::return(stminus) }
#'   )
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframeinterimanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[1L]]
#' ) #max-combo test statistic 2.51 at the interim analysis
#' oodoublecutoffforinterimanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:1L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = base::vector(mode="double",length=0L),
#'   oodoublealphaincrement = 0.06*0.025
#' )
#' oodoublecutoffforinterimanalysis #cutoff of 2.97 for the max-combo test statistic at
#' #the interim analysis
#' #the max-combo test statistic at the interim analysis does not exceed the cutoff (since 2.51 < 2.97)
#' #so the experiment continues until the final analysis.
#' 
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframefinalanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[2L]]
#' ) #max-combo test statistic 2.07 at the final analysis
#' oodoublecutoffforfinalanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis,oodataframefinalanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:2L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = c(oodoublecutoffforinterimanalysis),
#'   oodoublealphaincrement = 0.025 - 0.06*0.025
#' )
#' oodoublecutoffforfinalanalysis #cutoff of 2.19 for the max-combo test statistic at
#' #the final analysis
#' #the max-combo test statistic at the final analysis does not exceed the cutoff
#' #(since 2.07 < 2.19), so you fail to reject that the survival curves in the two arms are
#' #the same at the 0.025 level
#' 
#' 
#' 
#' #plan to use the standardized log-rank test statistic and the weighted log-rank test
#' #statistic with Fleming-Harrington 0-1 weighting function, which places greater weight on
#' #later times, at the interim analysis and to use the max-combo test statistic based on
#' #the standardized log-rank test statistic, the weighted log-rank test statistic with
#' #Fleming-Harrington 0-1 weighting function, which places greater weight on later times,
#' #and the weighted log-rank test statistic with Fleming-Harrington 1-0 weighting
#' #function, which places greater weight on earlier times, at the final analysis
#' oolistlistweightingfunctionsbytimepoint=base::list(
#'   interim=base::list(
#'     logrank=function(stminus){ base::return(1.0) },
#'     flemingharrington01=function(stminus){ base::return(1.0 - stminus) }
#'   ),
#'   final=base::list(
#'     logrank=function(stminus){ base::return(1.0) },
#'     flemingharrington01=function(stminus){ base::return(1.0 - stminus) },
#'     flemingharrington10=function(stminus){ base::return(stminus) }
#'   )
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframeinterimanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[1L]]
#' ) #max-combo test statistic 2.51 at the interim analysis
#' oodoublecutoffforinterimanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:1L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = base::vector(mode="double",length=0L),
#'   oodoublealphaincrement = 0.06*0.025
#' )
#' oodoublecutoffforinterimanalysis #cutoff of 3.13 for the max-combo test statistic at
#' #the interim analysis
#' #the max-combo test statistic at the interim analysis does not exceed the cutoff (since 2.51 < 3.13)
#' #so the experiment continues until the final analysis.
#' 
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframefinalanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[2L]]
#' ) #max-combo test statistic 2.07 at the final analysis
#' oodoublecutoffforfinalanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis,oodataframefinalanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:2L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = c(oodoublecutoffforinterimanalysis),
#'   oodoublealphaincrement = 0.025 - 0.06*0.025
#' )
#' oodoublecutoffforfinalanalysis #cutoff of 2.20 for the max-combo test statistic at
#' #the final analysis
#' #the max-combo test statistic at the final analysis does not exceed the cutoff
#' #(since 2.07 < 2.20), so you fail to reject that the survival curves in the two arms are
#' #the same at the 0.025 level
#' 
#' 
#' #plan to use the standardized log-rank test statistic and the weighted log-rank test
#' #statistic with Fleming-Harrington 1-0 weighting function, which places greater weight on
#' #earlier times, at the interim analysis and to use the max-combo test statistic based on
#' #the standardized log-rank test statistic, the weighted log-rank test statistic with
#' #Fleming-Harrington 1-0 weighting function, which places greater weight on earlier times,
#' #at the final analysis
#' oolistlistweightingfunctionsbytimepoint=base::list(
#'   interim=base::list(
#'     logrank=function(stminus){ base::return(1.0) },
#'     flemingharrington10=function(stminus){ base::return(stminus) }
#'   ),
#'   final=base::list(
#'     logrank=function(stminus){ base::return(1.0) },
#'     flemingharrington10=function(stminus){ base::return(stminus) }
#'   )
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframeinterimanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[1L]]
#' ) #max-combo test statistic 2.71 at the interim analysis
#' oodoublecutoffforinterimanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:1L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = base::vector(mode="double",length=0L),
#'   oodoublealphaincrement = 0.06*0.025
#' )
#' oodoublecutoffforinterimanalysis #cutoff of 3.02 for the max-combo test statistic at
#' #the interim analysis
#' #the max-combo test statistic at the interim analysis does not exceed the cutoff (since 2.71 < 3.02)
#' #so the experiment continues until the final analysis.
#' 
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframefinalanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[2L]]
#' ) #max-combo test statistic 2.07 at the final analysis
#' oodoublecutoffforfinalanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis,oodataframefinalanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:2L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = c(oodoublecutoffforinterimanalysis),
#'   oodoublealphaincrement = 0.025 - 0.06*0.025
#' )
#' oodoublecutoffforfinalanalysis #cutoff of 2.06 for the max-combo test statistic at
#' #the final analysis
#' #the max-combo test statistic at the final analysis exceeds the cutoff (since 2.07 > 2.06),
#' #so you can declare that the survival curves in the two arms are statistically significantly
#' #different at the 0.025 level
#' 
#' 
#' # -------------------------------------------------------------------------------------------------
#' # Example 3: Usage on a single deterministic dataset in which subjects in the placebo arm all have
#' # the event after being on the study for 1.2 months (i.e., a delayed treatment effect situation)
#' # -------------------------------------------------------------------------------------------------
#'  
#' oointnparticipants=100L
#' oointnparticipantsplacebo=oointnparticipants/2L
#' oointnparticipantsactive=oointnparticipants/2L
#' 
#' oodoublerateactive=0.250
#' 
#' oovecinttreated=c(
#'   base::rep(0L,length.out=oointnparticipantsplacebo),
#'   base::rep(1L,length.out=oointnparticipantsactive)
#' )
#' oovecdoubletAabsolute=c( #the start time, i.e., when the subject enters the study.
#'   base::seq(from=0.0,to=2.0,length.out=oointnparticipantsplacebo),
#'   base::seq(from=0.0,to=2.0,length.out=oointnparticipantsactive)
#' )
#' #the duration of time from when the subject enters the study until the subject experiences the event
#' oovecdoubletAtoB=c(
#'   base::ifelse(
#'     stats::qexp(
#'       base::seq(from=0.0,to=0.98,length.out=oointnparticipantsplacebo),
#'       rate=oodoublerateactive
#'     ) <= 1.2,
#'     stats::qexp(
#'       base::seq(from=0.0,to=0.98,length.out=oointnparticipantsplacebo),
#'       rate=oodoublerateactive
#'     ),
#'     1.2
#'   ),
#'   stats::qexp(
#'     base::seq(from=0.0,to=0.98,length.out=oointnparticipantsactive),
#'     rate=oodoublerateactive
#'   )
#' )
#' oovecdoubletBabsolute=oovecdoubletAabsolute + oovecdoubletAtoB
#' #the analysis takes place at absolute time 6.0 months, and no other censoring (e.g., dropout) occurs
#' oovecdoubletCabsolute=6.0
#' oovecdoubletminBvsC=base::pmin(oovecdoubletBabsolute,oovecdoubletCabsolute)
#' oovecboolobservedB=(oovecdoubletBabsolute < oovecdoubletCabsolute)
#' oovecboolobservedC=(oovecdoubletCabsolute <= oovecdoubletBabsolute)
#' 
#' oodataframe=dplyr::tibble(id=1L:oointnparticipants,
#'                           treated=oovecinttreated,
#'                           Atime=oovecdoubletAabsolute,
#'                           Btime=oovecdoubletminBvsC,
#'                           Bobserved=oovecboolobservedB,
#'                           Ctime=oovecdoubletminBvsC,
#'                           Cobserved=oovecboolobservedC)
#' 
#' oodataframeinterimanalysis=oogetdataframeearlierlookattime(
#'   oodataframe = oodataframe,
#'   oodoubletime = 3.0 #the interim analysis takes place at absolute time 3.0 months
#' )
#' oodataframefinalanalysis=oodataframe #the final analysis takes place at absolute time 6.0 months
#' 
#' 
#' 
#' #plan to use the standardized log-rank test statistic at both the interim and the final analysis
#' oolistlistweightingfunctionsbytimepoint=base::list(
#'   interim=base::list(
#'     logrank=function(stminus){ base::return(1.0) }
#'   ),
#'   final=base::list(
#'     logrank=function(stminus){ base::return(1.0) }
#'   )
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframeinterimanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[1L]]
#' ) #max-combo test statistic 1.24 at the interim analysis
#' oodoublecutoffforinterimanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:1L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = base::vector(mode="double",length=0L),
#'   oodoublealphaincrement = 0.06*0.025
#' )
#' oodoublecutoffforinterimanalysis #cutoff of 2.97 for the max-combo test statistic at
#' #the interim analysis
#' #the max-combo test statistic at the interim analysis does not exceed the cutoff (since 1.24 < 2.97)
#' #so the experiment continues until the final analysis.
#' 
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframefinalanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[2L]]
#' ) #max-combo test statistic 1.55 at the final analysis
#' oodoublecutoffforfinalanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis,oodataframefinalanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:2L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = c(oodoublecutoffforinterimanalysis),
#'   oodoublealphaincrement = 0.025 - 0.06*0.025
#' )
#' oodoublecutoffforfinalanalysis #cutoff of 1.96 for the max-combo test statistic at
#' #the final analysis
#' #the max-combo test statistic at the final analysis does not exceed the cutoff
#' #(since 1.55 < 1.96), so you fail to reject that the survival curves in the two arms are
#' #the same at the 0.025 level
#' 
#' 
#' 
#' #plan to use the standardized log-rank test statistic at the interim analysis and to use the
#' #max-combo test statistic based on the standardized log-rank test statistic and the weighted
#' #log-rank test statistic with Fleming-Harrington 0-1 weighting function, which places greater
#' #weight on later times, at the final analysis
#' oolistlistweightingfunctionsbytimepoint=base::list(
#'   interim=base::list(
#'     logrank=function(stminus){ base::return(1.0) }
#'   ),
#'   final=base::list(
#'     logrank=function(stminus){ base::return(1.0) },
#'     flemingharrington01=function(stminus){ base::return(1.0 - stminus) }
#'   )
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframeinterimanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[1L]]
#' ) #max-combo test statistic 1.24 at the interim analysis
#' oodoublecutoffforinterimanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:1L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = base::vector(mode="double",length=0L),
#'   oodoublealphaincrement = 0.06*0.025
#' )
#' oodoublecutoffforinterimanalysis #cutoff of 2.97 for the max-combo test statistic at
#' #the interim analysis
#' #the max-combo test statistic at the interim analysis does not exceed the cutoff (since 1.24 < 2.97)
#' #so the experiment continues until the final analysis.
#' 
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframefinalanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[2L]]
#' ) #max-combo test statistic 2.28 at the final analysis
#' oodoublecutoffforfinalanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis,oodataframefinalanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:2L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = c(oodoublecutoffforinterimanalysis),
#'   oodoublealphaincrement = 0.025 - 0.06*0.025
#' )
#' oodoublecutoffforfinalanalysis #cutoff of 2.12 for the max-combo test statistic at
#' #the final analysis
#' #the max-combo test statistic at the final analysis exceeds the cutoff (since 2.28 > 2.12),
#' #so you can declare that the survival curves in the two arms are statistically significantly
#' #different at the 0.025 level
#' 
#' 
#' 
#' #plan to use the standardized log-rank test statistic at the interim analysis and to use the
#' #max-combo test statistic based on the standardized log-rank test statistic, the weighted
#' #log-rank test statistic with Fleming-Harrington 0-1 weighting function, which places greater
#' #weight on later times, and the weighted log-rank test statistic with Fleming-Harrington 1-0
#' #weighting function, which places greater weight on earlier times, at the final analysis
#' oolistlistweightingfunctionsbytimepoint=base::list(
#'   interim=base::list(
#'     logrank=function(stminus){ base::return(1.0) }
#'   ),
#'   final=base::list(
#'     logrank=function(stminus){ base::return(1.0) },
#'     flemingharrington01=function(stminus){ base::return(1.0 - stminus) },
#'     flemingharrington10=function(stminus){ base::return(stminus) }
#'   )
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframeinterimanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[1L]]
#' ) #max-combo test statistic 1.24 at the interim analysis
#' oodoublecutoffforinterimanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:1L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = base::vector(mode="double",length=0L),
#'   oodoublealphaincrement = 0.06*0.025
#' )
#' oodoublecutoffforinterimanalysis #cutoff of 2.97 for the max-combo test statistic at
#' #the interim analysis
#' #the max-combo test statistic at the interim analysis does not exceed the cutoff (since 1.24 < 2.97)
#' #so the experiment continues until the final analysis.
#' 
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframefinalanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[2L]]
#' ) #max-combo test statistic 2.28 at the final analysis
#' oodoublecutoffforfinalanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis,oodataframefinalanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:2L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = c(oodoublecutoffforinterimanalysis),
#'   oodoublealphaincrement = 0.025 - 0.06*0.025
#' )
#' oodoublecutoffforfinalanalysis #cutoff of 2.16 for the max-combo test statistic at
#' #the final analysis
#' #the max-combo test statistic at the final analysis exceeds the cutoff (since 2.28 > 2.16),
#' #so you can declare that the survival curves in the two arms are statistically significantly
#' #different at the 0.025 level
#' 
#' 
#' 
#' #plan to use the standardized log-rank test statistic and the weighted log-rank test
#' #statistic with Fleming-Harrington 0-1 weighting function, which places greater weight on
#' #later times, at the interim analysis and to use the max-combo test statistic based on
#' #the standardized log-rank test statistic, the weighted log-rank test statistic with
#' #Fleming-Harrington 0-1 weighting function, which places greater weight on later times,
#' #and the weighted log-rank test statistic with Fleming-Harrington 1-0 weighting
#' #function, which places greater weight on earlier times, at the final analysis
#' oolistlistweightingfunctionsbytimepoint=base::list(
#'   interim=base::list(
#'     logrank=function(stminus){ base::return(1.0) },
#'     flemingharrington01=function(stminus){ base::return(1.0 - stminus) }
#'   ),
#'   final=base::list(
#'     logrank=function(stminus){ base::return(1.0) },
#'     flemingharrington01=function(stminus){ base::return(1.0 - stminus) },
#'     flemingharrington10=function(stminus){ base::return(stminus) }
#'   )
#' )
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframeinterimanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[1L]]
#' ) #max-combo test statistic 1.88 at the interim analysis
#' oodoublecutoffforinterimanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:1L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = base::vector(mode="double",length=0L),
#'   oodoublealphaincrement = 0.06*0.025
#' )
#' oodoublecutoffforinterimanalysis #cutoff of 3.12 for the max-combo test statistic at
#' #the interim analysis
#' #the max-combo test statistic at the interim analysis does not exceed the cutoff (since 1.88 < 3.12)
#' #so the experiment continues until the final analysis.
#' 
#' maxcombo::oogetdoublemaxcomboteststatistic(
#'   oodataframe = oodataframefinalanalysis,
#'   oolistfunctionweightasafunctionofstminus = oolistlistweightingfunctionsbytimepoint[[2L]]
#' ) #max-combo test statistic 2.28 at the final analysis
#' oodoublecutoffforfinalanalysis=maxcombo::oogetdoublemaxcombocutoffgroupsequential(
#'   oolistdataframesbytimepoint = base::list(oodataframeinterimanalysis,oodataframefinalanalysis),
#'   oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint[1L:2L],
#'   oovecdoublecutoffsUsedInEachPreviousTimepoint = c(oodoublecutoffforinterimanalysis),
#'   oodoublealphaincrement = 0.025 - 0.06*0.025
#' )
#' oodoublecutoffforfinalanalysis #cutoff of 2.16 for the max-combo test statistic at
#' #the final analysis
#' #the max-combo test statistic at the final analysis exceeds the cutoff (since 2.28 > 2.16),
#' #so you can declare that the survival curves in the two arms are statistically significantly
#' #different at the 0.025 level
#' }
NULL

