#the below is not strictly needed since it is included in packagesetupandimportsandexamples.R, but I am putting it here to remind you of what namespaces are available in this package
#' @importFrom magrittr %>% %<>% %$% %T>%
#' @import rlang
NULL

#### THE BELOW FUNCTIONS ARE RELATED TO CREATING SIMULATED DATA.  THE BELOW FUNCTIONS ARE CURRENTLY NOT USED IN ANY USER-FACING PART OF THIS PACKAGE AND ARE NOT VERY MUCH RELATED TO THE MAX-COMBO TEST ITSELF ####

#id is a variable that takes a unique value for each subject
#treated is a variable indicating which of the two arms the subject is in.  It should take the integer value 1L for the new drug or therapy and 0L for the placebo or established therapy.
#Atime is the absolute time the subject enters the study
#Btime is the absolute time of an event or censoring, whichever comes first
#Bobserved is TRUE if the event was observed
#Ctime is the absolute time of an event or censoring, whichever comes first
#Cobserved is TRUE if censoring occurred (and so the event was not observed)
#
#note that the below function is used in oogetdataframeearlierlookatdeathsvRECODED, but that that latter function is not used in any other function
oogetdataframeearlierlookattimevRECODED=function(oodataframe,oodoubletime,oostringorsymbolid="id",oostringorsymboltreated="treated",oostringorsymbolAtime="Atime",oostringorsymbolBtime="Btime",oostringorsymbolBobserved="Bobserved",oostringorsymbolCtime="Ctime",oostringorsymbolCobserved="Cobserved")
{
  ooexpressionid=rlang::ensym(oostringorsymbolid)
  ooexpressiontreated=rlang::ensym(oostringorsymboltreated)
  ooexpressionAtime=rlang::ensym(oostringorsymbolAtime)
  ooexpressionBtime=rlang::ensym(oostringorsymbolBtime)
  ooexpressionBobserved=rlang::ensym(oostringorsymbolBobserved)
  ooexpressionCtime=rlang::ensym(oostringorsymbolCtime)
  ooexpressionCobserved=rlang::ensym(oostringorsymbolCobserved)
  
  oodataframe %>% dplyr::filter(!!ooexpressionAtime <= oodoubletime) %>%
    dplyr::mutate(!!ooexpressionBobserved := base::ifelse(!!ooexpressionBtime <= oodoubletime,!!ooexpressionBobserved,FALSE),
                  !!ooexpressionCobserved := base::ifelse(!!ooexpressionCtime <= oodoubletime,!!ooexpressionCobserved,FALSE),
                  !!ooexpressionBtime := base::pmin(!!ooexpressionBtime,oodoubletime),
                  !!ooexpressionCtime := base::pmin(!!ooexpressionCtime,oodoubletime) )
}


#note that the below function is used in oogetdataframeearlierlookatdeathsvRECODED, but that that latter function is not used in any other function
oogetdoubletimeatwhichoointnumdeathsfirstoccurredvRECODED=function(oodataframe,oointnumdeaths,oostringorsymbolid="id",oostringorsymboltreated="treated",oostringorsymbolAtime="Atime",oostringorsymbolBtime="Btime",oostringorsymbolBobserved="Bobserved",oostringorsymbolCtime="Ctime",oostringorsymbolCobserved="Cobserved")
{
  ooexpressionid=rlang::ensym(oostringorsymbolid)
  ooexpressiontreated=rlang::ensym(oostringorsymboltreated)
  ooexpressionAtime=rlang::ensym(oostringorsymbolAtime)
  ooexpressionBtime=rlang::ensym(oostringorsymbolBtime)
  ooexpressionBobserved=rlang::ensym(oostringorsymbolBobserved)
  ooexpressionCtime=rlang::ensym(oostringorsymbolCtime)
  ooexpressionCobserved=rlang::ensym(oostringorsymbolCobserved)
  
  if(oointnumdeaths==0L)
  {
    0.0
  }else
  {
    oodataframeBobserved=oodataframe %>% dplyr::filter(!!ooexpressionBobserved==TRUE)
    oointnumtotaldeaths=base::nrow(oodataframeBobserved)
    
    if(oointnumdeaths <= oointnumtotaldeaths)
    {
      (oodataframeBobserved %>% dplyr::arrange(!!ooexpressionBtime) )[[oointnumdeaths,rlang::as_string(ooexpressionBtime) ]]
    }else #oointnumdeaths > oointnumtotaldeaths
    {
      Inf
    }
    
  }
}

#note that the below function is not used in any other function
oogetdataframeearlierlookatdeathsvRECODED=function(oodataframe,oointnumdeaths,oostringorsymbolid="id",oostringorsymboltreated="treated",oostringorsymbolAtime="Atime",oostringorsymbolBtime="Btime",oostringorsymbolBobserved="Bobserved",oostringorsymbolCtime="Ctime",oostringorsymbolCobserved="Cobserved")
{
  ooexpressionid=rlang::ensym(oostringorsymbolid)
  ooexpressiontreated=rlang::ensym(oostringorsymboltreated)
  ooexpressionAtime=rlang::ensym(oostringorsymbolAtime)
  ooexpressionBtime=rlang::ensym(oostringorsymbolBtime)
  ooexpressionBobserved=rlang::ensym(oostringorsymbolBobserved)
  ooexpressionCtime=rlang::ensym(oostringorsymbolCtime)
  ooexpressionCobserved=rlang::ensym(oostringorsymbolCobserved)
  
  oointnumtotaldeaths=oodataframe[[rlang::as_string(ooexpressionBobserved)]] %>% base::sum()
  
  if(oointnumdeaths > oointnumtotaldeaths)
  {
    base::stop(base::paste("The argument oointnumdeaths must be less than or equal to the total number of deaths, which is",oointnumtotaldeaths,".") )
  }else if(oointnumdeaths < 0)
  {
    base::stop(base::paste("The argument oointnumdeaths must be greater than or equal to zero.") )
  }
  
  oodoubletimeatwhichoointnumdeathsfirstoccurred=oogetdoubletimeatwhichoointnumdeathsfirstoccurredvRECODED(oodataframe=oodataframe,oointnumdeaths=oointnumdeaths,oostringorsymbolid = !!ooexpressionid,oostringorsymboltreated = !!ooexpressiontreated,oostringorsymbolAtime = !!ooexpressionAtime,oostringorsymbolBtime = !!ooexpressionBtime,oostringorsymbolBobserved = !!ooexpressionBobserved,oostringorsymbolCtime = !!ooexpressionCtime,oostringorsymbolCobserved = !!ooexpressionCobserved)

  oogetdataframeearlierlookattimevRECODED(oodataframe=oodataframe,oodoubletime=oodoubletimeatwhichoointnumdeathsfirstoccurred,oostringorsymbolid = !!ooexpressionid,oostringorsymboltreated = !!ooexpressiontreated,oostringorsymbolAtime = !!ooexpressionAtime,oostringorsymbolBtime = !!ooexpressionBtime,oostringorsymbolBobserved = !!ooexpressionBobserved,oostringorsymbolCtime = !!ooexpressionCtime,oostringorsymbolCobserved = !!ooexpressionCobserved)
  
}

#note that the below function is not used in any other function
oogetmatrixdoublerandomcorrelationmatrixv2 = function(oointp)
{
  oomatrixdoubleS = base::diag(oointp)
  oointv = oointp + 2
  MCMCpack::rwish(v = oointv,S = oomatrixdoubleS)  %>% stats::cov2cor()
}


#### THE BELOW FUNCTIONS ARE EXPECTED TO BE CALLED AT SOME POINT DURING THE EXECUTION OF USER-FACING FUNCTIONS OF THIS PACKAGE RELATED TO THE MAX-COMBO TEST ####

#the below function is used in oogetresultsbasicv2vRECODED and in oogetresultsbasiccovariance3vRECODED
oogetdataframemsdataprocessedvRECODED=function(oodataframe,oostringorsymbolid="id",oostringorsymboltreated="treated",oostringorsymbolAtime="Atime",oostringorsymbolBtime="Btime",oostringorsymbolBobserved="Bobserved",oostringorsymbolCtime="Ctime",oostringorsymbolCobserved="Cobserved")
{
  ooexpressionid=rlang::ensym(oostringorsymbolid)
  ooexpressiontreated=rlang::ensym(oostringorsymboltreated)
  ooexpressionAtime=rlang::ensym(oostringorsymbolAtime)
  ooexpressionBtime=rlang::ensym(oostringorsymbolBtime)
  ooexpressionBobserved=rlang::ensym(oostringorsymbolBobserved)
  ooexpressionCtime=rlang::ensym(oostringorsymbolCtime)
  ooexpressionCobserved=rlang::ensym(oostringorsymbolCobserved)
  
  oomatrixinttmat=base::matrix(NA_integer_,nrow=3L,ncol=3L)
  oomatrixinttmat[[1L,2L]]=1L; oomatrixinttmat[[1L,3L]]=2L;
  base::dimnames(oomatrixinttmat)=base::list(from=c("A","B","C"),to=c("A","B","C") )
  
  oodataframesupport=oodataframe %>% dplyr::select(-c(!!ooexpressionAtime,!!ooexpressionBtime,!!ooexpressionBobserved,!!ooexpressionCtime,!!ooexpressionCobserved) ) #auxiliary covariates, since mstate::msprep keep= argument seems to be broken
  oodataframemsdata=mstate::msprep(time=c(rlang::as_string(ooexpressionAtime),rlang::as_string(ooexpressionBtime),rlang::as_string(ooexpressionCtime) ),
                    status=c(NA,rlang::as_string(ooexpressionBobserved),rlang::as_string(ooexpressionCobserved) ),
                    data=oodataframe,
                    trans=oomatrixinttmat,
                    id=rlang::as_string(ooexpressionid),
                    start=base::list(state=base::rep(1L,base::nrow(oodataframe) ),time=oodataframe[[rlang::as_string(ooexpressionAtime)]] )
                   )
  oodataframemsdata=base::merge(oodataframemsdata,oodataframesupport,by=rlang::as_string(ooexpressionid) )
  oodataframemsdata
}

#the below function is used in oogetdoublepvalue and oogetdoublecutoff
oogetvecboolextendedampersand=function(...)
{
  oolistvecbool=base::list(...)
  #base::print(oolistarguments)
  
  if(base::length(oolistvecbool) == 0L)
  {
    base::stop("oogetvecboolextendedampersand must have at least one argument.")
  }
  
  #so we know that there is at least one argument
  oolistvecbool %>% purrr::reduce(`&`)
  
}

#the below function is used in oogetdoublecutoff
oogetdoublebisectionMethod=function(oogetdoublefromdouble,oodoublelower,oodoubleupper,oointnmaxiter=20L,oodoublefscaletolerance=0.00001,oodoubledefaultvalueifnotenclosingazero=oodoubleupper)
{
  oogetboolcloseToZero=function(oodouble)
  {
    base::abs(oodouble) < oodoublefscaletolerance
  }
  
  oogetboolhaveOppositeSigns=function(oodoublefofa,oodoublefofb)
  {
    (oodoublefofa <= 0 & oodoublefofb >= 0) || (oodoublefofa >=0 & oodoublefofb <= 0)
  }
  
  if(!(oodoublelower < oodoubleupper))
  {
    base::stop("oodoublelower must be strictly less than oodoubleupper")
  }
  
  oodoublea=oodoublelower
  oodoubleb=oodoubleupper
  oodoublefofa = oogetdoublefromdouble(oodoublea)
  oodoublefofb = oogetdoublefromdouble(oodoubleb)
  
  if(!oogetboolhaveOppositeSigns(oodoublefofa,oodoublefofb) )
  {
    base::warning("oodoublelower and oodoubleupper must have function values with opposite signs. incorrectly returning a default value as the zero.")
    base::return(oodoubledefaultvalueifnotenclosingazero)
  }
  
  oointncurrentiter = 0L
  while(oointncurrentiter < oointnmaxiter)
  {
    oointncurrentiter = oointncurrentiter + 1L
    
    oodoublec = (oodoublea + oodoubleb)/2
    oodoublefofc = oogetdoublefromdouble(oodoublec)
    
    if(oogetboolcloseToZero(oodoublefofa) )
    {
      base::return(oodoublea)
    }else if(oogetboolcloseToZero(oodoublefofb) )
    {
      base::return(oodoubleb)
    }else if(oogetboolcloseToZero(oodoublefofc) )
    {
      base::return(oodoublec)
    }else if(oogetboolhaveOppositeSigns(oodoublefofa,oodoublefofc) ) #then next interval should be (a to c)
    {
      oodoubleb = oodoublec
      oodoublefofb = oodoublefofc
    }else #then next interval should be (c to b)
    {
      oodoublea = oodoublec
      oodoublefofa = oodoublefofc
    }
    
  }
  
  base::warning(base::paste0("maximum number of iterations reached. abs of f=",base::abs(oodoublefofc),",capture interval=[",oodoublea,",",oodoubleb,"]" ) )
  base::return(oodoublec)
  
}

#the below function is used in oogetdoublemaxcombotestpvalue
oogetdoublepvalue = function(oovecintnumberOfVariablesInEachSegment,oovecdoublecutoffsUsedInEachPreviousSegmentAndObservedValueInLastSegment,oomatrixdoublesigma,oointnrep=100000L)
{
  if(base::length(oovecintnumberOfVariablesInEachSegment) < 1L)
  {
    base::stop("oovecintnumberOfVariablesInEachSegment must have length greater than or equal to 1.")
  }else if(base::length(oovecdoublecutoffsUsedInEachPreviousSegmentAndObservedValueInLastSegment) < 1L)
  {
    base::stop("oovecdoublecutoffsUsedInEachPreviousSegmentAndObservedValueInLastSegment must have length greater than or equal to 1.")
  }else if(base::length(oovecintnumberOfVariablesInEachSegment) != base::length(oovecdoublecutoffsUsedInEachPreviousSegmentAndObservedValueInLastSegment) )
  {
    base::stop("oovecintnumberOfVariablesInEachSegment and oovecdoublecutoffsUsedInEachPreviousSegmentAndObservedValueInLastSegment must have the same length.") 
  }else if(!(base::nrow(oomatrixdoublesigma) == base::ncol(oomatrixdoublesigma) & base::ncol(oomatrixdoublesigma) >= 1) )
  {
    base::stop("oomatrixdoublesigma must be a square correlation matrix.")
  }else if(!(base::ncol(oomatrixdoublesigma) == base::sum(oovecintnumberOfVariablesInEachSegment) ) )
  {
    base::stop("The sum of the integers in oovecintnumberOfVariablesInEachSegment must equal the number of columns in oomatrixdoublesigma.")
  }
  
  #there are no previous segments.  it is just a 1-dimensional max combo test.  so you can also use the multivariate norm CDF
  if(base::length(oovecintnumberOfVariablesInEachSegment) == 1L)
  {
    oodoubleobservedValueInLastSegment = oovecdoublecutoffsUsedInEachPreviousSegmentAndObservedValueInLastSegment[[1L]]
    
    oointp=base::ncol(oomatrixdoublesigma)
    oodoublepvalue=1 - mvtnorm::pmvnorm(mean=base::rep(0,times=oointp),sigma=oomatrixdoublesigma,lower=base::rep(-Inf,times=oointp),upper=base::rep(oodoubleobservedValueInLastSegment,times=oointp) )
    base::return(oodoublepvalue)
  }
  
  #so in the code below we now know for sure that there is more than one segment, which means that there is at least one previous segment.  this also means that we will be using simulation.
  oodataframesimulationresults=mvtnorm::rmvnorm(n=oointnrep,sigma=oomatrixdoublesigma) %>% base::as.data.frame() %>% dplyr::as_tibble()
  
  oointnSegments = base::length(oovecintnumberOfVariablesInEachSegment)
  oodoubleobservedValueInLastSegment = oovecdoublecutoffsUsedInEachPreviousSegmentAndObservedValueInLastSegment[[oointnSegments]]
  
  #we need to get these names
  oovecstringcolumnnames = base::names(oodataframesimulationresults)
  oolistvecstringcolumnNamesBySegment = base::vector(mode="list",length=oointnSegments)
  
  oointcounter=0L
  for(oointsegment in 1L:oointnSegments)
  {
    oointnumberOfVariablesInCurrentSegment = oovecintnumberOfVariablesInEachSegment[[oointsegment]]
    oovecintindices = oointcounter + 1L:oointnumberOfVariablesInCurrentSegment
    oovecstringcolumnNamesForCurrentSegment = oovecstringcolumnnames[oovecintindices]
    oolistvecstringcolumnNamesBySegment[[oointsegment]] = oovecstringcolumnNamesForCurrentSegment
    
    oointcounter = oointcounter + oointnumberOfVariablesInCurrentSegment
  }
  
  #print(oolistvecstringcolumnNamesBySegment)
  
  oolistvecstringcolumnNamesBySegmentExceptLastSegment = oolistvecstringcolumnNamesBySegment[1L:(oointnSegments - 1L)]
  oovecdoublecutoffsUsedInEachPreviousSegment = oovecdoublecutoffsUsedInEachPreviousSegmentAndObservedValueInLastSegment[1L:(oointnSegments - 1L)]
  oolistquosures=purrr::map2(oolistvecstringcolumnNamesBySegmentExceptLastSegment,oovecdoublecutoffsUsedInEachPreviousSegment,
                     ~rlang::quo(base::pmax(!!!rlang::syms(.x ) ) < !!.y )
                    )
  ooquosureforFilterStatement=rlang::quo(oogetvecboolextendedampersand(!!!oolistquosures) )
  
  ooquosureForTransmuteStatement=rlang::quo(base::pmax(!!!rlang::syms(oolistvecstringcolumnNamesBySegment[[oointnSegments]] ) ) )
  
  oovecdoublemaxima=oodataframesimulationresults %>% 
    dplyr::filter(!!ooquosureforFilterStatement) %>%
    dplyr::transmute(oodouble=!!ooquosureForTransmuteStatement) %>%
    magrittr::extract2("oodouble")
  oodoublepvalue=(oovecdoublemaxima >= oodoubleobservedValueInLastSegment) %>% base::mean()
  
  #add an error attribute to oodoublepvalue so you have some idea of the accuracy of it given the simulation error
  oodoubleerrorsd=base::sqrt(oodoublepvalue*(1-oodoublepvalue)/base::length(oovecdoublemaxima) )
  base::attr(oodoublepvalue,which="error")=oodoubleerrorsd
  
  oodoublepvalue
  
}

#the below function is used in oogetdoublecutofffrompastdatavRECODED
oogetdoublecutoff = function(oovecintnumberOfVariablesInEachSegment,oovecdoublecutoffsUsedInEachPreviousSegment,oomatrixdoublesigma,oodoublealphaincrement,oointnrep=100000L,oodoublelower=-10,oodoubleupper=10,oointnmaxiter=25L,oodoublefscaletolerance=0.00001)
{
  if(base::length(oovecintnumberOfVariablesInEachSegment) < 1L)
  {
    base::stop("oovecintnumberOfVariablesInEachSegment must have length greater than or equal to 1.")
  }else if(base::length(oovecdoublecutoffsUsedInEachPreviousSegment) != base::length(oovecintnumberOfVariablesInEachSegment) - 1L)
  {
    base::stop("oovecdoublecutoffsUsedInEachPreviousSegment must have length exactly equal to 1 less than the length of oovecintnumberOfVariablesInEachSegment.")
  }else if(!(base::nrow(oomatrixdoublesigma) == base::ncol(oomatrixdoublesigma) & base::ncol(oomatrixdoublesigma) >= 1L) )
  {
    base::stop("oomatrixdoublesigma must be a square correlation matrix.")
  }else if(!(base::ncol(oomatrixdoublesigma) == base::sum(oovecintnumberOfVariablesInEachSegment) ) )
  {
    base::stop("The sum of the integers in oovecintnumberOfVariablesInEachSegment must equal the number of columns in oomatrixdoublesigma.")
  }
  
  #there are no previous segments.  it is just a 1-dimensional max combo test.  so you can also use the multivariate norm CDF
  if(base::length(oovecintnumberOfVariablesInEachSegment) == 1L)
  {
    oointp=base::ncol(oomatrixdoublesigma)
    
    oogetdoublepvaluebycutoff=function(oodoublecutoff)
    {
      oodoublepvalue=1 - mvtnorm::pmvnorm(mean=base::rep(0,times=oointp),sigma=oomatrixdoublesigma,lower=base::rep(-Inf,times=oointp),upper=base::rep(oodoublecutoff,times=oointp) )
      base::return(oodoublepvalue - oodoublealphaincrement)
    }
    
    oodoublereturnvaluecutoff = oogetdoublebisectionMethod(oogetdoublefromdouble = oogetdoublepvaluebycutoff,oodoublelower = oodoublelower,oodoubleupper = oodoubleupper,oointnmaxiter = oointnmaxiter,oodoublefscaletolerance = oodoublefscaletolerance)
    base::return(oodoublereturnvaluecutoff)
  
  }
  
  #so in the code below we now know for sure that there is more than one segment, which means that there is at least one previous segment.  this also means that we will be using simulation.
  oodataframesimulationresults=mvtnorm::rmvnorm(n=oointnrep,sigma=oomatrixdoublesigma) %>% base::as.data.frame() %>% dplyr::as_tibble()
  
  oointnSegments = base::length(oovecintnumberOfVariablesInEachSegment)
  #oodoubleobservedValueInLastSegment = oovecdoublecutoffsUsedInEachPreviousSegmentAndObservedValueInLastSegment[[oointnSegments]]
  
  #we need to get these names
  oovecstringcolumnnames = base::names(oodataframesimulationresults)
  oolistvecstringcolumnNamesBySegment = base::vector(mode="list",length=oointnSegments)
  
  oointcounter=0L
  for(oointsegment in 1L:oointnSegments)
  {
    oointnumberOfVariablesInCurrentSegment = oovecintnumberOfVariablesInEachSegment[[oointsegment]]
    oovecintindices = oointcounter + 1L:oointnumberOfVariablesInCurrentSegment
    oovecstringcolumnNamesForCurrentSegment = oovecstringcolumnnames[oovecintindices]
    oolistvecstringcolumnNamesBySegment[[oointsegment]] = oovecstringcolumnNamesForCurrentSegment
    
    oointcounter = oointcounter + oointnumberOfVariablesInCurrentSegment
  }
  
  #print(oolistvecstringcolumnNamesBySegment)
  
  oolistvecstringcolumnNamesBySegmentExceptLastSegment = oolistvecstringcolumnNamesBySegment[1L:(oointnSegments - 1L)]
  #oovecdoublecutoffsUsedInEachPreviousSegment = oovecdoublecutoffsUsedInEachPreviousSegmentAndObservedValueInLastSegment[1:(oointnSegments - 1)]
  oolistquosures=purrr::map2(oolistvecstringcolumnNamesBySegmentExceptLastSegment,oovecdoublecutoffsUsedInEachPreviousSegment,
                     ~rlang::quo(base::pmax(!!!rlang::syms(.x ) ) < !!.y )
                    )
  ooquosureforFilterStatement=rlang::quo(oogetvecboolextendedampersand(!!!oolistquosures) )
  
  ooquosureForTransmuteStatement=rlang::quo(base::pmax(!!!rlang::syms(oolistvecstringcolumnNamesBySegment[[oointnSegments]] ) ) )
  
  oovecdoublemaxima=oodataframesimulationresults %>% 
    dplyr::filter(!!ooquosureforFilterStatement) %>%
    dplyr::transmute(oodouble=!!ooquosureForTransmuteStatement) %>%
    magrittr::extract2("oodouble")
  
  oogetdoublepvaluebycutoff=function(oodoublecutoff)
  {
    oodoublepvalue=(oovecdoublemaxima >= oodoublecutoff) %>% base::mean()
    base::return(oodoublepvalue - oodoublealphaincrement)
  }
  
  oodoublereturnvaluecutoff = oogetdoublebisectionMethod(oogetdoublefromdouble = oogetdoublepvaluebycutoff,oodoublelower = oodoublelower,oodoubleupper = oodoubleupper,oointnmaxiter = oointnmaxiter,oodoublefscaletolerance = oodoublefscaletolerance)
  base::return(oodoublereturnvaluecutoff)
  
  #oodoublepvalue=(oovecdoublemaxima >= oodoubleobservedValueInLastSegment) %>% base::mean()
  
  #add an error attribute to oodoublepvalue so you have some idea of the accuracy of it given the simulation error
  #oodoubleerrorsd=base::sqrt(oodoublepvalue*(1-oodoublepvalue)/base::length(oovecdoublemaxima) )
  #base::attr(oodoublepvalue,which="error")=oodoubleerrorsd
  
  #oodoublepvalue
  
}

#note that the below function is currently not used in any other function; however, this function is as close to a user-facing function that this package currently has; hence, it has been placed here
oogetresultsbasicv2vRECODED=function(oodataframe,oofunctionweightasafunctionofstminus=function(oodoublestminus){ base::return(1.0) },oostringorsymbolid="id",oostringorsymboltreated="treated",oostringorsymbolAtime="Atime",oostringorsymbolBtime="Btime",oostringorsymbolBobserved="Bobserved",oostringorsymbolCtime="Ctime",oostringorsymbolCobserved="Cobserved")
{
  ooexpressionid=rlang::ensym(oostringorsymbolid)
  ooexpressiontreated=rlang::ensym(oostringorsymboltreated)
  ooexpressionAtime=rlang::ensym(oostringorsymbolAtime)
  ooexpressionBtime=rlang::ensym(oostringorsymbolBtime)
  ooexpressionBobserved=rlang::ensym(oostringorsymbolBobserved)
  ooexpressionCtime=rlang::ensym(oostringorsymbolCtime)
  ooexpressionCobserved=rlang::ensym(oostringorsymbolCobserved)
  
  oodataframetrans1=oodataframe %>% oogetdataframemsdataprocessedvRECODED(oostringorsymbolid = !!ooexpressionid,oostringorsymboltreated = !!ooexpressiontreated,oostringorsymbolAtime = !!ooexpressionAtime,oostringorsymbolBtime = !!ooexpressionBtime,oostringorsymbolBobserved = !!ooexpressionBobserved,oostringorsymbolCtime = !!ooexpressionCtime,oostringorsymbolCobserved = !!ooexpressionCobserved) %>% dplyr::filter(.data$trans==1L) #see explanation of why I am using .data$trans==1L instead of trans==1L here in the following guide about how to use dplyr from within a package while avoiding R CMD CHECK notes: https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html 
  oovecinttreated=oodataframetrans1[[rlang::as_string(ooexpressiontreated) ]]
  oovecdoublemintc=oodataframetrans1$time
  oovecboolobservedt=oodataframetrans1$status
  
  ooSurv=survival::Surv(time=oovecdoublemintc,event=oovecboolobservedt,type="right")
  oosurvfit=survival::survfit(formula=ooSurv~1)
  #base::unclass(oosurvfit)
  
  #this particular variable is used in many of the weight specifications, so we calculate it here
  oovecdoublesurvivaltminus=c(1.0,oosurvfit$surv)[1L:(base::length(oosurvfit$surv) ) ]
  
  #oovecinty1plusy2=oosurvfit$n.risk
  #oosurvfit$time %>% purrr::map_int(~base::sum(oovecdoublemintc >= .x)  ) #should equal the above
  oovecinty1=oosurvfit$time %>% purrr::map_int(~base::sum(oovecdoublemintc[oovecinttreated==0L] >= .x)  )
  oovecinty2=oosurvfit$time %>% purrr::map_int(~base::sum(oovecdoublemintc[oovecinttreated==1L] >= .x)  )
  
  #oovecintdn1plusdn2=oosurvfit$n.event
  #oosurvfit$time %>% purrr::map_int(~base::sum(oovecdoublemintc==.x & oovecboolobservedt==TRUE) ) #should equal the above
  oovecintdn1=oosurvfit$time %>% purrr::map_int(~base::sum(oovecdoublemintc[oovecinttreated==0L]==.x & oovecboolobservedt[oovecinttreated==0L]==TRUE) )
  oovecintdn2=oosurvfit$time %>% purrr::map_int(~base::sum(oovecdoublemintc[oovecinttreated==1L]==.x & oovecboolobservedt[oovecinttreated==1L]==TRUE) )
  
  oodataframebytimepoint=dplyr::tibble(oovecdoubletime=oosurvfit$time,oovecdoublesurvivaltminus,oovecinty1,oovecinty2,oovecintdn1,oovecintdn2)
  oodataframebytimepointreduced=oodataframebytimepoint %>% dplyr::filter(oovecinty1 > 0L & oovecinty2 > 0L & (oovecintdn1 > 0L | oovecintdn2 > 0L) )

  oovecdoubleweights=oodataframebytimepointreduced$oovecdoublesurvivaltminus %>% purrr::map_dbl(oofunctionweightasafunctionofstminus)
  
  #see "Lee 2007 On the versatility of the combination of weighted log-rank statistics.pdf"
  oodoublestatisticnotnormalized=base::sum( oovecdoubleweights * (oodataframebytimepointreduced$oovecinty1*oodataframebytimepointreduced$oovecinty2)/(oodataframebytimepointreduced$oovecinty1 + oodataframebytimepointreduced$oovecinty2)*(oodataframebytimepointreduced$oovecintdn1/oodataframebytimepointreduced$oovecinty1 - oodataframebytimepointreduced$oovecintdn2/oodataframebytimepointreduced$oovecinty2) )

  #see "Lee 2007 On the versatility of the combination of weighted log-rank statistics.pdf"
  oodoublevarianceestimateofunnormalizedstatistic=base::sum( oovecdoubleweights * oovecdoubleweights * (oodataframebytimepointreduced$oovecinty1*oodataframebytimepointreduced$oovecinty2)/(oodataframebytimepointreduced$oovecinty1 + oodataframebytimepointreduced$oovecinty2)*(1 - (oodataframebytimepointreduced$oovecintdn1 + oodataframebytimepointreduced$oovecintdn2 - 1)/(oodataframebytimepointreduced$oovecinty1 + oodataframebytimepointreduced$oovecinty2 - 1) ) * (oodataframebytimepointreduced$oovecintdn1 + oodataframebytimepointreduced$oovecintdn2)/(oodataframebytimepointreduced$oovecinty1 + oodataframebytimepointreduced$oovecinty2) )
  
  oolistreturnvalue=base::list(oodoublestatisticnotnormalized=oodoublestatisticnotnormalized,oodoublevarianceestimateofunnormalizedstatistic=oodoublevarianceestimateofunnormalizedstatistic,oodoublestatisticnormalized=oodoublestatisticnotnormalized/base::sqrt(oodoublevarianceestimateofunnormalizedstatistic) )
  
  oolistreturnvalue
  
}

#the below function is used in oogetdoublecutofffrompastdatavRECODED
#
#i think what we can do is make a function for S2 that takes the timepoints from the other dataframe as a function and linearly interpolates between the entries in the survivaltminusAtt2.  or you could just search for an index and if you can't find it, THEN linearly interpolate.  OR what you could do is just search for an index and if you can't find it, pick the index that is closest to what you are interested in (tom note: this appears to be what is done below).  in any case, make this function and apply it to the original time values from the other dataframe instead of doing a merge operation.
oogetresultsbasiccovariance3vRECODED=function(oodataframet1,oodataframet2,oofunctionweightasafunctionofstminus1=function(stminus){ base::return(1.0) },oofunctionweightasafunctionofstminus2=function(stminus){ base::return(1.0) }, oostringorsymbolid="id",oostringorsymboltreated="treated",oostringorsymbolAtime="Atime",oostringorsymbolBtime="Btime",oostringorsymbolBobserved="Bobserved",oostringorsymbolCtime="Ctime",oostringorsymbolCobserved="Cobserved")
{
  ooexpressionid=rlang::ensym(oostringorsymbolid)
  ooexpressiontreated=rlang::ensym(oostringorsymboltreated)
  ooexpressionAtime=rlang::ensym(oostringorsymbolAtime)
  ooexpressionBtime=rlang::ensym(oostringorsymbolBtime)
  ooexpressionBobserved=rlang::ensym(oostringorsymbolBobserved)
  ooexpressionCtime=rlang::ensym(oostringorsymbolCtime)
  ooexpressionCobserved=rlang::ensym(oostringorsymbolCobserved)
  
  
  oodataframetrans1=oodataframet1 %>% oogetdataframemsdataprocessedvRECODED(oostringorsymbolid = !!ooexpressionid,oostringorsymboltreated = !!ooexpressiontreated,oostringorsymbolAtime = !!ooexpressionAtime,oostringorsymbolBtime = !!ooexpressionBtime,oostringorsymbolBobserved = !!ooexpressionBobserved,oostringorsymbolCtime = !!ooexpressionCtime,oostringorsymbolCobserved = !!ooexpressionCobserved) %>% dplyr::filter(.data$trans==1L) #see explanation of why I am using .data$trans==1L instead of trans==1L here in the following guide about how to use dplyr from within a package while avoiding R CMD CHECK notes: https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html 
  oovecinttreated=oodataframetrans1[[rlang::as_string(ooexpressiontreated) ]]
  oovecdoublemintc=oodataframetrans1$time
  oovecboolobservedt=oodataframetrans1$status
  
  ooSurv=survival::Surv(time=oovecdoublemintc,event=oovecboolobservedt,type="right")
  oosurvfit=survival::survfit(formula=ooSurv~1)
  #base::unclass(oosurvfit)
  
  #this particular variable is used in many of the weight specifications, so we calculate it here
  oovecdoublesurvivaltminus=c(1.0,oosurvfit$surv)[1:(base::length(oosurvfit$surv) ) ]
  
  #oovecinty1plusy2=oosurvfit$n.risk
  #oosurvfit$time %>% purrr::map_int(~base::sum(oovecdoublemintc >= .x)  ) #should equal the above
  oovecinty1=oosurvfit$time %>% purrr::map_int(~base::sum(oovecdoublemintc[oovecinttreated==0] >= .x)  )
  oovecinty2=oosurvfit$time %>% purrr::map_int(~base::sum(oovecdoublemintc[oovecinttreated==1] >= .x)  )
  
  #oovecintdn1plusdn2=oosurvfit$n.event
  #oosurvfit$time %>% purrr::map_int(~base::sum(oovecdoublemintc==.x & oovecboolobservedt==TRUE) ) #should equal the above
  oovecintdn1=oosurvfit$time %>% purrr::map_int(~base::sum(oovecdoublemintc[oovecinttreated==0]==.x & oovecboolobservedt[oovecinttreated==0]==TRUE) )
  oovecintdn2=oosurvfit$time %>% purrr::map_int(~base::sum(oovecdoublemintc[oovecinttreated==1]==.x & oovecboolobservedt[oovecinttreated==1]==TRUE) )
  
  oodataframebytimepoint=dplyr::tibble(oovecdoubletime=oosurvfit$time,oovecdoublesurvivaltminus,oovecinty1,oovecinty2,oovecintdn1,oovecintdn2)
  oodataframebytimepointreduced=oodataframebytimepoint %>% dplyr::filter(oovecinty1 > 0 & oovecinty2 > 0 & (oovecintdn1 > 0 | oovecintdn2 > 0) )
  
  oovecdoubleweights=oodataframebytimepointreduced$oovecdoublesurvivaltminus %>% purrr::map_dbl(oofunctionweightasafunctionofstminus1)
  

  #### This below part is new.  Now we also need the weight function from the second timepoint. ####
  
  oodataframetrans1t2=oodataframet2 %>% oogetdataframemsdataprocessedvRECODED(oostringorsymbolid = !!ooexpressionid,oostringorsymboltreated = !!ooexpressiontreated,oostringorsymbolAtime = !!ooexpressionAtime,oostringorsymbolBtime = !!ooexpressionBtime,oostringorsymbolBobserved = !!ooexpressionBobserved,oostringorsymbolCtime = !!ooexpressionCtime,oostringorsymbolCobserved = !!ooexpressionCobserved) %>% dplyr::filter(.data$trans==1L) #see explanation of why I am using .data$trans==1L instead of trans==1L here in the following guide about how to use dplyr from within a package while avoiding R CMD CHECK notes: https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html 
  oosurvfitt2=survival::survfit(formula=survival::Surv(time=oodataframetrans1t2$time,event=oodataframetrans1t2$status,type="right")~1)
  
  oovecdoublesurvivaltminust2=c(1.0,oosurvfitt2$surv)[1:(base::length(oosurvfitt2$surv) ) ]
  
  oodataframeworkingt2=dplyr::tibble(time=oosurvfitt2$time,oovecdoublesurvivalminusATt2=oovecdoublesurvivaltminust2)
  
  oogetdoublefromdoublesurvivalminusAtt2=function(oodoubletime)
  {
    ooointcolumnindex=base::match(oodoubletime,oodataframeworkingt2$time)
    if(!base::is.na(ooointcolumnindex) )
    {
      base::return(oodataframeworkingt2$oovecdoublesurvivalminusATt2[[ooointcolumnindex]] )
    }else
    {
      oointcolumnindex=base::which.min( base::abs(oodataframeworkingt2$time - oodoubletime) )
      base::return(oodataframeworkingt2$oovecdoublesurvivalminusATt2[[oointcolumnindex]] )
    }
  }
  
  oovecdoubleweights2=oodataframebytimepointreduced$oovecdoubletime %>% purrr::map_dbl(oogetdoublefromdoublesurvivalminusAtt2) %>% purrr::map_dbl(oofunctionweightasafunctionofstminus2)
  
  #oodataframebytimepointreducedwitht2survival=base::merge(oodataframebytimepointreduced,oodataframeworkingt2,by.x="oovecdoubletime",by.y="time")
  
  #oovecdoubleweights2=oodataframebytimepointreducedwitht2survival$oovecdoublesurvivalminusATt2 %>% purrr::map_dbl(oofunctionweightasafunctionofstminus2)
  
  #if(base::length(oovecdoubleweights) != base::length(oovecdoubleweights2) )
  #{
  #  base::warning(base::paste0("oovecdoubleweights length ",base::length(oovecdoubleweights)," and oovecdoubleweights2 length ",base::length(oovecdoubleweights2)," and oovecdoubleweights: ",base::toString(base::round(oovecdoubleweights,digits=2))," and oovecdoubleweights2: ",base::toString(base::round(oovecdoubleweights2,digits = 2) ) ) )
  #}
  
  #### The above part is new. ####

  #see "Lee 2007 On the versatility of the combination of weighted log-rank statistics.pdf"
  oodoublecovarianceestimate=base::sum( oovecdoubleweights * oovecdoubleweights2 * (oodataframebytimepointreduced$oovecinty1*oodataframebytimepointreduced$oovecinty2)/(oodataframebytimepointreduced$oovecinty1 + oodataframebytimepointreduced$oovecinty2)*(1 - (oodataframebytimepointreduced$oovecintdn1 + oodataframebytimepointreduced$oovecintdn2 - 1)/(oodataframebytimepointreduced$oovecinty1 + oodataframebytimepointreduced$oovecinty2 - 1) ) * (oodataframebytimepointreduced$oovecintdn1 + oodataframebytimepointreduced$oovecintdn2)/(oodataframebytimepointreduced$oovecinty1 + oodataframebytimepointreduced$oovecinty2) )
  
  oodoublecovarianceestimate
  
}

#note that the below function is currently not used in any other function; however, this function is as close to a user-facing function that this package currently has; hence, it has been placed here
oogetdoublecutofffrompastdatavRECODED=function(oolistdataframesbytimepoint,oolistlistweightingfunctionsbytimepoint,oovecdoublecutoffsUsedInEachPreviousTimepoint,oodoublealphaincrement,oointnrep=100000L,oodoublelower=-10,oodoubleupper=10,oointnmaxiter=25L,oodoublefscaletolerance=0.00001,oostringorsymbolid="id",oostringorsymboltreated="treated",oostringorsymbolAtime="Atime",oostringorsymbolBtime="Btime",oostringorsymbolBobserved="Bobserved",oostringorsymbolCtime="Ctime",oostringorsymbolCobserved="Cobserved")
{
  ooexpressionid=rlang::ensym(oostringorsymbolid)
  ooexpressiontreated=rlang::ensym(oostringorsymboltreated)
  ooexpressionAtime=rlang::ensym(oostringorsymbolAtime)
  ooexpressionBtime=rlang::ensym(oostringorsymbolBtime)
  ooexpressionBobserved=rlang::ensym(oostringorsymbolBobserved)
  ooexpressionCtime=rlang::ensym(oostringorsymbolCtime)
  ooexpressionCobserved=rlang::ensym(oostringorsymbolCobserved)
  
  if(base::length(oolistdataframesbytimepoint) < 1L)
  {
    base::stop("oolistdataframesbytimepoint must have length greater than or equal to 1.")
  }else if(base::length(oolistdataframesbytimepoint) != base::length(oolistlistweightingfunctionsbytimepoint) )
  {
    base::stop("oolistdataframesbytimepoint and oolistlistweightingfunctionsbytimepoint must have the same length.")
  }else if(base::length(oovecdoublecutoffsUsedInEachPreviousTimepoint) != base::length(oolistdataframesbytimepoint) - 1L)
  {
    base::stop("oovecdoublecutoffsUsedInEachPreviousTimepoint must have length exactly equal to 1 less than the length of oolistdataframesbytimepoint.")
  }
  
  oovecintnumberOfVariablesBySegment = oolistlistweightingfunctionsbytimepoint %>% purrr::map_int(base::length)
  oointp = oovecintnumberOfVariablesBySegment %>% base::sum()
  oomatrixdoublecovariancematrix = base::matrix(NA,nrow = oointp,ncol = oointp)
  oointnSegments=base::length(oolistdataframesbytimepoint)
  
  oogetintsegmentbycovariancematrixcolumnindex=function(oointcolumnindex)
  {
    oointcounter=0L
    for(oointsegment in 1:oointnSegments)
    {
      for(oointvariableWithinSegment in 1:oovecintnumberOfVariablesBySegment[[oointsegment]] )
      {
        oointcounter = oointcounter + 1L
        if(oointcounter == oointcolumnindex)
        {
          base::return(c(oointsegment,oointvariableWithinSegment) )
        }
      }
    }
  }
  
  for(oointj in 1:oointp)
  {
    for(oointk in 1:oointj) #note that dataframe for k must be earlier or the same as the dataframe for j.
    {
      oovecintresultsforj = oogetintsegmentbycovariancematrixcolumnindex(oointj)
      oointsegmentforj = oovecintresultsforj[[1]]
      oointvariableWithinSegmentforj = oovecintresultsforj[[2]]
      
      oovecintresultsfork = oogetintsegmentbycovariancematrixcolumnindex(oointk)
      oointsegmentfork = oovecintresultsfork[[1]]
      oointvariableWithinSegmentfork = oovecintresultsfork[[2]]
      
      oomatrixdoublecovariancematrix[[oointj,oointk]]=oogetresultsbasiccovariance3vRECODED(oodataframet1 = oolistdataframesbytimepoint[[oointsegmentfork]],oodataframet2 = oolistdataframesbytimepoint[[oointsegmentforj]],oofunctionweightasafunctionofstminus1 = oolistlistweightingfunctionsbytimepoint[[oointsegmentfork]][[oointvariableWithinSegmentfork]],oofunctionweightasafunctionofstminus2 = oolistlistweightingfunctionsbytimepoint[[oointsegmentforj]][[oointvariableWithinSegmentforj]],oostringorsymbolid = !!ooexpressionid,oostringorsymboltreated = !!ooexpressiontreated,oostringorsymbolAtime = !!ooexpressionAtime,oostringorsymbolBtime = !!ooexpressionBtime,oostringorsymbolBobserved = !!ooexpressionBobserved,oostringorsymbolCtime = !!ooexpressionCtime,oostringorsymbolCobserved = !!ooexpressionCobserved)
      oomatrixdoublecovariancematrix[[oointk,oointj]]=oomatrixdoublecovariancematrix[[oointj,oointk]]
    }
  }
  
  oomatrixdoublecorrelationmatrix = oomatrixdoublecovariancematrix %>% stats::cov2cor()
  
  oogetdoublecutoff(oovecintnumberOfVariablesInEachSegment = oovecintnumberOfVariablesBySegment,oovecdoublecutoffsUsedInEachPreviousSegment = oovecdoublecutoffsUsedInEachPreviousTimepoint,oomatrixdoublesigma = oomatrixdoublecorrelationmatrix,oodoublealphaincrement = oodoublealphaincrement,oointnrep = oointnrep,oodoublelower = oodoublelower,oodoubleupper = oodoubleupper,oointnmaxiter = oointnmaxiter,oodoublefscaletolerance = oodoublefscaletolerance)
  
}



#### THE BELOW FUNCTIONS ARE USER-FACING FUNCTIONS OF THIS PACKAGE RELATED TO THE MAX-COMBO TEST ####

#' Compute the max-combo test statistic
#' 
#' @description 
#' This function computes the max-combo test statistic for comparing times to an event from two different arms.  The max-combo test statistic is the maximum of multiple standardized weighted log-rank test statistics.  The user of the function is expected to provide the function with a data frame containing the time-to-event data for the two arms; additionally, this data frame is expected to be in a particular format (see more details below).  Additionally, the user of the function is expected to provide a list of one or more weighting functions to use for the respective one or more weighted log-rank tests; in particular this function only allows the user to specify a weighting function that takes as input the Kaplan-Meier estimate for the survival curve obtained by pooling both of the two arms (a double that is between 0 and 1, inclusive) and provides as output a weight (a double that is typically greater than or equal to zero); note that the Fleming-Harrington class of weighting functions falls within what can be specified in this way.  If no list of weighting functions is provided, a single weighting function that is identically 1 is used, which means that the test statistic that is returned is exactly equal to the usual standardized log-rank test statistic.
#' 
#' The supplied data frame is expected to have one row for each subject.  The data frame should have the following columns:
#' * `id` - a variable that takes a unique value for each subject
#' * `treated` - should be 0 for subjects in the control arm and 1 for subjects in the experimental arm.
#' * `Atime` - the absolute time the subject enters the study
#' * `Btime` - the absolute time of an event or censoring, whichever comes first
#' * `Bobserved` - should be TRUE if the event was observed
#' * `Ctime` - the absolute time of an event or censoring, whichever comes first
#' * `Cobserved` - should be TRUE if censoring occurred (and so the event was not observed)
#' 
#' The columns in the data frame do not have to be named exactly as above; however, if different names are used, those names must be specified as arguments to the function.  Please see below for more detail regarding how to specify those names and for more detail regarding the expectations of the function regarding the columns in the data frame.
#' 
#' @param oodataframe A data frame containing time-to-event data from two different arms.
#' @param oolistfunctionweightasafunctionofstminus A list of one or more weighting functions.  Each weighting function should take as input the pooled Kaplan-Meier estimate of survival across the two arms (i.e., a double between 0 and 1, inclusive) and output a single weight (i.e., a double, typically greater than or equal to zero).  Defaults to the following list of a single weighting function that is identically 1, which results in the usual standardized log-rank test statistic: `base::list(function(oodoublestminus){ base::return(1) } )`
#' @param oostringorsymbolid The name of the column in the supplied data frame for the id variable.  Defaults to "id".  The id variable should take a unique value for each subject.  The column with this name in the data frame can be an integer vector with a different integer for each subject; the function will most likely also work if the column is a character vector with a different value for each subject.
#' @param oostringorsymboltreated The name of the column in the supplied data frame for a treatment indicator variable.  Defaults to "treated".  The treatment indicator variable is a variable indicating which of the two arms the subject is in.  The column with this name in the data frame should be an integer vector that takes only the value 0L (e.g., for placebo) or the value 1L (e.g., for a new drug or therapy).  The test statistic that is returned by this function will generally be for a test that the arm represented by subjects with treatment indicator 1L is superior to the arm represented by subjects with treatment indicator 0L (i.e., a one-sided test of superiority of arm 1 over arm 0).
#' @param oostringorsymbolAtime The name of the column in the supplied data frame for the Atime variable.  Defaults to "Atime".  Atime is the absolute time the subject enters the study.  The column with this name in the data frame can be vector of doubles; the function will most likely also work if the column is a vector of integers.
#' @param oostringorsymbolBtime The name of the column in the supplied data frame for the Btime variable.  Defaults to "Btime".  Btime is the absolute time of an event or censoring, whichever comes first.  The column with this name in the data frame can be vector of doubles; the function will most likely also work if the column is a vector of integers.
#' @param oostringorsymbolBobserved The name of the column in the supplied data frame for the Bobserved variable.  Defaults to "Bobserved".  Bobserved should be TRUE if the event was observed.  The column with this name in the data frame can be a logical vector; the function will most likely also work if the column is a vector of integers (i.e., with 1L in place of TRUE and 0L in place of FALSE).
#' @param oostringorsymbolCtime The name of the column in the supplied data frame for the Ctime variable.  Defaults to "Ctime".  Ctime is the absolute time of an event or censoring, whichever comes first.  The column with this name in the data frame can be vector of doubles; the function will most likely also work if the column is a vector of integers.
#' @param oostringorsymbolCobserved The name of the column in the supplied data frame for the Cobserved variable.  Defaults to "Cobserved".  Cobserved should be TRUE if censoring occurred (and so the event was not observed).  The column with this name in the data frame can be a logical vector; the function will most likely also work if the column is a vector of integers (i.e., with 1L in place of TRUE and 0L in place of FALSE).
#' 
#' @return A double
#' 
#' @export
oogetdoublemaxcomboteststatistic=function(oodataframe,oolistfunctionweightasafunctionofstminus=base::list(function(oodoublestminus){ base::return(1.0) } ),oostringorsymbolid="id",oostringorsymboltreated="treated",oostringorsymbolAtime="Atime",oostringorsymbolBtime="Btime",oostringorsymbolBobserved="Bobserved",oostringorsymbolCtime="Ctime",oostringorsymbolCobserved="Cobserved")
{
  ooexpressionid=rlang::ensym(oostringorsymbolid)
  ooexpressiontreated=rlang::ensym(oostringorsymboltreated)
  ooexpressionAtime=rlang::ensym(oostringorsymbolAtime)
  ooexpressionBtime=rlang::ensym(oostringorsymbolBtime)
  ooexpressionBobserved=rlang::ensym(oostringorsymbolBobserved)
  ooexpressionCtime=rlang::ensym(oostringorsymbolCtime)
  ooexpressionCobserved=rlang::ensym(oostringorsymbolCobserved)
  
  if(base::length(oolistfunctionweightasafunctionofstminus) < 1L)
  {
    base::stop("oolistfunctionweightasafunctionofstminus must have length greater than or equal to 1.")
  }
  
  oovecdoublestandardizedweightedlogrankstatistics=oolistfunctionweightasafunctionofstminus %>% purrr::map_dbl(~oogetresultsbasicv2vRECODED(oodataframe=oodataframe,oofunctionweightasafunctionofstminus=.x,oostringorsymbolid = !!ooexpressionid,oostringorsymboltreated = !!ooexpressiontreated,oostringorsymbolAtime = !!ooexpressionAtime,oostringorsymbolBtime = !!ooexpressionBtime,oostringorsymbolBobserved = !!ooexpressionBobserved,oostringorsymbolCtime = !!ooexpressionCtime,oostringorsymbolCobserved = !!ooexpressionCobserved)[["oodoublestatisticnormalized"]] )
  
  base::return(oovecdoublestandardizedweightedlogrankstatistics %>% base::max() )
}


#' Compute the p-value for the max-combo test statistic at a single timepoint
#' 
#' @description 
#' This function computes the p-value for the max-combo test statistic for comparing times to an event from two different arms at a single timepoint; please note that this is *not* a p-value for a group sequential test procedure involving two or more timepoints, but rather a p-value for a scenario in which there is only one planned analysis timepoint.  The max-combo test statistic is the maximum of multiple standardized weighted log-rank test statistics.  The user of the function is expected to provide the function with a data frame containing the time-to-event data for the two arms; additionally, this data frame is expected to be in a particular format (see more details below).  Additionally, the user of the function is expected to provide a list of one or more weighting functions to use for the respective one or more weighted log-rank tests; in particular this function only allows the user to specify a weighting function that takes as input the Kaplan-Meier estimate for the survival curve obtained by pooling both of the two arms (a double that is between 0 and 1, inclusive) and provides as output a weight (a double that is typically greater than or equal to zero); note that the Fleming-Harrington class of weighting functions falls within what can be specified in this way.  If no list of weighting functions is provided, a single weighting function that is identically 1 is used, which means that the test statistic that is used is exactly equal to the usual standardized log-rank test statistic.
#' 
#' The supplied data frame is expected to have one row for each subject.  The data frame should have the following columns:
#' * `id` - a variable that takes a unique value for each subject
#' * `treated` - should be 0 for subjects in the control arm and 1 for subjects in the experimental arm.
#' * `Atime` - the absolute time the subject enters the study
#' * `Btime` - the absolute time of an event or censoring, whichever comes first
#' * `Bobserved` - should be TRUE if the event was observed
#' * `Ctime` - the absolute time of an event or censoring, whichever comes first
#' * `Cobserved` - should be TRUE if censoring occurred (and so the event was not observed)
#' 
#' The columns in the data frame do not have to be named exactly as above; however, if different names are used, those names must be specified as arguments to the function.  Please see below for more detail regarding how to specify those names and for more detail regarding the expectations of the function regarding the columns in the data frame.
#' 
#' @param oodataframe A data frame containing time-to-event data from two different arms.
#' @param oolistfunctionweightasafunctionofstminus A list of one or more weighting functions.  Each weighting function should take as input the pooled Kaplan-Meier estimate of survival across the two arms (i.e., a double between 0 and 1, inclusive) and output a single weight (i.e., a double, typically greater than or equal to zero).  Defaults to the following list of a single weighting function that is identically 1, which results in the usual standardized log-rank test statistic: `base::list(function(oodoublestminus){ base::return(1) } )`
#' @param oostringorsymbolid The name of the column in the supplied data frame for the id variable.  Defaults to "id".  The id variable should take a unique value for each subject.  The column with this name in the data frame can be an integer vector with a different integer for each subject; the function will most likely also work if the column is a character vector with a different value for each subject.
#' @param oostringorsymboltreated The name of the column in the supplied data frame for a treatment indicator variable.  Defaults to "treated".  The treatment indicator variable is a variable indicating which of the two arms the subject is in.  The column with this name in the data frame should be an integer vector that takes only the value 0L (e.g., for placebo) or the value 1L (e.g., for a new drug or therapy).  The test statistic that is used by this function will generally be for a test that the arm represented by subjects with treatment indicator 1L is superior to the arm represented by subjects with treatment indicator 0L (i.e., a one-sided test of superiority of arm 1 over arm 0).
#' @param oostringorsymbolAtime The name of the column in the supplied data frame for the Atime variable.  Defaults to "Atime".  Atime is the absolute time the subject enters the study.  The column with this name in the data frame can be vector of doubles; the function will most likely also work if the column is a vector of integers.
#' @param oostringorsymbolBtime The name of the column in the supplied data frame for the Btime variable.  Defaults to "Btime".  Btime is the absolute time of an event or censoring, whichever comes first.  The column with this name in the data frame can be vector of doubles; the function will most likely also work if the column is a vector of integers.
#' @param oostringorsymbolBobserved The name of the column in the supplied data frame for the Bobserved variable.  Defaults to "Bobserved".  Bobserved should be TRUE if the event was observed.  The column with this name in the data frame can be a logical vector; the function will most likely also work if the column is a vector of integers (i.e., with 1L in place of TRUE and 0L in place of FALSE).
#' @param oostringorsymbolCtime The name of the column in the supplied data frame for the Ctime variable.  Defaults to "Ctime".  Ctime is the absolute time of an event or censoring, whichever comes first.  The column with this name in the data frame can be vector of doubles; the function will most likely also work if the column is a vector of integers.
#' @param oostringorsymbolCobserved The name of the column in the supplied data frame for the Cobserved variable.  Defaults to "Cobserved".  Cobserved should be TRUE if censoring occurred (and so the event was not observed).  The column with this name in the data frame can be a logical vector; the function will most likely also work if the column is a vector of integers (i.e., with 1L in place of TRUE and 0L in place of FALSE).
#' @param ooodoublemaxcomboteststatistic A value for a hypothetical max-combo test statistic that could have been observed (but was not) can be provided to this argument, in which case the p-value for that hypothetical statistic that could have been observed (but was not) is what is provided.  If `NULL` is provided, a p-value for the max-combo test statistic that was actually observed is provided.  Defaults to `NULL`.
#' 
#' @return A double
#' 
#' @export
oogetdoublemaxcombotestpvalue=function(oodataframe,oolistfunctionweightasafunctionofstminus=base::list(function(oodoublestminus){ base::return(1.0) } ),oostringorsymbolid="id",oostringorsymboltreated="treated",oostringorsymbolAtime="Atime",oostringorsymbolBtime="Btime",oostringorsymbolBobserved="Bobserved",oostringorsymbolCtime="Ctime",oostringorsymbolCobserved="Cobserved",ooodoublemaxcomboteststatistic=NULL)
{
  ooexpressionid=rlang::ensym(oostringorsymbolid)
  ooexpressiontreated=rlang::ensym(oostringorsymboltreated)
  ooexpressionAtime=rlang::ensym(oostringorsymbolAtime)
  ooexpressionBtime=rlang::ensym(oostringorsymbolBtime)
  ooexpressionBobserved=rlang::ensym(oostringorsymbolBobserved)
  ooexpressionCtime=rlang::ensym(oostringorsymbolCtime)
  ooexpressionCobserved=rlang::ensym(oostringorsymbolCobserved)
  
  if(base::length(oolistfunctionweightasafunctionofstminus) < 1L)
  {
    base::stop("oolistfunctionweightasafunctionofstminus must have length greater than or equal to 1.")
  }
  
  oodoublemaxcomboteststatistic={
    if(base::is.null(ooodoublemaxcomboteststatistic) )
    {
      oogetdoublemaxcomboteststatistic(oodataframe = oodataframe,oolistfunctionweightasafunctionofstminus = oolistfunctionweightasafunctionofstminus,oostringorsymbolid = !!ooexpressionid,oostringorsymboltreated = !!ooexpressiontreated,oostringorsymbolAtime = !!ooexpressionAtime,oostringorsymbolBtime = !!ooexpressionBtime,oostringorsymbolBobserved = !!ooexpressionBobserved,oostringorsymbolCtime = !!ooexpressionCtime,oostringorsymbolCobserved = !!ooexpressionCobserved)
    }else
    {
      ooodoublemaxcomboteststatistic
    }
  }
  
  
  oointp = base::length(oolistfunctionweightasafunctionofstminus)
  oomatrixdoublecovariancematrix = base::matrix(NA,nrow = oointp,ncol = oointp)
  
  for(oointj in 1L:oointp)
  {
    for(oointk in 1L:oointj)
    {
      oomatrixdoublecovariancematrix[[oointj,oointk]]=oogetresultsbasiccovariance3vRECODED(oodataframet1 = oodataframe,oodataframet2 = oodataframe,oofunctionweightasafunctionofstminus1 = oolistfunctionweightasafunctionofstminus[[oointj]],oofunctionweightasafunctionofstminus2 = oolistfunctionweightasafunctionofstminus[[oointk]],oostringorsymbolid = !!ooexpressionid,oostringorsymboltreated = !!ooexpressiontreated,oostringorsymbolAtime = !!ooexpressionAtime,oostringorsymbolBtime = !!ooexpressionBtime,oostringorsymbolBobserved = !!ooexpressionBobserved,oostringorsymbolCtime = !!ooexpressionCtime,oostringorsymbolCobserved = !!ooexpressionCobserved)
      oomatrixdoublecovariancematrix[[oointk,oointj]]=oomatrixdoublecovariancematrix[[oointj,oointk]]
    }
  }
  
  oomatrixdoublecorrelationmatrix = oomatrixdoublecovariancematrix %>% stats::cov2cor()
  
  base::return(oogetdoublepvalue(oovecintnumberOfVariablesInEachSegment=c(oointp),oovecdoublecutoffsUsedInEachPreviousSegmentAndObservedValueInLastSegment=c(oodoublemaxcomboteststatistic),oomatrixdoublesigma=oomatrixdoublecorrelationmatrix,oointnrep=100000L) %>% base::as.double() ) #note that it doesn't really matter what is supplied to the oointnrep argument here, since the value isn't actually used for anything in this case.  the base::as.double is used to get rid of some of the annoying names and attributes that are part of the result provided by oogetdoublepvalue; however they are useful and I could add an argument to this function to allow the user to specify whether or not they should be provided.
  
}


#' Compute the cutoff value for the max-combo test statistic at a single timepoint
#' 
#' @description 
#' This function computes the cutoff value for a user-specified nominal type I error rate (also known as a user-specified "alpha") for the max-combo test statistic for comparing times to an event from two different arms at a single timepoint; please note that this is *not* a cutoff for a group sequential test procedure involving two or more timepoints, but rather a cutoff for a scenario in which there is only one planned analysis timepoint.  If the true survival curves from the two arms are not at all different, then the probability, before performing the experiment, that the max-combo statistic will be greater than the cutoff value returned by this function, is less than or approximately equal to (but typically approximately equal to) the user-specified nominal type I error rate (e.g., 0.025 for a confirmatory clinical trial). The max-combo test statistic is the maximum of multiple standardized weighted log-rank test statistics.  The user of the function is expected to provide the function with a data frame containing the time-to-event data for the two arms; additionally, this data frame is expected to be in a particular format (see more details below).  Additionally, the user of the function is expected to provide a list of one or more weighting functions to use for the respective one or more weighted log-rank tests; in particular this function only allows the user to specify a weighting function that takes as input the Kaplan-Meier estimate for the survival curve obtained by pooling both of the two arms (a double that is between 0 and 1, inclusive) and provides as output a weight (a double that is typically greater than or equal to zero); note that the Fleming-Harrington class of weighting functions falls within what can be specified in this way.  If no list of weighting functions is provided, a single weighting function that is identically 1 is used, which means that the cutoff that is returned is exactly equal to the cutoff for the usual standardized log-rank test statistic (e.g., approximately 1.96 for a user-specified nominal type I error rate of 0.025).
#' 
#' The supplied data frame is expected to have one row for each subject.  The data frame should have the following columns:
#' * `id` - a variable that takes a unique value for each subject
#' * `treated` - should be 0 for subjects in the control arm and 1 for subjects in the experimental arm.
#' * `Atime` - the absolute time the subject enters the study
#' * `Btime` - the absolute time of an event or censoring, whichever comes first
#' * `Bobserved` - should be TRUE if the event was observed
#' * `Ctime` - the absolute time of an event or censoring, whichever comes first
#' * `Cobserved` - should be TRUE if censoring occurred (and so the event was not observed)
#' 
#' The columns in the data frame do not have to be named exactly as above; however, if different names are used, those names must be specified as arguments to the function.  Please see below for more detail regarding how to specify those names and for more detail regarding the expectations of the function regarding the columns in the data frame.
#' 
#' @param oodataframe A data frame containing time-to-event data from two different arms.
#' @param oolistfunctionweightasafunctionofstminus A list of one or more weighting functions.  Each weighting function should take as input the pooled Kaplan-Meier estimate of survival across the two arms (i.e., a double between 0 and 1, inclusive) and output a single weight (i.e., a double, typically greater than or equal to zero).  Defaults to the following list of a single weighting function that is identically 1, which results in the usual standardized log-rank test statistic: `base::list(function(oodoublestminus){ base::return(1) } )`
#' @param oostringorsymbolid The name of the column in the supplied data frame for the id variable.  Defaults to "id".  The id variable should take a unique value for each subject.  The column with this name in the data frame can be an integer vector with a different integer for each subject; the function will most likely also work if the column is a character vector with a different value for each subject.
#' @param oostringorsymboltreated The name of the column in the supplied data frame for a treatment indicator variable.  Defaults to "treated".  The treatment indicator variable is a variable indicating which of the two arms the subject is in.  The column with this name in the data frame should be an integer vector that takes only the value 0L (e.g., for placebo) or the value 1L (e.g., for a new drug or therapy).  The test statistic that is returned by this function will generally be for a test that the arm represented by subjects with treatment indicator 1L is superior to the arm represented by subjects with treatment indicator 0L (i.e., a one-sided test of superiority of arm 1 over arm 0).
#' @param oostringorsymbolAtime The name of the column in the supplied data frame for the Atime variable.  Defaults to "Atime".  Atime is the absolute time the subject enters the study.  The column with this name in the data frame can be vector of doubles; the function will most likely also work if the column is a vector of integers.
#' @param oostringorsymbolBtime The name of the column in the supplied data frame for the Btime variable.  Defaults to "Btime".  Btime is the absolute time of an event or censoring, whichever comes first.  The column with this name in the data frame can be vector of doubles; the function will most likely also work if the column is a vector of integers.
#' @param oostringorsymbolBobserved The name of the column in the supplied data frame for the Bobserved variable.  Defaults to "Bobserved".  Bobserved should be TRUE if the event was observed.  The column with this name in the data frame can be a logical vector; the function will most likely also work if the column is a vector of integers (i.e., with 1L in place of TRUE and 0L in place of FALSE).
#' @param oostringorsymbolCtime The name of the column in the supplied data frame for the Ctime variable.  Defaults to "Ctime".  Ctime is the absolute time of an event or censoring, whichever comes first.  The column with this name in the data frame can be vector of doubles; the function will most likely also work if the column is a vector of integers.
#' @param oostringorsymbolCobserved The name of the column in the supplied data frame for the Cobserved variable.  Defaults to "Cobserved".  Cobserved should be TRUE if censoring occurred (and so the event was not observed).  The column with this name in the data frame can be a logical vector; the function will most likely also work if the column is a vector of integers (i.e., with 1L in place of TRUE and 0L in place of FALSE).
#' @param oodoublealpha A double that is the user-specified nominal type I error rate.  Defaults to 0.025.  This value should be strictly between 0 and 1.
#' @param oointnmaxiter An integer that is maximum number of iterations used by the bisection method while finding the cutoff.  Defaults to 25L.  This value can be increased to 50L, 100L, or 200L if greater accuracy is desired while finding the cutoff; anything greater than that would typically not be needed.
#' @param oodoublelower A double that is the lower bound of the interval within which the cutoff is searched for.  Defaults to -10.
#' @param oodoubleupper A double that is the upper bound of the interval within which the cutoff is searched for.  Defaults to 10.
#' @param oodoublefscaletolerance A double that is related to early stopping of the bisection method; if the bisection method finds a cutoff value for which the absolute value of the difference between the nominal type I error rate when using that cutoff value and the user-specified nominal type I error rate is less than or equal to `oodoublefscaletolerance`, the bisection method will stop and return that cutoff value.  Defaults to 0.00001.  This value can be decreased to a smaller strictly positive value if greater accuracy is desired while finding the cutoff.
#' 
#' @return A double
#' 
#' @export
oogetdoublemaxcombocutoff=function(oodataframe,oolistfunctionweightasafunctionofstminus=base::list(function(oodoublestminus){ base::return(1.0) } ),oostringorsymbolid="id",oostringorsymboltreated="treated",oostringorsymbolAtime="Atime",oostringorsymbolBtime="Btime",oostringorsymbolBobserved="Bobserved",oostringorsymbolCtime="Ctime",oostringorsymbolCobserved="Cobserved",oodoublealpha=0.025,oointnmaxiter=25L,oodoublelower=-10,oodoubleupper=10,oodoublefscaletolerance=0.00001)
{
  ooexpressionid=rlang::ensym(oostringorsymbolid)
  ooexpressiontreated=rlang::ensym(oostringorsymboltreated)
  ooexpressionAtime=rlang::ensym(oostringorsymbolAtime)
  ooexpressionBtime=rlang::ensym(oostringorsymbolBtime)
  ooexpressionBobserved=rlang::ensym(oostringorsymbolBobserved)
  ooexpressionCtime=rlang::ensym(oostringorsymbolCtime)
  ooexpressionCobserved=rlang::ensym(oostringorsymbolCobserved)
  
  if(base::length(oolistfunctionweightasafunctionofstminus) < 1L)
  {
    base::stop("oolistfunctionweightasafunctionofstminus must have length greater than or equal to 1.")
  }
  
  base::return(
    oogetdoublecutofffrompastdatavRECODED(
      oolistdataframesbytimepoint = base::list(oodataframe),
      oolistlistweightingfunctionsbytimepoint = base::list(oolistfunctionweightasafunctionofstminus),
      oovecdoublecutoffsUsedInEachPreviousTimepoint = base::vector(mode="double",length=0L),
      oodoublealphaincrement = oodoublealpha,
      oointnrep = 100000L, #it doesn't matter what you put here, since it won't actually be used for anything since there is only a single timepoint so simulation is not used
      oodoublelower = oodoublelower,
      oodoubleupper = oodoubleupper,
      oointnmaxiter = oointnmaxiter,
      oodoublefscaletolerance = oodoublefscaletolerance,
      oostringorsymbolid = !!ooexpressionid,
      oostringorsymboltreated = !!ooexpressiontreated,
      oostringorsymbolAtime = !!ooexpressionAtime,
      oostringorsymbolBtime = !!ooexpressionBtime,
      oostringorsymbolBobserved = !!ooexpressionBobserved,
      oostringorsymbolCtime = !!ooexpressionCtime,
      oostringorsymbolCobserved = !!ooexpressionCobserved
    )
  )
  
}


#' Compute a cutoff value for a max-combo test statistic in a group sequential setting
#' 
#' @description 
#' This function computes a cutoff value for a max-combo test statistic that is used in a group sequential setting.  For example, consider the situation of a clinical trial with a planned interim analysis 2 years after the start of the clinical trial and a planned final analysis 4 years after the start of the clinical trial that plans to control the type I error rate across the interim and final analyses at the 0.025 level.  Suppose the interim analysis took place 2 years after a clinical trial began, and at the time of that interim analysis, equality of the true survival curves for the two arms was tested using a max-combo test statistic based on the log-rank test statistic and the weighted log-rank test statistic with Fleming-Harrington 1-0 weighting function using a nominal type I error rate of 0.06 x 0.025 = 0.0015, which is what might be done in a study using the typical and conservative O'Brien-Fleming-like type I error spending, and further suppose that that max-combo test failed to reject equality of the survival curves for the two arms.  Suppose that now 4 years have elapsed since the start of the study, so the final analysis is now taking place. Suppose that, as per the study protocol, the final analysis uses a max-combo test statistic based on the log-rank test statistic, the weighted log-rank test statistic with Fleming-Harrington 1-0 weighting function, *and* the weighted log-rank test statistic with Fleming-Harrington 0-1 weighting function.  This is a scenario in which this function might be used, e.g., to compute a cutoff value for the max-combo test statistic for the final analysis; if the max-combo test statistic for the final analysis exceeds the cutoff returned by this function, then the study can declare that the survival curves in the two arms are statistically significantly different according to its planned analysis (note that in this case, the `oodoublealphaincrement` argument should be specified to be 0.94 x 0.025 = 0.0235).
#' 
#' Please note that this function allows for the possibility that zero, one, two, or more interim analyses have occurred.  Please also note that the arguments to this function expect only data and information related to the current analysis and previous analyses; this function does not expect any information in its arguments related to future planned analyses or planned future type I error spending.  Also please note that, in a real clinical study, the timing of the analyses would most likely be based on the number of events that have occurred, possibly in addition to considerations related to elapsed time since the beginning of the study (e.g., "the interim analysis will take place after 50 events have occurred, and the final analysis will take place after 100 events have occurred or 4 years have elapsed since the start of the study, whichever occurs first"); this function is compatible with such scenarios.  If the true survival curves from the two arms are not at all different, then the probability, before performing the experiment, that at least one of the multiple max-combo statistics at the respective multiple analysis timepoints will be greater than its respective cutoff value for that timepoint returned by this function, is less than or approximately equal to (but typically approximately equal to) the nominal type I error rate for the group sequential experiment (e.g., 0.025 for a confirmatory clinical trial).
#' 
#' A max-combo test statistic is the maximum of multiple standardized weighted log-rank test statistics.  The user of the function is expected to provide the function with a *list* of data frames containing the time-to-event data for the two arms; this list should contain one data frame for each previous analysis that has occurred and one data frame for the current analysis, in order, from earliest analysis timepoint up to the current analysis timepoint.  Each such data frame is expected to be in a particular format (see more details below).  Additionally, the user of the function is expected to provide a *list* of lists of one or more weighting functions to use for the respective one or more weighted log-rank tests for each analysis timepoint; in particular this function only allows the user to specify a weighting function that takes as input the Kaplan-Meier estimate for the survival curve obtained by pooling both of the two arms (a double that is between 0 and 1, inclusive) and provides as output a weight (a double that is typically greater than or equal to zero); note that the Fleming-Harrington class of weighting functions falls within what can be specified in this way.  For each list of weighting functions for a previous analysis timepoint, the list should contain the actual weighting functions that were used at that analysis timepoint.  The list of weighting functions for the current analysis timepoint should contain the weighting functions that were planned to be used at the current analysis timepoint.
#' 
#' Each supplied data frame is expected to have one row for each subject.  Each data frame should have the following columns:
#' * `id` - a variable that takes a unique value for each subject
#' * `treated` - should be 0 for subjects in the control arm and 1 for subjects in the experimental arm.
#' * `Atime` - the absolute time the subject enters the study
#' * `Btime` - the absolute time of an event or censoring, whichever comes first
#' * `Bobserved` - should be TRUE if the event was observed
#' * `Ctime` - the absolute time of an event or censoring, whichever comes first
#' * `Cobserved` - should be TRUE if censoring occurred (and so the event was not observed)
#' 
#' The columns in the data frame do not have to be named exactly as above; however, if different names are used, those names must be specified as arguments to the function, and those names should be the same for each supplied data frame in the list of data frames supplied to the function.  Please see below for more detail regarding how to specify those names and for more detail regarding the expectations of the function regarding the columns in the data frames.
#' 
#' @param oolistdataframesbytimepoint A list of one or more data frames containing time-to-event data from two different arms.  This list should have one data frame for each analysis timepoint, in order from the earliest analysis timepoint up to the current analysis timepoint.  Each such supplied data frame for an analysis timepoint previous to the current analysis timepoint should contain the actual data that was used at that previous analysis timepoint.
#' @param oolistlistweightingfunctionsbytimepoint A list with one element for each analysis timepoint, in order from the earliest analysis timepoint up to the current analysis timepoint.  Each element should be itself a list of one or more weighting functions.  Each such element for an analysis timepoint previous to the current analysis timepoint should contain the actual weighting functions that were used at that previous analysis timepoint.  Each weighting function should take as input the pooled Kaplan-Meier estimate of survival across the two arms (i.e., a double between 0 and 1, inclusive) and output a single weight (i.e., a double, typically greater than or equal to zero).
#' @param oovecdoublecutoffsUsedInEachPreviousTimepoint A vector of doubles with one element for each *previous* analysis timepoint.  Each such element should contain the actual cutoff value (e.g., as was returned by a previous invocation of this function) that was used at that previous analysis timepoint.  Please note that the length of this argument should be exactly 1 less than the length of `oolistdataframesbytimepoint` and `oolistlistweightingfunctionsbytimepoint`, which should have exactly the same length.  If there were no previous analysis timepoints (e.g., the current analysis timepoint is the first planned interim analysis timepoint), then this argument should be specified to be `base::vector(mode="double",length=0L)`.
#' @param oodoublealphaincrement A double that is the user-specified nominal type I error rate for the *current* analysis timepoint.  In a group sequential experiment, the nominal type I error of the experiment is the *sum* of the nominal type I error rates for each respective analysis timepoint.  This value should be strictly greater than 0, and the sum of this value for the current analysis timepoint and any previous analysis timepoints should be strictly less than 1.
#' @param oostringorsymbolid The name of the column in the supplied data frames for the id variable.  Defaults to "id".  The id variable should take a unique value for each subject.  The column with this name in the data frames can be an integer vector with a different integer for each subject; the function will most likely also work if the column is a character vector with a different value for each subject.
#' @param oostringorsymboltreated The name of the column in the supplied data frames for a treatment indicator variable.  Defaults to "treated".  The treatment indicator variable is a variable indicating which of the two arms the subject is in.  The column with this name in the data frames should be an integer vector that takes only the value 0L (e.g., for placebo) or the value 1L (e.g., for a new drug or therapy).  The test statistic that is returned by this function will generally be for a test that the arm represented by subjects with treatment indicator 1L is superior to the arm represented by subjects with treatment indicator 0L (i.e., a one-sided test of superiority of arm 1 over arm 0).
#' @param oostringorsymbolAtime The name of the column in the supplied data frames for the Atime variable.  Defaults to "Atime".  Atime is the absolute time the subject enters the study.  The column with this name in the data frames can be vector of doubles; the function will most likely also work if the column is a vector of integers.
#' @param oostringorsymbolBtime The name of the column in the supplied data frames for the Btime variable.  Defaults to "Btime".  Btime is the absolute time of an event or censoring, whichever comes first.  The column with this name in the data frames can be vector of doubles; the function will most likely also work if the column is a vector of integers.
#' @param oostringorsymbolBobserved The name of the column in the supplied data frames for the Bobserved variable.  Defaults to "Bobserved".  Bobserved should be TRUE if the event was observed.  The column with this name in the data frames can be a logical vector; the function will most likely also work if the column is a vector of integers (i.e., with 1L in place of TRUE and 0L in place of FALSE).
#' @param oostringorsymbolCtime The name of the column in the supplied data frames for the Ctime variable.  Defaults to "Ctime".  Ctime is the absolute time of an event or censoring, whichever comes first.  The column with this name in the data frames can be vector of doubles; the function will most likely also work if the column is a vector of integers.
#' @param oostringorsymbolCobserved The name of the column in the supplied data frames for the Cobserved variable.  Defaults to "Cobserved".  Cobserved should be TRUE if censoring occurred (and so the event was not observed).  The column with this name in the data frame can be a logical vector; the function will most likely also work if the column is a vector of integers (i.e., with 1L in place of TRUE and 0L in place of FALSE).
#' @param oointnmaxiter An integer that is maximum number of iterations used by the bisection method while finding the cutoff.  Defaults to 25L.  This value can be increased to 50L, 100L, or 200L if greater accuracy is desired while finding the cutoff; anything greater than that would typically not be needed.
#' @param oodoublelower A double that is the lower bound of the interval within which the cutoff is searched for.  Defaults to -10.
#' @param oodoubleupper A double that is the upper bound of the interval within which the cutoff is searched for.  Defaults to 10.
#' @param oodoublefscaletolerance A double that is related to early stopping of the bisection method; if the bisection method finds a cutoff value for which the absolute value of the difference between the nominal type I error rate when using that cutoff value and the user-specified nominal type I error rate is less than or equal to `oodoublefscaletolerance`, the bisection method will stop and return that cutoff value.  Defaults to 0.00001.  This value can be decreased to a smaller strictly positive value if greater accuracy is desired while finding the cutoff.
#' @param oointnrep This function may use repeated simulations to estimate the type I error rate; this argument controls how many simulations are used to do this.  Defaults to 100000L.  There is typically not much need to change this argument from its default value.
#' 
#' @return A double
#' 
#' @export
oogetdoublemaxcombocutoffgroupsequential=function(oolistdataframesbytimepoint,oolistlistweightingfunctionsbytimepoint,oovecdoublecutoffsUsedInEachPreviousTimepoint,oodoublealphaincrement,oostringorsymbolid="id",oostringorsymboltreated="treated",oostringorsymbolAtime="Atime",oostringorsymbolBtime="Btime",oostringorsymbolBobserved="Bobserved",oostringorsymbolCtime="Ctime",oostringorsymbolCobserved="Cobserved",oointnmaxiter=25L,oodoublelower=-10,oodoubleupper=10,oodoublefscaletolerance=0.00001,oointnrep=100000L)
{
  ooexpressionid=rlang::ensym(oostringorsymbolid)
  ooexpressiontreated=rlang::ensym(oostringorsymboltreated)
  ooexpressionAtime=rlang::ensym(oostringorsymbolAtime)
  ooexpressionBtime=rlang::ensym(oostringorsymbolBtime)
  ooexpressionBobserved=rlang::ensym(oostringorsymbolBobserved)
  ooexpressionCtime=rlang::ensym(oostringorsymbolCtime)
  ooexpressionCobserved=rlang::ensym(oostringorsymbolCobserved)
  
  if(base::length(oolistdataframesbytimepoint) < 1L)
  {
    base::stop("oolistdataframesbytimepoint must have length greater than or equal to 1.")
  }else if(base::length(oolistdataframesbytimepoint) != base::length(oolistlistweightingfunctionsbytimepoint) )
  {
    base::stop("oolistdataframesbytimepoint and oolistlistweightingfunctionsbytimepoint must have the same length.")
  }else if(base::length(oovecdoublecutoffsUsedInEachPreviousTimepoint) != base::length(oolistdataframesbytimepoint) - 1L)
  {
    base::stop("oovecdoublecutoffsUsedInEachPreviousTimepoint must have length exactly equal to 1 less than the length of oolistdataframesbytimepoint.")
  }
  
  base::return(
    oogetdoublecutofffrompastdatavRECODED(
      oolistdataframesbytimepoint = oolistdataframesbytimepoint,
      oolistlistweightingfunctionsbytimepoint = oolistlistweightingfunctionsbytimepoint,
      oovecdoublecutoffsUsedInEachPreviousTimepoint = oovecdoublecutoffsUsedInEachPreviousTimepoint,
      oodoublealphaincrement = oodoublealphaincrement,
      oointnrep = oointnrep,
      oodoublelower = oodoublelower,
      oodoubleupper = oodoubleupper,
      oointnmaxiter = oointnmaxiter,
      oodoublefscaletolerance = oodoublefscaletolerance,
      oostringorsymbolid = !!ooexpressionid,
      oostringorsymboltreated = !!ooexpressiontreated,
      oostringorsymbolAtime = !!ooexpressionAtime,
      oostringorsymbolBtime = !!ooexpressionBtime,
      oostringorsymbolBobserved = !!ooexpressionBobserved,
      oostringorsymbolCtime = !!ooexpressionCtime,
      oostringorsymbolCobserved = !!ooexpressionCobserved
    )
  )
  
}