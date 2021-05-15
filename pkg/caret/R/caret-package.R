

#' Blood Brain Barrier Data
#'
#' Mente and Lombardo (2005) develop models to predict the log
#' of the ratio of the concentration of a compound in the brain
#' and the concentration in blood. For each compound, they computed
#' three sets of molecular descriptors: MOE 2D, rule-of-five and
#' Charge Polar Surface Area (CPSA). In all, 134 descriptors were
#' calculated. Included in this package are 208 non-proprietary
#' literature compounds. The vector \code{logBBB} contains the concentration
#' ratio and the data fame \code{bbbDescr} contains the descriptor values.
#'
#'
#' @name BloodBrain
#' @aliases BloodBrain bbbDescr logBBB
#' @docType data
#' @return \item{bbbDescr}{data frame of chemical descriptors} \item{logBBB}{vector of assay results}
#'
#' @source Mente, S.R. and Lombardo, F. (2005). A recursive-partitioning model
#' for blood-brain barrier permeation, \emph{Journal of Computer-Aided Molecular Design},
#' Vol. 19, pg. 465-481.
#'
#' @keywords datasets
NULL


#' COX-2 Activity Data
#'
#' From Sutherland, O'Brien, and Weaver (2003): "A set of 467 cyclooxygenase-2
#' (COX-2) inhibitors has been assembled from the published work of a single
#' research group, with in vitro activities against human recombinant enzyme
#' expressed as IC50 values ranging from 1 nM to >100 uM (53 compounds have
#' indeterminate IC50 values)."
#'
#' The data are in the Supplemental Data file for the article.
#'
#' A set of 255 descriptors (MOE2D and QikProp) were generated. To classify the
#' data, we used a cutoff of $2^2.5$ to determine activity
#'
#'
#' @name cox2
#' @aliases cox2 cox2Class cox2Descr cox2IC50
#' @docType data
#' @return \item{cox2Descr}{the descriptors} \item{cox2IC50}{the IC50 data used
#' to determine activity}
#'
#' \item{cox2Class}{the categorical outcome ("Active" or "Inactive") based on
#' the $2^2.5$ cutoff}
#' @source Sutherland, J. J., O'Brien, L. A. and Weaver, D. F. (2003).
#' Spline-Fitting with a Genetic Algorithm: A Method for Developing
#' Classification Structure-Activity Relationships, \emph{Journal of Chemical
#' Information and Computer Sciences}, Vol. 43, pg. 1906-1915.
#' @keywords datasets
NULL





#' Dihydrofolate Reductase Inhibitors Data
#'
#' Sutherland and Weaver (2004) discuss QSAR models for dihydrofolate reductase
#' (DHFR) inhibition. This data set contains values for 325 compounds. For each
#' compound, 228 molecular descriptors have been calculated. Additionally, each
#' samples is designated as "active" or "inactive".
#'
#' The data frame \code{dhfr} contains a column called \code{Y} with the
#' outcome classification. The remainder of the columns are molecular
#' descriptor values.
#'
#'
#' @name dhfr
#' @docType data
#' @return \item{dhfr}{data frame of chemical descriptors and the activity
#' values}
#' @source Sutherland, J.J. and Weaver, D.F. (2004). Three-dimensional
#' quantitative structure-activity and structure-selectivity relationships of
#' dihydrofolate reductase inhibitors, \emph{Journal of Computer-Aided
#' Molecular Design}, Vol. 18, pg. 309-331.
#' @keywords datasets
NULL





#' German Credit Data
#'
#' Data from Dr. Hans Hofmann of the University of Hamburg.
#'
#' These data have two classes for the credit worthiness: good or bad. There
#' are predictors related to attributes, such as: checking account status,
#' duration, credit history, purpose of the loan, amount of the loan, savings
#' accounts or bonds, employment duration, Installment rate in percentage of
#' disposable income, personal information, other debtors/guarantors, residence
#' duration, property, age, other installment plans, housing, number of
#' existing credits, job information, Number of people being liable to provide
#' maintenance for, telephone, and foreign worker status.
#'
#' Many of these predictors are discrete and have been expanded into several
#' 0/1 indicator variables
#'
#'
#' @name GermanCredit
#' @docType data
#' @source UCI Machine Learning Repository
#' @keywords datasets
NULL




#' Multidrug Resistance Reversal (MDRR) Agent Data
#'
#' Svetnik et al. (2003) describe these data: "Bakken and Jurs studied a set of
#' compounds originally discussed by Klopman et al., who were interested in
#' multidrug resistance reversal (MDRR) agents. The original response variable
#' is a ratio measuring the ability of a compound to reverse a leukemia cell's
#' resistance to adriamycin. However, the problem was treated as a
#' classification problem, and compounds with the ratio >4.2 were considered
#' active, and those with the ratio <= 2.0 were considered inactive. Compounds
#' with the ratio between these two cutoffs were called moderate and removed
#' from the data for twoclass classification, leaving a set of 528 compounds
#' (298 actives and 230 inactives). (Various other arrangements of these data
#' were examined by Bakken and Jurs, but we will focus on this particular one.)
#' We did not have access to the original descriptors, but we generated a set
#' of 342 descriptors of three different types that should be similar to the
#' original descriptors, using the DRAGON software."
#'
#' The data and R code are in the Supplemental Data file for the article.
#'
#'
#' @name mdrr
#' @aliases mdrr mdrrClass mdrrDescr
#' @docType data
#' @return \item{mdrrDescr}{the descriptors} \item{mdrrClass}{the categorical
#' outcome ("Active" or "Inactive")}
#' @source Svetnik, V., Liaw, A., Tong, C., Culberson, J. C., Sheridan, R. P.
#' Feuston, B. P (2003).  Random Forest: A Classification and Regression Tool
#' for Compound Classification and QSAR Modeling, \emph{Journal of Chemical
#' Information and Computer Sciences}, Vol. 43, pg. 1947-1958.
#' @keywords datasets
NULL




#' Fatty acid composition of commercial oils
#'
#' Fatty acid concentrations of commercial oils were measured using gas
#' chromatography.  The data is used to predict the type of oil.  Note that
#' only the known oils are in the data set. Also, the authors state that there
#' are 95 samples of known oils. However, we count 96 in Table 1 (pgs.  33-35).
#'
#'
#' @name oil
#' @aliases oil oilType fattyAcids
#' @docType data
#' @return \item{fattyAcids}{data frame of fatty acid compositions: Palmitic,
#' Stearic, Oleic, Linoleic, Linolenic, Eicosanoic and Eicosenoic. When values
#' fell below the lower limit of the assay (denoted as <X in the paper), the
#' limit was used. } \item{oilType}{factor of oil types: pumpkin (A), sunflower
#' (B), peanut (C), olive (D), soybean (E), rapeseed (F) and corn (G).}
#' @source Brodnjak-Voncina et al. (2005). Multivariate data analysis in
#' classification of vegetable oils characterized by the content of fatty
#' acids, \emph{Chemometrics and Intelligent Laboratory Systems}, Vol.
#' 75:31-45.
#' @keywords datasets
NULL





#' Pottery from Pre-Classical Sites in Italy
#'
#' Measurements of 58 pottery samples.
#'
#'
#' @name pottery
#' @aliases pottery potteryClass
#' @docType data
#' @return \item{pottery}{11 elemental composition measurements }
#' \item{potteryClass}{factor of pottery type: black carbon containing bulks
#' (A) and clayey (B)}
#' @source R. G. Brereton (2003). \emph{Chemometrics: Data Analysis for the
#' Laboratory and Chemical Plant}, pg. 261.
#' @keywords datasets
NULL





#' Sacramento CA Home Prices
#'
#' This data frame contains house and sale price data for 932 homes in
#' Sacramento CA.  The original data were obtained from the website for the
#' SpatialKey software. From their website: "The Sacramento real estate
#' transactions file is a list of 985 real estate transactions in the
#' Sacramento area reported over a five-day period, as reported by the
#' Sacramento Bee." Google was used to fill in missing/incorrect data.
#'
#'
#' @name Sacramento
#' @docType data
#' @return \item{Sacramento}{a data frame with columns '\code{city}',
#' '\code{zip}', '\code{beds}', '\code{baths}', '\code{sqft}', '\code{type}',
#' '\code{price}', '\code{latitude}', and '\code{longitude}'}
#' @source SpatialKey website:
#' \url{https://support.spatialkey.com/spatialkey-sample-csv-data/}
#' @keywords datasets
#' @examples
#'
#' data(Sacramento)
#'
#' set.seed(955)
#' in_train <- createDataPartition(log10(Sacramento$price), p = .8, list = FALSE)
#'
#' training <- Sacramento[ in_train,]
#' testing  <- Sacramento[-in_train,]
#'
#'
NULL





#' Morphometric Data on Scat
#'
#' Reid (2015) collected data on animal feses in coastal California. The data
#' consist of DNA verified species designations as well as fields related to
#' the time and place of the collection and the scat itself. The data frame
#' \code{scat_orig} contains while \code{scat} contains data on the three main
#' species.
#'
#'
#' @name scat
#' @aliases scat scat_orig
#' @docType data
#' @return \item{scat_orig}{the entire data set in the Supplemental Materials}
#' \item{scat}{data on the three main species}
#' @source Reid, R. E. B. (2015). A morphometric modeling approach to
#' distinguishing among bobcat, coyote and gray fox scats. \emph{Wildlife
#' Biology}, 21(5), 254-262
#' @keywords datasets
NULL

#' Kelly Blue Book resale data for 2005 model year GM cars
#'
#' Kuiper (2008) collected data on Kelly Blue Book resale data for 804 GM cars (2005 model year).
#'
#' @name cars
#' @docType data
#' @return \item{cars}{data frame of the suggested retail price (column \code{Price}) and various
#' characteristics of each car (columns \code{Mileage}, \code{Cylinder}, \code{Doors}, \code{Cruise},
#' \code{Sound}, \code{Leather}, \code{Buick}, \code{Cadillac}, \code{Chevy}, \code{Pontiac}, \code{Saab},
#'  \code{Saturn}, \code{convertible}, \code{coupe}, \code{hatchback}, \code{sedan} and \code{wagon})}
#' @source Kuiper, S. (2008). Introduction to Multiple Regression: How Much Is Your Car Worth?,
#' \emph{Journal of Statistics Education}, Vol. 16
#' \url{http://jse.amstat.org/jse_archive.htm#2008}.
#' @keywords datasets
NULL

#' Cell Body Segmentation
#'
#' Hill, LaPan, Li and Haney (2007) develop models to predict which cells in a
#' high content screen were well segmented.  The data consists of 119 imaging
#' measurements on 2019. The original analysis used 1009 for training and 1010
#' as a test set (see the column called \code{Case}).
#'
#' The outcome class is contained in a factor variable called \code{Class} with
#' levels "PS" for poorly segmented and "WS" for well segmented.
#'
#' The raw data used in the paper can be found at the Biomedcentral website.
#' Versions of caret < 4.98 contained the original data. The version now
#' contained in \code{segmentationData} is modified. First, several discrete
#' versions of some of the predictors (with the suffix "Status") were removed.
#' Second, there are several skewed predictors with minimum values of zero
#' (that would benefit from some transformation, such as the log). A constant
#' value of 1 was added to these fields: \code{AvgIntenCh2},
#' \code{FiberAlign2Ch3}, \code{FiberAlign2Ch4}, \code{SpotFiberCountCh4} and
#' \code{TotalIntenCh2}.
#'
#' A binary version of the original data is at
#' \url{http://topepo.github.io/caret/segmentationOriginal.RData}.
#'
#'
#' @name segmentationData
#' @docType data
#' @return \item{segmentationData}{data frame of cells}
#' @source Hill, LaPan, Li and Haney (2007). Impact of image segmentation on
#' high-content screening data quality for SK-BR-3 cells, \emph{BMC
#' Bioinformatics}, Vol. 8, pg. 340,
#' \url{https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-8-340}.
#' @keywords datasets
NULL





#' Fat, Water and Protein Content of Meat Samples
#'
#' "These data are recorded on a Tecator Infratec Food and Feed Analyzer
#' working in the wavelength range 850 - 1050 nm by the Near Infrared
#' Transmission (NIT) principle. Each sample contains finely chopped pure meat
#' with different moisture, fat and protein contents.
#'
#' If results from these data are used in a publication we want you to mention
#' the instrument and company name (Tecator) in the publication.  In addition,
#' please send a preprint of your article to
#'
#' Karin Thente, Tecator AB, Box 70, S-263 21 Hoganas, Sweden
#'
#' The data are available in the public domain with no responsibility from the
#' original data source. The data can be redistributed as long as this
#' permission note is attached."
#'
#' "For each meat sample the data consists of a 100 channel spectrum of
#' absorbances and the contents of moisture (water), fat and protein.  The
#' absorbance is -log10 of the transmittance measured by the spectrometer. The
#' three contents, measured in percent, are determined by analytic chemistry."
#'
#' Included here are the traning, monitoring and test sets.
#'
#'
#' @name tecator
#' @aliases tecator absorp endpoints
#' @docType data
#' @return \item{absorp}{absorbance data for 215 samples. The first 129 were
#' originally used as a training set} \item{endpoints}{the percentages of
#' water, fat and protein}
#' @keywords datasets
#' @examples
#'
#' data(tecator)
#'
#' splom(~endpoints)
#'
#' # plot 10 random spectra
#' set.seed(1)
#' inSubset <- sample(1:dim(endpoints)[1], 10)
#'
#' absorpSubset <- absorp[inSubset,]
#' endpointSubset <- endpoints[inSubset, 3]
#'
#' newOrder <- order(absorpSubset[,1])
#' absorpSubset <- absorpSubset[newOrder,]
#' endpointSubset <- endpointSubset[newOrder]
#'
#' plotColors <- rainbow(10)
#'
#' plot(absorpSubset[1,],
#'      type = "n",
#'      ylim = range(absorpSubset),
#'      xlim = c(0, 105),
#'      xlab = "Wavelength Index",
#'      ylab = "Absorption")
#'
#' for(i in 1:10)
#' {
#'    points(absorpSubset[i,], type = "l", col = plotColors[i], lwd = 2)
#'    text(105, absorpSubset[i,100], endpointSubset[i], col = plotColors[i])
#' }
#' title("Predictor Profiles for 10 Random Samples")
#'
NULL



