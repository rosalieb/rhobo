% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rhobo.append.R
\name{rhobo.append}
\alias{rhobo.append}
\title{Routine HOBO part 2: Apply correction factors and append to previous files}
\usage{
rhobo.append(
  metadata_QAQC,
  path2data,
  foldername,
  TM_init,
  TM_end,
  write = TRUE
)
}
\arguments{
\item{metadata_QAQC}{output from the rhobo.CF() function. Data frame with the correction factors for all the new HOBO files.}

\item{path2data}{User's path to the folder where the data are stored. Within that folder, there should be two subfolders with the raw data (Hobo_Raw) and the processed data (Hobo_Process)}

\item{foldername}{Folder name of the newest data that have been downloaded post-HOBO deployment. Folder name should be the date of the end of the deployment with the format YYYY_MM_DD, e.g., 2021_07_27}

\item{TM_init}{Time measurement initialization : when probes are back in the lakes}

\item{TM_end}{Time measurement end: when probes are taken out of the lakes}

\item{write}{Whether to save the output file or not. Logical.}
}
\description{
Apply the correction factors to measured dissolved oxygen and append file for each lake to previous file. If no previous file was found, only the new file is kept.
}
\author{
Rosalie Bruel and Sophie Guillon
}
\keyword{hobo}
\keyword{planaqua}
