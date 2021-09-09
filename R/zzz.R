# Custom Terms used for the US Supreme Court Citation Network
# See: https://github.com/desmarais-lab/Supreme_Court_Citation_Network

# Add start up message
.onAttach <- function(libname, pkgname) {
  desc  <- packageDescription(pkgname, libname)
  packageStartupMessage(
    'Package:  cERGM\n',
    'Version:  ', desc$Version, '\n',
    'Date:     ', desc$Date, '\n',
    'Authors:  Christian S Schmid (Pennsylvania State University)\n',
    '          Ted H Y Chen (University of Helsinki)\n',
    '          Bruce A. Desmarais (Pennsylvania State University)\n',
    'Contributor: Alex Stivala (University of Lugano)  \n',
    '\n\nPlease cite the cERGM package in your publications ',
    '-- see citation("cERGM").\n'
  )
}


