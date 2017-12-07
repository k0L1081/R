#Clear console.
cat("\f")

# Convert individual Roman Characters to Integers.
# I: 1
# V: 5
# X: 10
# L: 50
# C: 100
# D: 500
# M: 1000
RomanCharacterToIntegerValue <- function(RomanCharacter)
{
  if (RomanCharacter == "I") {
    IntegerValue <- 1
  } else if (RomanCharacter == "V") {
    IntegerValue <- 5
  } else if (RomanCharacter == "X") {
    IntegerValue <- 10
  } else if (RomanCharacter == "L") {
    IntegerValue <- 50
  } else if (RomanCharacter == "C") {
    IntegerValue <- 100
  } else if (RomanCharacter == "D") {
    IntegerValue <- 500
  } else if (RomanCharacter == "M") {
    IntegerValue <- 1000
  } else {
    IntegerValue <- NA}

  return(IntegerValue)
}

# Unit Test: RomanCharacterToIntegerValue
ifelse(identical(RomanCharacterToIntegerValue("X"), 10), yes = "Passed", no = "Failed")
ifelse(identical(RomanCharacterToIntegerValue("D"), 500), yes = "Passed", no = "Failed")
ifelse(identical(RomanCharacterToIntegerValue("M"), 1000), yes = "Passed", no = "Failed")
ifelse(identical(RomanCharacterToIntegerValue("A"), NA), yes = "Passed", no = "Failed")
ifelse(identical(RomanCharacterToIntegerValue("c"), NA), yes = "Passed", no = "Failed")

# Convert characters (a string) to vector of characters.
ConvertCharactersToVector <- function(Characters){
  return(unlist(strsplit(Characters,"")))
}

# Unit Test: ConvertCharactersToVector
ifelse(identical(ConvertCharactersToVector("LXXIV"), c("L", "X", "X", "I", "V")), yes = "Passed", no = "Failed")
ifelse(identical(ConvertCharactersToVector("DCCV"), c("D", "C", "C", "V")), yes = "Passed", no = "Failed")
ifelse(identical(ConvertCharactersToVector("LXXIII"), c("L", "X", "X", "I", "I", "I")), yes = "Passed", no = "Failed")
ifelse(!identical(ConvertCharactersToVector("CXXI"), c("X", "X", "I")), yes = "Passed", no = "Failed")
ifelse(!identical(ConvertCharactersToVector("DCXIII"), c("D", "X", "I", "I", "I")), yes = "Passed", no = "Failed")
ifelse(!identical(ConvertCharactersToVector("MCXXI"), c("M", "X", "I")), yes = "Passed", no = "Failed")

# Function to check for invalid Roman characters.
# If character is not in: I, V, X, L, C, D, M, return FALSE else return TRUE.
AreValidRomanCharacters <- function(RomanCharacters) {
  # List of valid Roman characters
  ValidRomanCharacters <- c("I", "V", "X", "L", "C", "D", "M")
  
  # Split input string into vector of charaacters.
  RomanCharactersSequence <- ConvertCharactersToVector(RomanCharacters)
  
  # Compare each vector element to list of valid Roman characters.
  # Create matrix containing results of each comparison.
  AreValidRomanCharacters <- sapply(RomanCharactersSequence,
                                    FUN = function(rc){is.element(rc, ValidRomanCharacters)})
  
  # If the matrix does not contain a FALSE value, then all characters are valid.
  if (!is.element(FALSE, AreValidRomanCharacters)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Unit Test: AreValidRomanCharacters
ifelse(identical(AreValidRomanCharacters("XX"), TRUE), yes = "Passed", no = "Failed")
ifelse(identical(AreValidRomanCharacters("LXXX"), TRUE), yes = "Passed", no = "Failed")
ifelse(identical(AreValidRomanCharacters("MD"), TRUE), yes = "Passed", no = "Failed")
ifelse(identical(AreValidRomanCharacters("ABC"), FALSE), yes = "Passed", no = "Failed")
ifelse(identical(AreValidRomanCharacters("abc"), FALSE), yes = "Passed", no = "Failed")

InvalidNegationSequencesExists <- function(RomanCharacters) {
  ValidNegationSequences <- c("IV", "IX", "XL", "XC", "CD", "CM")
  
  for (Sequence in ValidNegationSequences){
    SequenceParts <- ConvertCharactersToVector(Sequence)
    SequencePart1 <- SequenceParts[1]
    SequencePart2 <- SequenceParts[2]
    
    # Check for multiple negation characters preceding negation sequence. E.g.,
    # IIV, IIIV, XXL, XXXL, CCM, CCCM
    SearchString = paste(SequencePart1, "{2,}", SequencePart2, sep="" )
    
    SearchStringExists <- grep(SearchString, RomanCharacters)
    
    if(length(SearchStringExists) > 0){
      return(TRUE)
    }
    
    # Check for negation characters following negation sequence. E.g.,
    # IVI, IXI, XLX, XCX, CDC, CMC
    SearchString = paste(SequencePart1, SequencePart2, SequencePart1, "{1,}", sep="" )
    
    SearchStringExists <- grep(SearchString, RomanCharacters)
    
    if(length(SearchStringExists) > 0){
      return(TRUE)
    }
    
    # Check for negated characters following negation sequence. E.g.,
    # IVV, IXX, XLL, XCC, CDD, CMM
    SearchString = paste(SequencePart1, SequencePart2, "{2,}", sep="" )
    
    SearchStringExists <- grep(SearchString, RomanCharacters)
    
    if(length(SearchStringExists) > 0){
      return(TRUE)
    }
  }
  
  return(FALSE)
}

# Unit Test: InvalidNegationSequencesExists
ifelse(identical(InvalidNegationSequencesExists("IIIV"), TRUE), yes = "Passed", no = "Failed")
ifelse(identical(InvalidNegationSequencesExists("IXII"), TRUE), yes = "Passed", no = "Failed")
ifelse(identical(InvalidNegationSequencesExists("XCC"), TRUE), yes = "Passed", no = "Failed")
ifelse(identical(InvalidNegationSequencesExists("VL"), FALSE), yes = "Passed", no = "Failed")
ifelse(identical(InvalidNegationSequencesExists("CD"), FALSE), yes = "Passed", no = "Failed")

InvalidRepeatedCharacterSequenceExists <- function(RomanCharacters){
  for(RomanCharacter in ConvertCharactersToVector(RomanCharacters)){
    # Search for two or more occurences of D, L or V.
    # If two or more occurences exist, the Roman character sequence is invalid.
    if(is.element(RomanCharacter, c("D", "L", "V"))){
      SearchString <- paste(RomanCharacter, ".*", RomanCharacter, sep="" )
      SearchStringExists <- grep(SearchString, RomanCharacters)
      
      if(length(SearchStringExists) > 0){
        return(TRUE)
      }
    }
    
    # Search for four or more of any character.
    # If four or more of any character sequence exists, the Roman character sequence is invalid.
    SearchString <- paste(RomanCharacter, ".*", RomanCharacter, ".*", RomanCharacter, ".*", RomanCharacter, sep="" )
    SearchStringExists <- grep(SearchString, RomanCharacters)
    
    if(length(SearchStringExists) > 0){
      return(TRUE)
    }
  }
  
  return(FALSE)
}

# Unit Test: InvalidRepeatedCharacterSequenceExists
ifelse(identical(InvalidRepeatedCharacterSequenceExists("IVVV"), TRUE), yes = "Passed", no = "Failed")
ifelse(identical(InvalidRepeatedCharacterSequenceExists("LLLL"), TRUE), yes = "Passed", no = "Failed")
ifelse(identical(InvalidRepeatedCharacterSequenceExists("XDXDX"), TRUE), yes = "Passed", no = "Failed")
ifelse(identical(InvalidRepeatedCharacterSequenceExists("XXX"), FALSE), yes = "Passed", no = "Failed")
ifelse(identical(InvalidRepeatedCharacterSequenceExists("XCIII"), FALSE), yes = "Passed", no = "Failed")

RomanCharactersToInteger <- function(RomanCharacters) {
  # Convert Roman Characters into vector.
  RomanCharacterSequence <- ConvertCharactersToVector(RomanCharacters)
  
  # Check for the existence of invalid characters.
  if(!AreValidRomanCharacters(RomanCharacters)) {
    return("invalid input")
  }

  # Check for invalid negation sequences.
  if(InvalidNegationSequencesExists(RomanCharacters)){
    return("invalid input")
  }

  # Check for invalid repeated character sequences:
  #   Repeated D, L & V characers
  #   Four or more of any character: I, V, X, L, C, D, M
  if (InvalidRepeatedCharacterSequenceExists(RomanCharacters)){
    return("invalid input")
  }
  
  # Declare variable(s).
  IntegerSequence <- 0
  i <- 1

  RomanCharacterDataFrame <- data.frame(RomanCharacterSequence, number = 0, processed = FALSE)
  #names(RomanCharacterDataFrame) <- RomanCharacterSequence
     
  while (i <= nrow(RomanCharacterDataFrame)) {
    
    # Populate variable(s).
    CurrentRomanCharacter <- RomanCharacterDataFrame[i, 1]
    PreviousRomanCharacter <- ""
    NextPreviousRomanCharacter <- ""
        
    if (i >= 2) {
      PreviousRomanCharacter <- RomanCharacterDataFrame[(i - 1), 1]
    }
  
    if (i >= 3) {
      NextPreviousRomanCharacter <- RomanCharacterDataFrame[(i - 2), 1]
    }
    
    # Process negation Roman Character sequences:
    # IV: 5 - 1 = 4
    # IX: 10 - 1 = 9
    # XL: 50 - 10 = 40
    # XC: 100 - 10 = 90
    # CD: 500 - 100 = 400
    # CM: 1000 - 100 = 900
    if ((PreviousRomanCharacter == "I" & CurrentRomanCharacter == "V")
        | (PreviousRomanCharacter == "I" & CurrentRomanCharacter == "X")
        | (PreviousRomanCharacter == "X" & CurrentRomanCharacter == "L")
        | (PreviousRomanCharacter == "X" & CurrentRomanCharacter == "C")
        | (PreviousRomanCharacter == "C" & CurrentRomanCharacter == "D")
        | (PreviousRomanCharacter == "C" & CurrentRomanCharacter == "M")
        ){
      
      # Check for invalid character sequences: Two or more consecutive negation characters. E.g.:
      # I: IIV, IIIV, IIX, etc.
      # X: XXV, XXC, XXXC, etc.
      # C: CCD, CCCD, CCM, etc.
      if (NextPreviousRomanCharacter == PreviousRomanCharacter) {
        return("invalid input")    
      }
      
      RomanCharacterDataFrame[i, 2] <- RomanCharacterToIntegerValue(CurrentRomanCharacter)
      RomanCharacterDataFrame[(i - 1), 2] <- -RomanCharacterToIntegerValue(PreviousRomanCharacter)
      
      RomanCharacterDataFrame[i, 3] <- TRUE
      RomanCharacterDataFrame[(i - 1), 3] <- TRUE
    }

    if (RomanCharacterDataFrame[i,3] == FALSE) {

      RomanCharacterDataFrame[i, 2] <- RomanCharacterToIntegerValue(CurrentRomanCharacter)
      RomanCharacterDataFrame[i, 3] <- TRUE
    }

    i <- i + 1
  }

  IntegerTotal <- sum(RomanCharacterDataFrame[c("number")])

  return(IntegerTotal)
}

# Unit Test: RomanCharactersToInteger
ifelse(identical(RomanCharactersToInteger("MCMLXIX"), 1969), yes = "Passed", no = "Failed")
ifelse(identical(RomanCharactersToInteger("CXXXIV"), 134), yes = "Passed", no = "Failed")
ifelse(identical(RomanCharactersToInteger("XCIII"), 93), yes = "Passed", no = "Failed")
ifelse(identical(RomanCharactersToInteger("XXXX"), "invalid input"), yes = "Passed", no = "Failed")
ifelse(identical(RomanCharactersToInteger("XCXXX"), "invalid input"), yes = "Passed", no = "Failed")

RomanCharactersToInteger("MCMLXIX")