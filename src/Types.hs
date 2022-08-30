module Types where

newtype Start
    = Start Int

newtype Max
    = Max Int

data TryOuts
    = TryOuts Start Max

newtype Attempts
    = Attempts Int

newtype ToMatch = ToMatch String

newtype ToMatchAgainst = ToMatchAgainst String 

data Match 
    = Match Result
    | NoMatch String

newtype Result =
    Result
        { searchedWebsite ::  
        , selectedElement :: 
        , matchedPattern ::   
        }

data KindOfMatch
    = FirstMatch
    | HasBeenMatched
    | MatchAndTextBeforeAndAfter
    | AllMatches