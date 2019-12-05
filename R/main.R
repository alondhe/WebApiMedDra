# @file Main
#
# Copyright 2019 Observational Health Data Sciences and Informatics
#
# This file is part of WebApiMedDra
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


.getSourceConceptId <- function(baseUrl, sourceCode, sourceVocabularyId) {
  url <- sprintf("%s/vocabulary/vocab/search/%s", baseUrl, sourceCode)
  json <- httr::GET(url)
  json <- httr::content(json)
  df <- do.call(rbind.data.frame, json)
  df$CONCEPT_ID[df$INVALID_REASON == "V" & df$VOCABULARY_ID == sourceVocabularyId &
                                   df$DOMAIN_ID == "Condition" & df$CONCEPT_CODE == sourceCode]
}

.sourceToStandard <- function(baseUrl, sourceConceptId) {
  df <- .getRelatedConcepts(baseUrl, sourceConceptId)
  df$CONCEPT_ID[df$STANDARD_CONCEPT == "S"]
}

.getRelatedConcepts <- function(baseUrl, conceptId) {
  url <- sprintf("%s/vocabulary/vocab/concept/%s/related", baseUrl, conceptId)
  json <- httr::GET(url)
  json <- httr::content(json)

  jsonList <- lapply(json, function(j) {
    j[sapply(j, is.null)] <- NA
    as.data.frame(j, stringsAsFactors = FALSE)
  })
  
  do.call(dplyr::bind_rows, jsonList)
}

.getPreferredMedDraTerm <- function(baseUrl, sourceCode, sourceVocabularyId) {
  sourceConceptId <- .getSourceConceptId(baseUrl, sourceCode, sourceVocabularyId)
  standardConceptId <- .sourceToStandard(baseUrl, sourceConceptId)
  df <- .getRelatedConcepts(baseUrl, standardConceptId)
  medDraConcept <- df[df$VOCABULARY_ID == "MedDRA" & df$CONCEPT_CLASS_ID == "PT" & df$RELATIONSHIPS.RELATIONSHIP_DISTANCE == 0,] %>%
    dplyr::select(CONCEPT_ID,
                  CONCEPT_NAME,
                  CONCEPT_CODE)
}


#' Get MedDRA Terms
#' 
#' @param sourceCodeDf     A data frame consisting of SOURCE_CODE, SOURCE_VOCABULARY_ID
#' @param baseUrl          The WebAPI endpoint
#' 
#' @export
getMedDraTerms <- function(sourceCodeDf, baseUrl) {
  
  result <- apply(sourceCodeDf, 1, function(s) {
    .getPreferredMedDraTerm(baseUrl, s["SOURCE_CODE"][[1]], s["SOURCE_VOCABULARY_ID"][[1]])
  })
  
  do.call(rbind.data.frame, result)
}