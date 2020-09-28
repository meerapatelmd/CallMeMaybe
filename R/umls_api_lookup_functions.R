#' @title
#' Lookup a Concept
#'
#' @details
#' '/content/current/CUI/C0009044'path Retrieves CUI and returns a JSON Object classType of Concept
#' '/content/current/CUI/C0009044/atoms'path Retrieve atoms in a CUI and returns a JSON Object classType of Atom
#' '/content/current/CUI/C0009044/definitions'path Retrieve CUI definitions and returns a JSON Object classType of Definition
#' '/content/current/CUI/C0009044/relations'path Retrieve CUI relations and returns a JSON Object classType of ConceptRelation
#' @seealso
#'  \code{\link[httr]{GET}},\code{\link[httr]{content}}
#' @rdname lookup_concept
#' @export
#' @importFrom httr GET content

lookup_concept <-
        function(CUI) {

                baseURL <- paste0("https://uts-ws.nlm.nih.gov/rest/content/current/CUI/",CUI)

                link_response <- httr::GET(url = baseURL,
                                           query = list(ticket = get_service_ticket())
                )

                link_response %>%
                        httr::content()
        }



#' @title
#' List Atoms
#'
#' @param sabs			 (optional)  Comma-separated list of source vocabularies to include in your search
#' @param ttys			 (optional)  One or more term types
#' @param language	         (optional)  Retrieve only atoms that have a specific language
#' @param includeObsolete	 (optional)  Include content that is obsolete according to the content provider or NLM.
#' @param includeSuppressible	 (optional)  Include content that is suppressible according to NLM Editors.
#' @param pageNumber	         (optional)  Whole number that specifies which page of results to fetch.
#' @param pageSize	         (optional)  Whole number that specifies the number of results to include per page.
#'
#' @details
#' '/content/current/CUI/C0155502/atoms'path Retrieves all atoms for C0155502 and returns a JSON Object classType of Atom
#' '/content/current/CUI/C0155502/atoms/preferred'path Retrieves the default preferred atom of C0155502 and returns a JSON Object classType of Atom
#' '/content/current/CUI/C0155502/atoms?language=ENG'path Retrieves all English language atoms for C0155502 and returns a JSON Object classType of Atom
#' '/content/current/CUI/C0155502/atoms?sabs=SNOMEDCT_US,ICD9CM&ttys=PT'path Retrieve SNOMEDCT_US and ICD9CM preferred terms in C0155502 and returns a JSON Object classType of Atom
#' '/content/current/source/SNOMEDCT_US/111541001/atoms'path Retrieve atoms belonging to SNOMED CT concept 111541001 and returns a JSON Object classType of Atom
#' '/content/current/source/SNOMEDCT_US/111541001/atoms/preferred'path Retrieve the default preferred atom belonging to SNOMED CT concept 111541001 and returns a JSON Object classType of Atom
#' '/content/current/AUI/A8345234'path Retrieve information about AUI A8345234 and returns a JSON Object classType of Atom
#' '/content/current/AUI/A8345234/ancestors'path Retrieve ancestors of a UMLS atom and returns a JSON Object classType of Atom
#' '/content/current/AUI/A8345234/descendants'path Retrieve descendants of a UMLS atom and returns a JSON Object classType of Atom
#' '/content/current/AUI/A8345234/parents'path Retrieve parents of a UMLS atom and returns a JSON Object classType of Atom
#' '/content/current/AUI/A8345234/children'path Retrieve children of a UMLS atom and returns a JSON Object classType of Atom
#' @seealso
#'  \code{\link[httr]{GET}},\code{\link[httr]{content}}
#' @rdname list_atoms
#' @export
#' @importFrom httr GET content

list_atoms <-
        function(CUI,
                sabs = NULL,
                ttys = NULL,
                language = NULL,
                includeObsolete = NULL,
                includeSuppressible = NULL,
                pageNumber = NULL,
                pageSize = NULL) {

                # sabs = NULL
                # ttys = NULL
                # language = NULL
                # includeObsolete = NULL
                # includeSuppressible = NULL
                # pageNumber = NULL
                # pageSize = NULL


                baseURL <- paste0("https://uts-ws.nlm.nih.gov/rest/content/current/CUI/",CUI, "/atoms")

                link_response <- httr::GET(url = baseURL,
                                           query = list(ticket = get_service_ticket(),
                                                                sabs = sabs,
                                                                ttys = ttys,
                                                                language = language,
                                                                includeObsolete = includeObsolete,
                                                                includeSuppressible = includeSuppressible,
                                                                pageNumber = pageNumber,
                                                                pageSize = pageSize)
                )

                link_response %>%
                        httr::content()
        }



#' @title
#' List Atoms
#'
#' @param sabs			 (optional)  Comma-separated list of source vocabularies to include in your search
#' @param ttys			 (optional)  One or more term types
#' @param language	         (optional)  Retrieve only atoms that have a specific language
#' @param includeObsolete	 (optional)  Include content that is obsolete according to the content provider or NLM.
#' @param includeSuppressible	 (optional)  Include content that is suppressible according to NLM Editors.
#' @param pageNumber	         (optional)  Whole number that specifies which page of results to fetch.
#' @param pageSize	         (optional)  Whole number that specifies the number of results to include per page.
#'
#' @details
#' '/content/current/CUI/C0155502/atoms'path Retrieves all atoms for C0155502 and returns a JSON Object classType of Atom
#' '/content/current/CUI/C0155502/atoms/preferred'path Retrieves the default preferred atom of C0155502 and returns a JSON Object classType of Atom
#' '/content/current/CUI/C0155502/atoms?language=ENG'path Retrieves all English language atoms for C0155502 and returns a JSON Object classType of Atom
#' '/content/current/CUI/C0155502/atoms?sabs=SNOMEDCT_US,ICD9CM&ttys=PT'path Retrieve SNOMEDCT_US and ICD9CM preferred terms in C0155502 and returns a JSON Object classType of Atom
#' '/content/current/source/SNOMEDCT_US/111541001/atoms'path Retrieve atoms belonging to SNOMED CT concept 111541001 and returns a JSON Object classType of Atom
#' '/content/current/source/SNOMEDCT_US/111541001/atoms/preferred'path Retrieve the default preferred atom belonging to SNOMED CT concept 111541001 and returns a JSON Object classType of Atom
#' '/content/current/AUI/A8345234'path Retrieve information about AUI A8345234 and returns a JSON Object classType of Atom
#' '/content/current/AUI/A8345234/ancestors'path Retrieve ancestors of a UMLS atom and returns a JSON Object classType of Atom
#' '/content/current/AUI/A8345234/descendants'path Retrieve descendants of a UMLS atom and returns a JSON Object classType of Atom
#' '/content/current/AUI/A8345234/parents'path Retrieve parents of a UMLS atom and returns a JSON Object classType of Atom
#' '/content/current/AUI/A8345234/children'path Retrieve children of a UMLS atom and returns a JSON Object classType of Atom
#' @seealso
#'  \code{\link[httr]{GET}},\code{\link[httr]{content}}
#' @rdname lookup_atoms
#' @export
#' @importFrom httr GET content

lookup_atoms <-
        function(AUI,
                 sabs = NULL,
                 ttys = NULL,
                 language = NULL,
                 includeObsolete = NULL,
                 includeSuppressible = NULL,
                 pageNumber = NULL,
                 pageSize = NULL) {

                # sabs = NULL
                # ttys = NULL
                # language = NULL
                # includeObsolete = NULL
                # includeSuppressible = NULL
                # pageNumber = NULL
                # pageSize = NULL


                baseURL <- paste0("https://uts-ws.nlm.nih.gov/rest/content/current/AUI/",AUI)

                link_response <- httr::GET(url = baseURL,
                                           query = list(ticket = get_service_ticket(),
                                                        sabs = sabs,
                                                        ttys = ttys,
                                                        language = language,
                                                        includeObsolete = includeObsolete,
                                                        includeSuppressible = includeSuppressible,
                                                        pageNumber = pageNumber,
                                                        pageSize = pageSize)
                )

                link_response %>%
                        httr::content()
        }




#' @title
#' Lookup Definitions
#'
#' @param sabs			 (optional)  Comma-separated list of source vocabularies to include in your search
#' @param pageNumber		 (optional)  Whole number that specifies which page of results to fetch.
#' @param pageSize		 (optional)  Whole number that specifies the number of results to include per page.
#' @details
#' '/content/current/CUI/C0155502/definitions'path Retrieves definitions of the CUI and returns a JSON Object classType of Definition
#' @seealso
#'  \code{\link[httr]{GET}},\code{\link[httr]{content}}
#' @rdname lookup_definitions
#' @export
#' @importFrom httr GET content

lookup_definitions <-
        function(
                sabs = NULL,
                pageNumber = NULL,
                pageSize = NULL) {


                baseURL <- paste0("https://uts-ws.nlm.nih.gov/rest/content/current/CUI/",CUI, "/definitions")

                link_response <- httr::GET(url = baseURL,
                                                   query = list(ticket = get_service_ticket(),
                                                                sabs = sabs,
                                                                pageNumber = pageNumber,
                                                                pageSize = pageSize))

                link_response %>%
                        httr::content()
        }

#' @title
#' Lookup Relations
#'
#' @param ticket			 A single-use service ticket is required for each call to the API. See authentication for more information
#' @param pageNumber			 (optional)  Whole number that specifies which page of results to fetch.
#' @param pageSize			 (optional)  Whole number that specifies the number of results to include per page.
#' @details
#' '/content/current/CUI/C0009044/relations' path Retrieves NLM-asserted relationships of the CUI and returns a JSON Object classType of ConceptRelation
#' @seealso
#'  \code{\link[httr]{GET}},\code{\link[httr]{content}}
#' @rdname lookup_relations
#' @export
#' @importFrom httr GET content

lookup_relations <-
        function(
                pageNumber = NULL,
                pageSize = NULL) {

                baseURL <- paste0("https://uts-ws.nlm.nih.gov/rest/content/current/CUI/",CUI, "/relations")

                link_response <- httr::GET(url = baseURL,
                                                   query = list(ticket = get_service_ticket(),
                                                                pageNumber = pageNumber,
                                                                pageSize = pageSize)
                )
                link_response %>%
                        httr::content()
        }


#' @title
#' Lookup Source Asserted Identifiers

#' @details
#' '/content/current/source/SNOMEDCT_US/9468002'path Retrieves Source Concept and returns a JSON Object classType of SourceAtomCluster
#' '/content/current/source/MSH/D015242'path Retrieves Source Descriptor and returns a JSON Object classType of SourceAtomCluster
#' '/content/current/source/LNC/54112-8'path Retrieves Code and returns a JSON Object classType of SourceAtomCluster
#' '/content/current/source/SNOMEDCT_US/9468002/atoms'path Retrieve atoms in a source-asserted identifier and returns a JSON Object classType of Atom
#' '/content/current/source/SNOMEDCT_US/9468002/parents'path Retrieve immediate parents of a source-asserted identifier and returns a JSON Object classType of SourceAtomCluster
#' '/content/current/source/SNOMEDCT_US/9468002/children'path Retrieve immediate children of source-asserted identifier and returns a JSON Object classType of SourceAtomCluster
#' '/content/current/source/SNOMEDCT_US/9468002/ancestors'path Retrieve all ancestors of a source-asserted identifier and returns a JSON Object classType of SourceAtomCluster
#' '/content/current/source/SNOMEDCT_US/9468002/descendants'path Retrieve all descendants of source-asserted identifier and returns a JSON Object classType of SourceAtomCluster
#' '/content/current/source/SNOMEDCT_US/9468002/attributes'path Retrieves information about source-asserted attributes and returns a JSON Object classType of Attribute
#' @seealso
#'  \code{\link[httr]{GET}},\code{\link[httr]{content}}
#' @rdname lookup_source_asserted_identifiers
#' @export
#' @importFrom httr GET content



lookup_source_asserted_identifiers <-
        function(sourceId,
                 sab) {

                baseURL <- paste0("https://uts-ws.nlm.nih.gov/rest/content/current/source/",sab, "/", sourceId)


                link_response <- httr::GET(url = baseURL,
                                                   query = list(ticket = get_service_ticket()))

                link_response %>%
                        httr::content()
        }


#' @title
#' Lookup Source Parents
#'
#' @param pageNumber		 (optional)  Whole number that specifies which page of results to fetch.
#' @param pageSize		 (optional)  Whole number that specifies the number of results to include per page.
#'
#' @details
#' '/content/current/source/SNOMEDCT_US/9468002/parents'path Retrieves parents a source-asserted identifier and returns a JSON Object classType of SourceAtomCluster*
#' '/content/current/source/SNOMEDCT_US/9468002/children'path Retrieves children of a source-asserted identifier and returns a JSON Object classType of SourceAtomCluster*
#' @seealso
#'  \code{\link[httr]{GET}},\code{\link[httr]{content}}
#' @rdname lookup_source_parents
#' @export
#' @importFrom httr GET content

lookup_source_parents <-
        function(sourceId,
                 sab,
                pageNumber = NULL,
                pageSize = NULL) {


                baseURL <- paste0("https://uts-ws.nlm.nih.gov/rest/content/current/source/",sab, "/", sourceId, "/parents")
                link_response <- httr::GET(url = baseURL,
                                                   query = list(ticket = get_service_ticket(),
                                                                pageNumber = pageNumber,
                                                                pageSize = pageSize)
                )

                link_response %>%
                        httr::content()
        }


#' @title
#' Lookup Source Children
#'
#' @param pageNumber		 (optional)  Whole number that specifies which page of results to fetch.
#' @param pageSize		 (optional)  Whole number that specifies the number of results to include per page.
#'
#' @details
#' '/content/current/source/SNOMEDCT_US/9468002/parents'path Retrieves parents a source-asserted identifier and returns a JSON Object classType of SourceAtomCluster*
#' '/content/current/source/SNOMEDCT_US/9468002/children'path Retrieves children of a source-asserted identifier and returns a JSON Object classType of SourceAtomCluster*
#' @seealso
#'  \code{\link[httr]{GET}},\code{\link[httr]{content}}
#' @rdname lookup_source_children
#' @export
#' @importFrom httr GET content

lookup_source_children <-
        function(sourceId,
                 sab,
                 pageNumber = NULL,
                 pageSize = NULL) {


                baseURL <- paste0("https://uts-ws.nlm.nih.gov/rest/content/current/source/",sab, "/", sourceId, "/children")

                link_response <- httr::GET(url = baseURL,
                                                   query = list(ticket = get_service_ticket(),
                                                                pageNumber = pageNumber,
                                                                pageSize = pageSize)
                )

                link_response %>%
                        httr::content()
        }



#' @title
#' Lookup Source Ancestors
#'
#' @param pageNumber			(optional)  Whole number that specifies which page of results to fetch.
#' @param pageSize			(optional)  Whole number that specifies the number of results to include per page.
#' @details
#' '/content/current/source/SNOMEDCT_US/9468002/ancestors'path Retrieves ancestors of a source-asserted identifier and returns a JSON Object classType of SourceAtomCluster*
#' '/content/current/source/SNOMEDCT_US/9468002/descendants'path Retrieves descendants of a source-asserted identifier and returns a JSON Object classType of SourceAtomCluster*
#' @seealso
#'  \code{\link[httr]{GET}},\code{\link[httr]{content}}
#' @rdname lookup_source_ancestors
#' @export
#' @importFrom httr GET content

lookup_source_ancestors <-
        function(pageNumber = NULL,
                pageSize = NULL) {

                baseURL <- paste0("https://uts-ws.nlm.nih.gov/rest/content/current/source/", vocabulary, "/", sourceId,"/ancestors")


                link_response <- httr::GET(url = baseURL,
                                                   query = list(ticket = get_service_ticket(),
                                                                pageNumber = pageNumber,
                                                                pageSize = pageSize)
                )

                link_response %>%
                        httr::content()
        }


#' @title
#' Lookup Source Descendants
#'
#' @param pageNumber			(optional)  Whole number that specifies which page of results to fetch.
#' @param pageSize			(optional)  Whole number that specifies the number of results to include per page.
#' @details
#' '/content/current/source/SNOMEDCT_US/9468002/ancestors'path Retrieves ancestors of a source-asserted identifier and returns a JSON Object classType of SourceAtomCluster*
#' '/content/current/source/SNOMEDCT_US/9468002/descendants'path Retrieves descendants of a source-asserted identifier and returns a JSON Object classType of SourceAtomCluster*
#' @seealso
#'  \code{\link[httr]{GET}},\code{\link[httr]{content}}
#' @rdname lookup_source_descendants
#' @export
#' @importFrom httr GET content


lookup_source_descendants <-
        function(pageNumber = NULL,
                 pageSize = NULL) {

                baseURL <- paste0("https://uts-ws.nlm.nih.gov/rest/content/current/source/", vocabulary, "/", sourceId,"/descendants")


                link_response <- httr::GET(url = baseURL,
                                           query = list(ticket = get_service_ticket(),
                                                        pageNumber = pageNumber,
                                                        pageSize = pageSize)
                )

                link_response %>%
                        httr::content()
        }



#' @title
#' Lookup Source Relations
#'
#' @param includeRelationLabels		  (optional)  One or more relation labels
#' @param includeAdditionalRelationLabels (optional)  One or more relation attribute
#' @param includeObsolete		  (optional)  Include content that is obsolete according to the content provider or NLM.
#' @param includeSuppressible		  (optional)  Include content that is suppressible according to NLM Editors.
#' @param pageNumber			  (optional)  Whole number that specifies which page of results to fetch.
#' @param pageSize			  (optional)  Whole number that specifies the number of results to include per page.
#'
#' @details
#' '/content/current/source/LNC/44255-8/relations'path Retrieves relationships of LOINC code 44255-8 and returns a JSON Object classType of AtomClusterRelation
#' @seealso
#'  \code{\link[httr]{GET}},\code{\link[httr]{content}}
#' @rdname lookup_source_relations
#' @export
#' @importFrom httr GET content


lookup_source_relations <-
        function(
                sourceId,
                vocabulary,
                includeRelationLabels = NULL,
                includeAdditionalRelationLabels = NULL,
                includeObsolete = NULL,
                includeSuppressible = NULL,
                pageNumber = NULL,
                pageSize = NULL) {

                baseURL <- paste0("https://uts-ws.nlm.nih.gov/rest/content/current/source/", vocabulary, "/", sourceId,"/relations")

                link_response <- httr::GET(url = baseURL,
                                                   query = list(ticket = get_service_ticket(),
                                                                includeRelationLabels = includeRelationLabels,
                                                                includeAdditionalRelationLabels = includeAdditionalRelationLabels,
                                                                includeObsolete = includeObsolete,
                                                                includeSuppressible = includeSuppressible,
                                                                pageNumber = pageNumber,
                                                                pageSize = pageSize)
                )

                link_response %>%
                        httr::content()
        }


#' @title
#' Lookup Source Subsets
#'
#' @param pageNumber			(optional)  Whole number that specifies which page of results to fetch.
#' @param pageSize			(optional)  Whole number that specifies the number of results to include per page.
#' @param language			(optional)  3-letter abbreviation for language
#' @details
#' '/subsets/current'path Retrieves information about all subsets from the current release and returns a JSON Object classType of Subset
#' '/subsets/current/source/SNOMEDCT_US/6011000124106'path Retrieves information for a SNOMED CT subset and returns a JSON Object classType of Subset
#' '/subsets/current/source/SNOMEDCT_US/6011000124106/members'path Retrieves members of a SNOMED CT subset and returns a JSON Object classType of SourceConceptSubsetMember
#' '/subsets/current/source/SNOMEDCT_US/6011000124106/member/89361000119103' path Retrieves an individual member of a SNOMED CT subset and returns a JSON Object classType of SourceConceptSubsetMember
#' @seealso
#'  \code{\link[httr]{GET}},\code{\link[httr]{content}}
#' @rdname list_subsets
#' @export
#' @importFrom httr GET content

list_subsets <-
        function(
                pageNumber = NULL,
                pageSize = NULL,
                language = "ENG") {

                baseURL <- paste0("https://uts-ws.nlm.nih.gov/rest/subsets/current")


                link_response <- httr::GET(url = baseURL,
                                                   query = list(ticket = get_service_ticket(),
                                                                pageNumber = pageNumber,
                                                                pageSize = pageSize,
                                                                language = language)
                )

                link_response %>%
                        httr::content()

        }


#' @title
#' List Source Subsets
#'
#' @param pageNumber			(optional)  Whole number that specifies which page of results to fetch.
#' @param pageSize			(optional)  Whole number that specifies the number of results to include per page.
#' @param language			(optional)  3-letter abbreviation for language
#' @details
#' '/subsets/current'path Retrieves information about all subsets from the current release and returns a JSON Object classType of Subset
#' '/subsets/current/source/SNOMEDCT_US/6011000124106'path Retrieves information for a SNOMED CT subset and returns a JSON Object classType of Subset
#' '/subsets/current/source/SNOMEDCT_US/6011000124106/members'path Retrieves members of a SNOMED CT subset and returns a JSON Object classType of SourceConceptSubsetMember
#' '/subsets/current/source/SNOMEDCT_US/6011000124106/member/89361000119103' path Retrieves an individual member of a SNOMED CT subset and returns a JSON Object classType of SourceConceptSubsetMember
#' @seealso
#'  \code{\link[httr]{GET}},\code{\link[httr]{content}}
#' @rdname list_source_subsets
#' @export
#' @importFrom httr GET content

list_source_subsets <-
        function(
                vocabulary,
                pageNumber = NULL,
                pageSize = NULL,
                language = "ENG") {

                baseURL <- paste0("https://uts-ws.nlm.nih.gov/rest/subsets/current/source/", vocabulary)


                link_response <- httr::GET(url = baseURL,
                                           query = list(ticket = get_service_ticket(),
                                                        pageNumber = pageNumber,
                                                        pageSize = pageSize,
                                                        language = language)
                )

                link_response %>%
                        httr::content()

        }

#' @title
#' Lookup Source Attributes
#'
#' @param pageNumber			 (optional)  Whole number that specifies which page of results to fetch.
#' @param pageSize			 (optional)  Whole number that specifies the number of results to include per page.
#' @param includeAttributeNames		 (optional)  One or more attribute names
#' @details
#' '/content/current/source/SNOMEDCT_US/9468002/attributes'path Retrieves attributes of the SNOMED CT concept and returns a JSON Object classType of Attribute
#' @seealso
#'  \code{\link[httr]{GET}},\code{\link[httr]{content}}
#' @rdname lookup_source_attributes
#' @export
#' @importFrom httr GET content

lookup_source_attributes <-
        function(sourceId,
                 vocabulary,
                pageNumber = NULL,
                pageSize = NULL,
                includeAttributeNames = NULL) {

                baseURL <- paste0("https://uts-ws.nlm.nih.gov/rest/content/current/source/", vocabulary, "/", sourceId, "/attributes")


                link_response <- httr::GET(url = baseURL,
                                                   query = list(ticket = get_service_ticket(),
                                                                pageNumber = pageNumber,
                                                                pageSize = pageSize,
                                                                includeAttributeNames = includeAttributeNames)
                )

                link_response %>%
                        httr::content()


        }


#' @title
#' Semantic Network
#' @details
#' /semantic-network/current/TUI/T109' path Retrieves TUI and returns a JSON Object classType of SemanticType
#' @seealso
#'  \code{\link[httr]{GET}},\code{\link[httr]{content}}
#' @rdname lookup_semantic_network
#' @export
#' @importFrom httr GET content

lookup_semantic_network <-
        function() {


                baseURL <- "https://uts-ws.nlm.nih.gov/rest/semantic-network/current/TUI/T109"

                link_response <- httr::GET(url = baseURL,
                                                   query = list(ticket = get_service_ticket())
                )


                link_response %>%
                        httr::content()
        }


#' @title
#' List Content Views
#'
#' @param pageNumber    (optional)  Whole number that specifies which page of results to fetch.
#' @param pageSize      (optional)  Whole number that specifies the number of results to include per page.
#' @details
#' '/content-views/current' path retrieves information about all subsets from the current release and returns a JSON Object classType of ContentView
#' '/content-views/current/CUI/C2711988' path Retrieves information for the SNOMED CT CORE Problem List content view and returns a JSON Object classType of ContentView
#' '/content-views/current/CUI/C2711988/members' path Retrieves members of the SNOMED CT CORE Problem List content view and returns a JSON Object classType of SourceConceptContentViewMember
#' '/content-views/current/CUI/C2711988/member/238788004' path Retrieves an individual member of the SNOMED CT CORE Problem List content view and returns a JSON Object classType of SourceConceptContentViewMember
#' @seealso
#'  \code{\link[httr]{GET}},\code{\link[httr]{content}}
#' @rdname list_content_views
#' @export
#' @importFrom httr GET content

list_content_views <-
        function(
                pageNumber = NULL,
                pageSize = NULL) {

                baseURL <- "https://uts-ws.nlm.nih.gov/rest/content-views/current"


                link_response <- httr::GET(url = baseURL,
                                                   query = list(ticket = get_service_ticket(),
                                                                pageNumber = pageNumber,
                                                                pageSize = pageSize)
                )

                link_response %>%
                        httr::content()
        }


#' @title
#' Lookup Content Views
#'
#' @param pageNumber    (optional)  Whole number that specifies which page of results to fetch.
#' @param pageSize      (optional)  Whole number that specifies the number of results to include per page.
#' @details
#' '/content-views/current' path retrieves information about all subsets from the current release and returns a JSON Object classType of ContentView
#' '/content-views/current/CUI/C2711988' path Retrieves information for the SNOMED CT CORE Problem List content view and returns a JSON Object classType of ContentView
#' '/content-views/current/CUI/C2711988/members' path Retrieves members of the SNOMED CT CORE Problem List content view and returns a JSON Object classType of SourceConceptContentViewMember
#' '/content-views/current/CUI/C2711988/member/238788004' path Retrieves an individual member of the SNOMED CT CORE Problem List content view and returns a JSON Object classType of SourceConceptContentViewMember
#' @seealso
#'  \code{\link[httr]{GET}},\code{\link[httr]{content}}
#' @rdname lookup_content_view_members
#' @export
#' @importFrom httr GET content

lookup_content_view_members <-
        function(CUI,
                 pageNumber = NULL,
                 pageSize = NULL) {


                baseURL <- paste0("https://uts-ws.nlm.nih.gov/rest/content-views/current/CUI/", CUI, "/members")

                link_response <- httr::GET(url = baseURL,
                                           query = list(ticket = get_service_ticket(),
                                                        pageNumber = pageNumber,
                                                        pageSize = pageSize)
                )

                link_response %>%
                        httr::content()
        }


#' @title
#' Lookup a Crosswalk
#'
#' @param targetSource			 (optional)  Returns codes from the specified UMLS vocabulary
#' @param includeObsolete		 (optional)  Determines whether to return obsolete codes.
#' @param pageNumber			 (optional)  Whole number that specifies which page of results to fetch.
#' @param pageSize			 (optional)  Whole number that specifies the number of results to include per page.
#' @details
#' '/crosswalk/current/source/HPO/HP:0001947'path Retrieves all codes that share a UMLS CUI with HP:0001947 and returns a JSON Object classType of SourceAtomCluster
#' '/crosswalk/current/source/HPO/HP:0001947?targetSource=SNOMEDCT_US'path Retrieves all SNOMEDCT_US codes that share a UMLS CUI with HP:0001947 and returns a JSON Object classType of SourceAtomCluster
#' @seealso
#'  \code{\link[httr]{GET}},\code{\link[httr]{content}}
#' @rdname lookup_crosswalk
#' @export
#' @importFrom httr GET content


lookup_crosswalk <-
        function(targetSource = NULL,
                includeObsolete = NULL,
                pageNumber = NULL,
                pageSize = NULL) {


                # targetSource = NULL
                # includeObsolete = NULL
                # pageNumber = NULL
                # pageSize = NULL

                baseURL <- "https://uts-ws.nlm.nih.gov/rest/crosswalk/current/source/HPO/HP:0001947"

                link_response <- httr::GET(url = baseURL,
                                           query = list(ticket = get_service_ticket(),
                                                        targetSource = targetSource,
                                                        includeObsolete = includeObsolete,
                                                        pageNumber = pageNumber,
                                                        pageSize = pageSize)
                )

                link_response %>%
                        httr::content()
        }
