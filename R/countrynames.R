#' Country dictionary
#'
#' Table of countries, areas, and territories with their official names, M49 and
#' ISO codes, and regional groupings.
#'
#' @details This is the UN's *Standard Country or Area Codes for Statistical
#'   Use*, also known as the M49 standard
#'   (https://unstats.un.org/unsd/methodology/m49), with the following
#'   revisions:
#'   \itemize{
#'     \item "State of Palestine" is replaced with "Occupied Palestinian
#'       Territory" in accordance with the IOM *House Style Manual* (May 2023).
#'     \item "Kosovo (SCR 1244)" is added with ISO codes "XK" and "XKX".
#'       References to Kosovo shall be understood to be in the context of United
#'       Nations Security Council resolution 1244 (1999).
#'     \item "Channel Islands" is added with M49 code 830 and ISO codes "XC" and
#'       "XCX". The Channel Islands comprise the British crown dependencies of
#'       Guernsey and Jersey, which are present in the table. However, some
#'       datasets (such as UN DESA's international migrant stocks) combine
#'       Guernsey and Jersey under the Channel Islands, so the separate entry
#'       accounts for such cases.
#'     \item "Abyei" is added with ISO-3 code "AB9". This follows the
#'       designations of the IDMC, whose data separately identify this disputed
#'       area on the border between Sudan and South Sudan.
#'     \item "Taiwan Province of China, China" is added with M49 code 158 and
#'       ISO-3 code "TWN".
#'     \item "Stateless" is added with ISO-3 code "STL".
#'     \item "Unknown" is added with M49 code 2003 and ISO-3 code "OOO".
#'     \item "Unidentified Sub-Saharan Africa" is added with ISO-3 code "OOS".
#'       This designation appears in Frontex irregular migrants data.
#'     \item Additional groupings are added to indicate membership in the
#'       European Union (`eu`), the Schengen Area (`schengen`), and the Balkans
#'       (`balkans`).
#'   }
"countrynames"
