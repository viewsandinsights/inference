
# Last update: Oct 3, 2021

rm(list = ls())

# =========================================================================
# INITIAL DATA FRAME USING ESS DATA (Citation below) 
#
# library(readstata13) 
# ess <- readstata13::read.dta13(file="ESSW19S.dta",
#                                convert.factors = FALSE)
# save(ess, file="ess.Rdata")
# -------------------------------------------------------------------------
# PLEASE NOTE: 
#
# "ESSW19S.dta" is based on two files from the website
# of europeansocialsurvey.org
# 
# FILE 1: Cumulative Stata file ESS1-8e01.dta (edition 1) 
# for Rounds 1 to 8 using the ESS Cumulative Data Wizard at 
# https://www.europeansocialsurvey.org/downloadwizard/
# Download at November 06, 2020. 
# Country selection: 
# AT, BE, CH, CZ, DE, DK, EE, ES, FI, FR, GB, HU, IE, IT,
# LT, NL, NO, PL, PT, SE, SI, SK.
# Selection criteria: 
# a) continuous availability (as far as possible)
# of data across the rounds and b) parallel availability of 
# data for the latest round 9 (except for DK which is included
# despite missing data for round 9) 

# variable selection:
# all variables except for rotating modules and 
# cross-module replicated questions

# Citation of this data file:
# European Social Survey Cumulative File, ESS 1-8 (2018). 
# Data file edition 1.0. NSD - Norwegian Centre for Research 
# Data, Norway - Data Archive and distributor of ESS data for 
# ESS ERIC. doi:10.21338/NSD-ESS-CUMULATIVE


# FILE 2: ESS9e02.dta (edition 2). Download at July 08, 2020. 
# File is reduced to the same country selection as above.
# Then in both files the set of variables is reduced to 
# the same thematically relevant subset, afterwards 
# the file for round 9 was appended to the cumulative
# file for rounds 1 to 8 using Stata. The resulting file  
# was finally sorted by country and round within country
# and converted to the above R data frame.
# N=302,079 (round 1-8) and 38,136 (round 9), 
# overall 340,215 cases

# Reference: 
# ESS Round 9: European Social Survey Round 9 Data (2018). 
# Data file edition 2.0. NSD - Norwegian Centre for Research 
# Data, Norway – Data Archive and distributor of ESS data for 
# ESS ERIC. doi:10.21338/NSD-ESS9-2018.

# ========================================================================
#
# CODING FAR-RIGHT WING VOTING/CLOSENESS

# Primary coding source used:
# Lewis Davis and Sumit S. Deole (2017). 
# Immigration and the Rise of Far-right Parties in Europe 
# (ifo DICE Report 4/2017, December, Volume 15). 
# Technical Report January 2018.Downloaded from
# https://www.researchgate.net/publication/322732593_Immigration_and_the_Rise_of_Far-right_Parties_in_Europe
# (last access: November 27, 2020)

# Supplementary "BBC-SOURCE" source referred to below: 
# “Europe and right-wing nationalism: A country-by-country guide
# (13 November 2019) https://www.bbc.com/news/world-europe-36130006 
# (last access: November 27, 2020)

# Additional supplementary source occasionally cited below as "APP3": 
# APPENDIX A3 POLITICAL PARTIES, ESS9 - 2018 ed.3.0 (published 10.12.2020) 
# https://www.europeansocialsurvey.org/docs/round9/survey/ESS9_appendix_a3_e03_0.pdf

rm(list = ls())
load("ess.Rdata")

# IN AUSTRIA -----------------------------------------------------------
# FPÖ (Freiheitliche Partei Österreich  # "right-wing populist" acc. to APP3
# und BZÖ (Bündnis Zukunft Österreich) 

ess$rwingvtat <- ifelse( 
                      (ess$cntry == "AT" & ess$vote == 1 & 
                         
                            (ess$prtvtat == 3 | 
                            ess$prtvtbat == 3 | ess$prtvtbat == 4 | 
                            ess$prtvtcat == 3 )), 1, 0)

addmargins(table(ess$rwingvtat)) 


ess$rwingclat <- ifelse( 
  (ess$cntry == "AT" & ess$clsprty == 1 & 
     
        (ess$prtclat == 3 | 
        ess$prtclaat == 3 | ess$prtclaat == 4 | 
        ess$prtclcat == 3 | ess$prtclcat == 4 )), 1, 0)

addmargins(table(ess$rwingclat)) 

# IN BELGIUM -------------------------------------------------------------------------
# Vlaams Blok/Vlaams Belang (VB) , Front National

# BBC-SOURCE: N-VA Nieuw-Vlaamse Alliantie (New Flemish Alliance: codes == 3 and 4) 
# Supposedly not far-right but declaredy nationalistic - excluded from this 
# far-right coding version 'right", not "extreme right" acc. to. "APP3"

ess$rwingvtbe <- ifelse( 
  (ess$cntry == "BE" & ess$vote == 1 & 
     
        (ess$prtvtbe == 8 |  ess$prtvtbe == 15 | 
        ess$prtvtabe == 7 | ess$prtvtabe == 11 | # ess$prtvtabe == 3 |    # 3  =N-VA
        ess$prtvtbbe == 7 | ess$prtvtbbe == 11 |
        ess$prtvtcbe == 7 | ess$prtvtcbe == 11 | # ess$prtvtcbe == 3 |    # 3 = N-VA
        ess$prtvtdbe == 7 | ess$prtvtdbe == 11   # | ess$prtvtdbe == 3    # 3 = N-VA
         )), 1, 0)

addmargins(table(ess$rwingvtbe)) 


ess$rwingclbe <- ifelse( 
  (ess$cntry == "BE" & ess$clsprty == 1 & 
     
        (ess$prtclbe == 8 |  ess$prtclbe == 15 | #    ess$prtclbe == 4 |  # 4 = N-VA
        ess$prtclabe == 8 | ess$prtclabe == 12 | #   ess$prtclabe == 3 |  # 3 = N-VA
        ess$prtclbbe == 8 | ess$prtclbbe == 12 | #   ess$prtclbbe == 3 |  # 3 = N-VA
        ess$prtclcbe == 7 | ess$prtclcbe == 11 | #   ess$prtclcbe == 3 |  # 3 = N-VA
        ess$prtcldbe == 7                        # | ess$prtcldbe == 3    # 3 = N-VA
     )), 1, 0)

addmargins(table(ess$rwingclbe))

# IN SWITZERLAND --------------------------------------------------------------------
# SVP   Swiss People's Party # right-wing populist acc. to APP3
# PNOS  Swiss Nationalist Party 
# SD    Swiss Democrats

ess$rwingvtch <- ifelse( 
  (ess$cntry == "CH" & ess$vote == 1 & 
     
        (ess$prtvtch == 4 | ess$prtvtch ==  11 | 
        ess$prtvtach == 4 | ess$prtvtach == 11 | 
        ess$prtvtbch == 4 | ess$prtvtbch == 10 |
        ess$prtvtcch == 4 | ess$prtvtcch == 10 |
        ess$prtvtdch == 1 | ess$prtvtdch == 15 |
        ess$prtvtech == 1 | 
        ess$prtvtfch == 1 |
        ess$prtvtgch == 1 
     )), 1, 0)

addmargins(table(ess$rwingvtch)) 


ess$rwingclch <- ifelse( 
  (ess$cntry == "CH" & ess$clsprty == 1 & 
     
        (ess$prtclch == 4 | ess$prtclch ==  11 | 
        ess$prtclach == 4 | ess$prtclach == 11 | 
        ess$prtclbch == 4 | ess$prtclbch == 10 |
        ess$prtclcch == 4 | ess$prtclcch == 10 |
        ess$prtcldch == 1 | ess$prtcldch == 15 | ess$prtcldch == 17 |
        ess$prtclech == 1 | 
        ess$prtclfch == 1 |
        ess$prtclgch == 1
           )), 1, 0)

addmargins(table(ess$rwingclch))

# IN GERMANY --------------------------------------------------------
# NPD National Democratic Party of Germany # "right-wing extremist acc. to APP3
# REP Republicans # same as NPD, though not that extremist acc. to APP3 
# BBC-Source: AfD Alternative for Germany = included in this far-right 
# coding version because it is supposed to be at least in part 
# a far-right party (and the primary coding source's 
# own reference for Germany goes back to the year 2006) 
# AfD is described as "right-wing populist party" acc. to APP3

ess$rwingvtde <- ifelse( 
  (ess$cntry == "DE" & ess$vote == 1 & 
     
        (ess$prtvde1 == 6 |  
        ess$prtvade1 == 6 | ess$prtvade1 ==  7 | 
        ess$prtvbde1 == 6 | ess$prtvbde1 ==  7 |
        ess$prtvcde1 == 6 | ess$prtvcde1 ==  7 |
        ess$prtvdde1 == 6 | ess$prtvdde1 ==  7 |
        ess$prtvede1 == 8 |                            ess$prtvede1 ==  6 |  # 6=AFD

         ess$prtvde2 == 6 |  
        ess$prtvade2 == 6 | ess$prtvade2 ==  7 | 
        ess$prtvbde2 == 6 | ess$prtvbde2 ==  7 |
        ess$prtvcde2 == 6 | ess$prtvcde2 ==  7 |
        ess$prtvdde2 == 6 | ess$prtvdde2 ==  7 |
        ess$prtvede2 == 8                            | ess$prtvede2 ==  6    # 6=AFD        
     )), 1, 0)

addmargins(table(ess$rwingvtde))

ess$rwingclde <- ifelse( 
  (ess$cntry == "DE" & ess$clsprty == 1 &
     
     (ess$prtclde == 6 |  
     ess$prtclade == 6 | ess$prtclade ==  7 | 
     ess$prtclbde == 6 | ess$prtclbde ==  7 |
     ess$prtclcde == 6 | ess$prtclcde ==  7 |
     ess$prtcldde == 6 | ess$prtcldde ==  7 |
     ess$prtclede == 8                                | ess$prtclede ==  6   # 6=AFD   
  )), 1, 0)

addmargins(table(ess$rwingclde))

# IN DENMARK ------------------------------------------------------ 
# DF Danish People's Party - Dansk Folkeparti # right-wing acc. to APP3
# FP Danish Progress Party - Fremskridtspartiet 

ess$rwingvtdk <- ifelse( 
  (ess$cntry == "DK" & ess$vote == 1 & 
     
        (ess$prtvtdk == 6 |  ess$prtvtdk == 9 |
        ess$prtvtadk == 6 | ess$prtvtadk == 9 |
        ess$prtvtbdk == 5 |
        ess$prtvtcdk == 5 
     )), 1, 0)

addmargins(table(ess$rwingvtdk)) 

ess$rwingcldk <- ifelse( 
  (ess$cntry == "DK" & ess$clsprty == 1 & 
     
        (ess$prtcldk == 6 | ess$prtcldk == 9 |  
        ess$prtcladk == 6 | ess$prtcladk == 9 | 
        ess$prtclbdk == 5 | 
        ess$prtclcdk == 5 
     )), 1, 0)

addmargins(table(ess$rwingcldk))

# IN FINLAND ----------------------------------------------------- 
# PS Finns Party  (formerly the "True Finns Party") # "rightist populist" acc. to APP3
#       only the var label "true finns" appeared 
#       (codes 4 and 5 below)
# SKS Finnish People's Blue-whites (code 8 below)

ess$rwingvtfi <- ifelse( 
  (ess$cntry == "FI" & ess$vote == 1 & 
     
        (ess$prtvtfi == 5 | 
        ess$prtvtafi == 5 |  
        ess$prtvtbfi == 5 | ess$prtvtbfi == 8 |
        ess$prtvtcfi == 4 | 
        ess$prtvtdfi == 4  
     )), 1, 0)

addmargins(table(ess$rwingvtfi)) 


ess$rwingclfi <- ifelse( 
  (ess$cntry == "FI" & ess$clsprty == 1 &

        (ess$prtclfi == 5 | 
        ess$prtclafi == 5 |  
        ess$prtclbfi == 5 | 
        ess$prtclcfi == 4 | 
        ess$prtcldfi == 4 |
        ess$prtclefi == 4 
     )), 1, 0)

addmargins(table(ess$rwingclfi)) 

# IN FRANCE ------------------------------------------------------
# FN Front Natioal / RN Rassemblement National (National Rally) # "extreme right" acc. to APP3
# MNR National Republican Movement
# MPF Movement for France

ess$rwingvtfr <- ifelse( 
  (ess$cntry == "FR" & ess$vote == 1 & 
     
        (ess$prtvtfr == 3 |  ess$prtvtfr == 7 |  ess$prtvtfr == 8 | 
        ess$prtvtafr == 3 | ess$prtvtafr == 7 | ess$prtvtafr == 8 | 
        ess$prtvtbfr == 2 | ess$prtvtbfr == 5 |
        ess$prtvtcfr == 2 | ess$prtvtcfr == 8 | 
        ess$prtvtdfr == 11  
     )), 1, 0)

addmargins(table(ess$rwingvtfr)) 

# FN, MNR, MPF

ess$rwingclfr <- ifelse( 
  (ess$cntry == "FR" & ess$clsprty == 1 & 
     
        (ess$prtclfr == 3 |  ess$prtclfr == 7 |  ess$prtclfr == 8 | 
        ess$prtclafr == 2 | ess$prtclafr == 6 | ess$prtclafr == 7 | 
        ess$prtclbfr == 2 | ess$prtclbfr == 6 |
        ess$prtclcfr == 1 |  
        ess$prtcldfr == 2 | ess$prtcldfr == 9 |    
        ess$prtclefr == 2 | ess$prtclefr == 9 |
        ess$prtclffr == 11
           )), 1, 0)

addmargins(table(ess$rwingclfr))

# IN UNITED KINGDOM (GB) -----------------------------------------
# UKIP United Kingdom Independence Party # "right-wing" acc. to APP3
# BNP British National Party

ess$rwingvtgb <- ifelse( 
  (ess$cntry == "GB" & ess$vote == 1 & 
     
       (ess$prtvtagb == 7 | ess$prtvtagb == 8 |
        ess$prtvtbgb == 7 |
        ess$prtvtcgb == 7 
     )), 1, 0)

addmargins(table(ess$rwingvtgb)) 

ess$rwingclgb <- ifelse( 
  (ess$cntry == "GB" & ess$clsprty == 1 & 
     
       (ess$prtclagb == 7 | ess$prtclagb == 8 |
        ess$prtclbgb == 7 |
        ess$prtclcgb == 7 
     )), 1, 0)

addmargins(table(ess$rwingclgb)) 

# IN HUNGARY (HU) ------------------------------------------------
# MIEP Hungarian Justice and Life Party
# JOBBIK Movement for a better Hungary # "extreme right-wing" acc. to APP 3
#
# BBC SOURCE names - besides Jobbik - also  Fidesz: excluded  from 
# this far-right voting  
#  (it appears as a very strongly frequented center/right party,   
#   perhaps including far-right elements/streams ) 
# Fidesz is describes as being considered as "national conservative" and
# recently as "far-right" acc. to APP 3

ess$rwingvthu <- ifelse( 
  (ess$cntry == "HU" & ess$vote == 1 & 
     
       (ess$prtvtahu ==  3 |                           #   ess$prtvtahu == 1 | # 1 = Fidesz
        ess$prtvtbhu ==  3 |                           #   ess$prtvtbhu == 1 | # 1 = Fidesz
        ess$prtvtchu == 11 |                           #   ess$prtvtchu == 1 | # 1 = Fidesz
        ess$prtvtdhu ==  4 | ess$prtvtdhu == 7 |       #   ess$prtvtdhu == 3 | # 3 = Fidesz
        ess$prtvtehu ==  2 |                           #   ess$prtvtehu == 1 | # 1 = Fidesz  
        ess$prtvtfhu ==  4                             # | ess$prtvtfhu == 3   # 3 = Fidesz   
     )), 1, 0)

addmargins(table(ess$rwingvthu)) 

# MIEP, JOBBIK, (# Fidesz)

ess$rwingclhu <- ifelse( 
   (ess$cntry == "HU" & ess$clsprty == 1 & 

         (ess$prtclahu == 5 | ess$prtclahu ==  9 |      #   ess$prtclahu == 3 | # 3 = Fidesz
         ess$prtclbhu ==  3 | ess$prtclbhu == 11 |      #   ess$prtclbhu == 1 | # 1 = Fidesz
         ess$prtclchu == 11 |                           #   ess$prtclchu == 1 | # 1 = Fidesz
         ess$prtcldhu ==  7 | ess$prtcldhu == 12 |      #   ess$prtcldhu == 6 | # 6 = Fidesz
         ess$prtclehu ==  4 |                           #   ess$prtclehu == 3 | # 3 = Fidesz  
         ess$prtclfhu ==  4 |                           #   ess$prtclfhu == 3 | # 3 = Fidesz   
         ess$prtclghu ==  4                             # | ess$prtclghu == 3   # 3 = Fidesz 
     )), 1, 0)

addmargins(table(ess$rwingclhu))

# IN ITALY -------------------------------------------------------
# MS-FT Social Movement - Tricolour Flame (Fiamma Tricolore)
# LN Lega Nord # "right-wing regionalist party" acc. to APP 3

# Fratelli d'Italia "right-wing" (nationalism, conservatism,
# euroskepticism) acc. to APP3

# Casapound Italia "neo-fascist" (ultranationalism, hard euroskepticims,
# anti-immigration, ..)

ess$rwingvtit <- ifelse( 
  (ess$cntry == "IT" & ess$vote == 1 & 
     
       (ess$prtvtit == 11 |  ess$prtvtit == 16 |
        ess$prtvtbit == 9 | ess$prtvtbit == 10 |   
        ess$prtvtcit == 9 | ess$prtvtcit == 10 | ess$prtvtcit == 13 
     )), 1, 0)

addmargins(table(ess$rwingvtit)) 

ess$rwingclit <- ifelse( 
  (ess$cntry == "IT" & ess$clsprty == 1 & 

       (ess$prtclit == 11 |   ess$prtclit == 16 | 
        ess$prtclbit == 9 |  ess$prtclbit == 10 | 
        ess$prtclcit == 3 |  ess$prtclcit ==  5 |
        ess$prtcldit == 9 |  ess$prtcldit == 10 | ess$prtcldit == 13  
     )), 1, 0)

addmargins(table(ess$rwingclit))

# IN THE NETHERLANDS --------------------------------------------
# LPF Pim Fortuyn List
# PVV Party for Freedom (List Wilders)

ess$rwingvtnl <- ifelse( 
  (ess$cntry == "NL" & ess$vote == 1 & 
     
        (ess$prtvtnl == 4 |  
        ess$prtvtanl == 4 | 
        ess$prtvtbnl == 4 |
        ess$prtvtcnl == 4 | ess$prtvtcnl == 11 |
        ess$prtvtdnl == 3 | 
        ess$prtvtenl == 3 |  
        ess$prtvtfnl == 3 | 
        ess$prtvtgnl == 3   
     )), 1, 0)

addmargins(table(ess$rwingvtnl)) 

ess$rwingclnl <- ifelse( 
  (ess$cntry == "NL" & ess$clsprty == 1 & 
     
        (ess$prtclnl == 4 |  
        ess$prtclanl == 4 | ess$prtclanl == 12 | 
        ess$prtclbnl == 4 | ess$prtclbnl == 11 | 
        ess$prtclcnl == 3 | 
        ess$prtcldnl == 3 |  
        ess$prtclenl == 3 |   
        ess$prtclfnl == 3  
     )), 1, 0)

addmargins(table(ess$rwingclnl))

# IN NORWAY NO -----------------------------------------------------
# FRP Progress Party # "right-populist" acc. to APP 3

ess$rwingvtno <- ifelse( 
  (ess$cntry == "NO" & ess$vote == 1 & 
     
        (ess$prtvtno == 8 |  
        ess$prtvtano == 8 | 
        ess$prtvtbno == 8 
     )), 1, 0)

addmargins(table(ess$rwingvtno)) 

ess$rwingclno <- ifelse( 
  (ess$cntry == "NO" & ess$clsprty == 1 & 
     
     (ess$prtclno == 8 |  
        ess$prtclano == 8 | 
        ess$prtclbno == 8  
           )), 1, 0)

addmargins(table(ess$rwingclno))

# IN PORTUGAL PT -------------------------------------------------
# PNR National Renovator Party

ess$rwingvtpt <- ifelse( 
  (ess$cntry == "PT" & ess$vote == 1 & 

        (ess$prtvtapt ==  8 | 
         ess$prtvtbpt ==  8 | 
         ess$prtvtcpt == 11
     )), 1, 0)

addmargins(table(ess$rwingvtpt)) 

ess$rwingclpt <- ifelse( 
  (ess$cntry == "PT" & ess$clsprty == 1 & 

          (ess$prtclcpt ==   8 | 
           ess$prtclept ==  11  
     )), 1, 0)

addmargins(table(ess$rwingclpt)) 

# IN SWEDEN SE -------------------------------------------------- 
# SD Swedish Democrats (Sverigedemokraterna) # "nationalist right wing" acc. to APP 3

ess$rwingvtse <- ifelse( 
  (ess$cntry == "SE" & ess$vote == 1 & 
     
       (ess$prtvtase == 10 | 
        ess$prtvtbse == 10 | 
        ess$prtvtcse ==  9
     )), 1, 0)

addmargins(table(ess$rwingvtse)) 

ess$rwingclse <- ifelse( 
  (ess$cntry == "SE" & ess$clsprty == 1 & 
     
       (ess$prtclase == 10 | 
        ess$prtclbse == 10 | 
        ess$prtclcse ==  9
     )), 1, 0)

addmargins(table(ess$rwingclse)) 

# =====================================================================
#    FOR THE FOLLOWING COUNTRIES ONLY THE ABOVE-CITED BBC-SOURCE PLUS |
#   SUPPLEMENTARY CODING SOURCES AVAILABLE                |
# =====================================================================

# In CZECHIA CZ -----------------------------------------------------

# BBC-SOURCE: Freedom & Direct Democracy (Svoboda a přímá demokracie, 
# SPD) is described as a hard Euroskeptic, anti-immigration,
# pro-direct political party. Joins the far-right political group
# "Identity and Democracy" of the European Parliament, and hosted
# in 2017 a conference of the Movement for a Europe of Nations and
# Freedom in Prague, with parties such as the French Front National,
# Dutch Party for Freedom, Lega Nord, ..
# (acc. to https://en.wikipedia.org/wiki/Freedom_and_Direct_Democracy )
# "right-wing, anti-immigration, nationalist, anti-EI vision" acc. to APP 3

ess$rwingvtcz <- ifelse( 
  (ess$cntry == "CZ" & ess$vote == 1 & 
     
     (ess$prtvtecz == 8 
     )), 1, 0)

addmargins(table(ess$rwingvtcz)) 

ess$rwingclcz <- ifelse( 
  (ess$cntry == "CZ" & ess$clsprty == 1 & 
     
     (ess$prtclecz == 8 
     )), 1, 0)

addmargins(table(ess$rwingclcz)) 

# IN ESTONIA EE ----------------------------------------------------- 

# BBC-SOURCE: 
# EKRE Eesti Konservatiivne Rahvaerakond = 
# Conservative People's Party of Estonia - 
# is described as a national-conservative and 
# right-wing populist political party - which in the media 
# mainstream would be seen as belonging to the far-right

# Estonian Independence Party (Eesti Iseseisvuspartei EIP)
# "far-right nationalist" party acc. to APP 3


ess$rwingvtee <- ifelse( 
  (ess$cntry == "EE" & ess$vote == 1 & 
     
        (ess$prtvtbee == 9 |
         ess$prtvtcee == 8 | 
         ess$prtvtdee == 4 | ess$prtvtdee == 8 | 
         ess$prtvteee == 6 | ess$prtvteee == 9 | 
         ess$prtvtfee == 6 | ess$prtvtfee == 9 |
         ess$prtvtgee == 6 | ess$prtvtgee == 9 
     )), 1, 0)

addmargins(table(ess$rwingvtee)) 

ess$rwingclee <- ifelse( 
  (ess$cntry == "EE" & ess$clsprty == 1 & 
     
       (ess$prtclbee == 9 |
        ess$prtclcee == 8 |  
        ess$prtcldee == 4 | ess$prtcldee == 8 | 
        ess$prtcleee == 6 | ess$prtcleee == 9 | 
        ess$prtclfee == 6 |
        ess$prtclgee == 6 | ess$prtclgee == 9 
           )), 1, 0)

addmargins(table(ess$rwingclee)) 

# IN SPAIN  ES --------------------------------------------------

# BBC-SOURCE: VOX (described as far-right, national-conservative,
# populist party) # ""far right/radical-right" acc. to APP 3

ess$rwingvtes <- ifelse( 
  (ess$cntry == "ES" & ess$vote == 1 & 
     
     (ess$prtvtees == 16 
     )), 1, 0)

addmargins(table(ess$rwingvtes)) 

ess$rwingcles <- ifelse( 
  (ess$cntry == "ES" & ess$clsprty == 1 & 
     
     (ess$prtclfes == 16 
     )), 1, 0)

addmargins(table(ess$rwingcles))

# 
# IN SLOVENIA SI ------------------------------------------------

# SNS Slovene National Party (Slovenska nacionalna stranka)
# is described as a "nationalist far-right political party in Slovenia
# acc. to: https://en.wikipedia.org/wiki/Slovenian_National_Party
# "far-right" acc. to. APP 3

ess$rwingvtsi <- ifelse( 
  (ess$cntry == "SI" & ess$vote == 1 & 
     
        (ess$prtvtsi ==  4 | 
        ess$prtvtasi ==  6 | 
        ess$prtvtbsi ==  4 | 
        ess$prtvtcsi ==  7 |
        ess$prtvtdsi == 10 | 
        ess$prtvtfsi == 11   
     )), 1, 0)

addmargins(table(ess$rwingvtsi)) 


ess$rwingclsi <- ifelse( 
  (ess$cntry == "SI" & ess$clsprty == 1 & 

        (ess$prtclsi ==  4 | 
        ess$prtclasi ==  6 | 
        ess$prtclbsi ==  4 | 
        ess$prtclcsi ==  7 | 
        ess$prtcldsi == 10 | 
        ess$prtclfsi == 11               
     )), 1, 0)

addmargins(table(ess$rwingclsi)) 

# POLAND PL -------------------------------------------------------------

# BBC-Source: Confederation Liberty and Independence 
# (Polish: Konfederacja Wolność i Niepodległość) 

# frequently shortened to just Confederation (Polish: Konfederacja)
# far-right political party initially established in 2018 between 
# KORWiN and the National Movement party. According to: 
# https://en.wikipedia.org/wiki/Confederation_Liberty_and_Independence

# no explicit occurrence of the party in the survey data, 
# however "KORWIN" and "National Movement party" occurs. 
# KORWiN Coalition for the Renewal of the Republic - Liberty and Hope
# since 2016 known as Liberty (Polish: Wolnosc)
# acc. to https://en.wikipedia.org/wiki/KORWiN_(Poland)
# RN National Movement  (Polish: Ruch Narodowy) 
# described as far-right and right-wing populist political movement
# acc. to https://en.wikipedia.org/wiki/National_Movement_(Poland)
# These two parties are coded.

ess$rwingvtpl <- ifelse( 
  (ess$cntry == "PL" & ess$vote == 1 & 
     
       (ess$prtvtapl == 8 |
        ess$prtvtdpl == 1 
     )), 1, 0)

addmargins(table(ess$rwingvtpl)) 

ess$rwingclpl <- ifelse( 
  (ess$cntry == "PL" & ess$clsprty == 1 & 

      (ess$prtclbpl == 9 |
       ess$prtclfpl == 6 |    
       ess$prtclgpl == 1 
     )), 1, 0)

addmargins(table(ess$rwingclpl)) 

# SLOVAKIA SK --------------------------------------------------------

# BBC-SOURCE: People's Party - Our Slovakia
# (Slovak: Ľudová strana – Naše Slovensko, ĽSNS)

# https://en.wikipedia.org/wiki/People%27s_Party_Our_Slovakia
# far-right neo-Nazi political party 

# 
# prtvtdsk == 5 = LS Nase Slovensko (n=35)
# no explicit occurence in the closeness variables # NOT INCLUDED

# =====================================================================
#    FOR THE FOLLOWING COUNTRIES ONLY 
#   SUPPLEMENTARY CODING SOURCES AVAILABLE                |
# =====================================================================

# IRELAND IE --------------------------------------------------------

# National Party (An Páirtí Náisiúnta) "minor far-right nationalist
# political party" according to: 
# https://en.wikipedia.org/wiki/National_Party_(Ireland,_2016)

# no explicit occurrence in the survey data

# LITHUANIA LT ------------------------------------------------------
# CODING SOURCES: „The extreme right in the Baltic States: Lithuania” 
# (09 June 2020) by Małgorzata Kulbaczewska-Figat 
# https://www.transform-network.net/focus/overview/article/radical-far-and-populist-right/the-extreme-right-in-the-baltic-states-lithuania/
# https://www.wikiwand.com/en/Lithuanian_Nationalist_and_Republican_Union
# 
# LTS Lithuanian Nationalist and Republican Union (LTS or tautinnikai, nationalists)
#                         described as as nationalist right-wing party

# TVS  Lithuanian Unity National Union
# JL Young Lithuania Party     # Described as extremist groups 

ess$rwingvtlt <- ifelse( 
  (ess$cntry == "LT" & ess$vote == 1 & 
     
        (ess$prtvlt1 == 15 |
        ess$prtvalt1 == 15 |
         ess$prtvlt2 == 16 |   
        ess$prtvalt2 == 18 | ess$prtvalt2 == 13 |   
        ess$prtvblt2 ==  4 | ess$prtvblt2 ==  5 |
         ess$prtvlt3 == 16 |
        ess$prtvalt3 == 18 | ess$prtvalt3 == 13 |
        ess$prtvblt3 ==  4 | ess$prtvblt3 ==  5          
     )), 1, 0)

addmargins(table(ess$rwingvtlt)) 

ess$rwingcllt <- ifelse( 
  (ess$cntry == "LT" & ess$clsprty == 1 &

    (ess$prtcllt == 16 |
    ess$prtclalt == 18 | ess$prtclalt == 12 |
    ess$prtclblt == 15          
  )), 1, 0)

addmargins(table(ess$rwingcllt)) 

# ======================================================================
library(car)

ess$rwvt <- ifelse( 
    (ess$rwingvtat == 1) | 
    (ess$rwingvtbe == 1) | 
    (ess$rwingvtch == 1) |
    (ess$rwingvtde == 1) |
    (ess$rwingvtdk == 1) |
    (ess$rwingvtfi == 1) |
    (ess$rwingvtfr == 1) | 
    (ess$rwingvtgb == 1) | 
    (ess$rwingvthu == 1) |
    (ess$rwingvtit == 1) |
    (ess$rwingvtnl == 1) |
    (ess$rwingvtno == 1) |
    (ess$rwingvtpt == 1) |
    (ess$rwingvtse == 1) |
    (ess$rwingvtcz == 1) | 
    (ess$rwingvtee == 1) | 
    (ess$rwingvtes == 1) |
    (ess$rwingvtsi == 1) |
    (ess$rwingvtpl == 1) |
    (ess$rwingvtlt == 1
    ), 1, 0)

ess$rwvtr <- car::recode(ess$rwvt,
                  recodes = '1=1; 0=0; NA=0')

addmargins(table(ess$rwvtr)) # no NAs cat 1 = 11,003 (N=340,215)
addmargins(table(ess$rwvtr, ess$vote)) # Active NAs due to "vote"
addmargins(table(ess$vote)) 

ess$rwingvote <- ifelse( 
  (ess$vote == 1 & ess$rwvtr == 1), 1,
  ifelse(
    (ess$vote == 1 & ess$rwvtr == 0), 2,
    ifelse(
      (ess$vote == 2 & ess$rwvtr == 0), 3,
           ifelse(
             (ess$vote == 3 & ess$rwvtr == 0), 4, NA ))))
             
addmargins(table(ess$rwingvote))
addmargins(table(ess$rwingvote, ess$vote))
addmargins(table(ess$vote))

# ----------------------------------------------------------------

ess$rwcl <- ifelse( 
    (ess$rwingclat == 1) | 
    (ess$rwingclbe == 1) | 
    (ess$rwingclch == 1) |
    (ess$rwingclde == 1) |
    (ess$rwingcldk == 1) |
    (ess$rwingclfi == 1) |
    (ess$rwingclfr == 1) | 
    (ess$rwingclgb == 1) | 
    (ess$rwingclhu == 1) |
    (ess$rwingclit == 1) |
    (ess$rwingclnl == 1) |
    (ess$rwingclno == 1) |
    (ess$rwingclpt == 1) |
    (ess$rwingclse == 1) |
    (ess$rwingclcz == 1) | 
    (ess$rwingclee == 1) | 
    (ess$rwingcles == 1) |
    (ess$rwingclsi == 1) |
    (ess$rwingclpl == 1) |	
    (ess$rwingcllt == 1
    ), 1, 0)

ess$rwclr <- car::recode(ess$rwcl,
                         recodes = '1=1; 0=0; NA=0')

addmargins(table(ess$rwclr)) # no NAs cat 1 = 9,677 (N=340,215)
addmargins(table(ess$rwclr, ess$clsprty)) # Active NAs due to "clsprty"
addmargins(table(ess$clsprty)) 

ess$rwingclose <- ifelse( 
  (ess$clsprty == 1 & ess$rwclr == 1), 1,
  ifelse(
    (ess$clsprty == 1 & ess$rwclr == 0), 2,
    ifelse(
      (ess$clsprty == 2 & ess$rwclr == 0), 3, NA )))

addmargins(table(ess$rwingclose))
addmargins(table(ess$rwingclose, ess$clsprty))
addmargins(table(ess$clsprty))

# ======================================================================

save(ess, file="ess.Rdata") # Data Frame saved using R version 4.0.3

# ======================================================================
rm(list = ls())
load("ess.Rdata")

# The following code excludes from the file all country-specific voting 
# and closeness variables employed above from the data frame and keeps 
# only the two resulting index variables rwingvote and rwingclose
# This reduces the number of variables from k=420 to k=124

# variable names are identical to the original ESS data sources.
# Use the official ESS sources:
# https://www.europeansocialsurvey.org/docs/cumulative/ESS_cumulative_variable_list.pdf
# https://www.europeansocialsurvey.org/data/round-index.html

# Please note: when downloading the cumulative ESS dataset 
# using the Cumulative Data Wizard from
# https://www.europeansocialsurvey.org/downloadwizard/
# as, for instance a CSV or Stata file, the ESS system
# creates automatically a corresponding codebook. 

ess19 <- 
  subset(ess, select = c(
    round, cntry, essround, dweight, pspwght, pweight, region, regunit, 
    domicil, hhmmb, gndr, agea, eduyrs, hinctnta, hincfel, pdwrk, edctn,
    uempla, uempli, dsbld, rtrd, cmsrv, hswrk, dngoth, dngdk, dngref,
    dngna, emplrel, wrkctra, estsz, tporgwk, mbtru, rlgblg, rlgdnm, 
    rlgdgr, rlgatnd, pray, dscrgrp, dscrrce, dscrntn, dscrrlg, dscrlng,
    dscretn, dscrage, dscrgnd, dscrsex, dscrdsb, dscroth, dscrdk, dscrref,
    dscrnap, dscrna, ctzcntr, blgetmg, happy, stflife, sclmeet, inprdsc,
    sclact, crmvct, aesfdrk, health, hlthhmp, stfeco, stfgov, stfdem, 
    stfedu, stfhlth, ppltrst, pplfair, pplhlp, trstprl, trstlgl, trstplc,
    trstplt, trstprt, trstep, trstun, gincdif, freehms, ipcrtiv, imprich, 
    ipeqopt, ipshabt, impsafe, impdiff, ipfrule, ipudrst, ipmodst, ipgdtim,
    impfree, iphlppl, ipsuces, ipstrgv, ipadvnt, ipbhprp, iprspot, iplylfr,
    impenv, imptrad, impfun, euftf, imsmetn, imdfetn, impcntr, imbgeco, 
    imueclt, imwbcnt, polintr, lrscale, psppsgva, actrolga, psppipla, 
    cptppola, prtyban, scnsenv, netuse, netusoft, pstplonl, 
    vote, clsprty, prtdgcl, rwingvote, rwingclose))

# rwingvote
#   1 = voted last election far-right
#   2 = voted last election but not far-right
#   3 = did not vote
#   4 = Not eligible to vote
# rwingclose
#   1 = feel closer to far-right party
#   2 = feel closer but not to far-right
#   3 = feel not closer to a particular political party


# save(ess19, file="ess19.Rdata") 

# ======================================================

rm(list = ls())
load("ess19.Rdata")
#
# ESS sampling weight
ess19$anweight <- ess19$pspwght * ess19$pweight

# index construction: far-right

addmargins(table(ess19$lrscale))
ess19$rw1 <- ifelse(ess19$lrscale == 10, 1, 0)    
addmargins(table(ess19$rw1))  # 3.1% far-right

ess19$rw2 <- ifelse(
                (ess19$rwingvote == 1 & ess19$rwingclose == 1), 1, 
             ifelse(
                (ess19$rwingvote == 1 & ess19$rwingclose == 2)|
                (ess19$rwingvote == 1 & ess19$rwingclose == 3)|
                (ess19$rwingvote == 2 & ess19$rwingclose == 1)|
                (ess19$rwingvote == 2 & ess19$rwingclose == 2)|
                (ess19$rwingvote == 2 & ess19$rwingclose == 3)|
                (ess19$rwingvote == 3 & ess19$rwingclose == 1)|
                (ess19$rwingvote == 3 & ess19$rwingclose == 2)|
                (ess19$rwingvote == 3 & ess19$rwingclose == 3)|
                (ess19$rwingvote == 4 & ess19$rwingclose == 1)|
                (ess19$rwingvote == 4 & ess19$rwingclose == 2), 0, 0)) 
addmargins(table(ess19$rw2))  # 100 * (5670 / 329929) = 1.7% 

addmargins(table(ess19$rwingvote))
addmargins(table(ess19$rwingclose))
addmargins(table(ess19$rwingvote, ess19$rwingclose))

addmargins(table(ess19$rw1, ess19$rw2))

ess19$rw <- ifelse(
  (ess19$rw1 == 0 & ess19$rw2 == 0), 0,  
            ifelse(
  (ess19$rw1 == 0 & ess19$rw2 == 1)|
  (ess19$rw1 == 1 & ess19$rw2 == 0), 1, 1))

addmargins(table(ess19$rw)) # 100 * (13900 / 292483) = 4.8%

# =================================================================================

# save(ess19, file="ess19.Rdata")

# =================================================================================

rm(list = ls())
load("ess19.Rdata")

# far-right prevalence: sample-weighted 
library(survey)
ess19dwg <- survey::svydesign(id = ~1, strata=~cntry, weights=~anweight, data = ess19)

tab1 <- survey::svytable(~ess19$rw1, ess19dwg, round=TRUE)
tab2 <- survey::svytable(~ess19$rw2, ess19dwg, round=TRUE)
tab3 <- survey::svytable(~ess19$rw,  ess19dwg, round=TRUE)
tab1 # rw1 (0, 1, sum): 277428   8865 286293 ( 3.1% ) (="10" on left-right)
tab2 # rw2 (0, 1, sum): 309946   4210 314156 ( 1.3% ) (=close/vote)
tab3 #  rw (0, 1, sum): 267507  12131 279638 ( 4.3% ) (combined by logical "or")

# 8865/286293=0.03096478
# 4210/314156=0.01340099
# 12131/279638=0.04338109


rwindex <- survey::svyglm(rw1 ~ rw2, design = ess19dwg, family=gaussian())
summary(rwindex)

# Assessing model fit
# Significance of Multiple R (Pearson correlation between observed and predicted Y)
cor.test(fitted(rwindex), (residuals(rwindex)+fitted(rwindex))) # r=0.09181877

# =========================================================================================
#
# Measurement of authoritarian, prejudice, and trust: Building separate factor-score scales 
#
# ======== MEASUREMENT MODEL 1 (authoritarian personality (proxy)) ========================
#
rm(list = ls())
load("ess19.Rdata")
#
ess19$LNR <- seq_len(340215)
vdaten <- subset(ess19, select = c(LNR,  
                                   impsafe, ipstrgv, ipbhprp,  
                                   ipfrule, imptrad, 
                                   anweight ))
vdaten <- na.omit(vdaten)


# scale direction reversed for value items
library(car)

vdaten$impsafer <- car::recode(vdaten$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
vdaten$ipstrgvr <- car::recode(vdaten$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
vdaten$ipbhprpr <- car::recode(vdaten$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
vdaten$ipfruler <- car::recode(vdaten$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
vdaten$imptradr <- car::recode(vdaten$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

library(lavaan)

# authoritarian values

value.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

'
value.fit <- cfa(value.model, data=vdaten, sampling.weights="anweight",
                 meanstructure = TRUE)
summary(value.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)

factor.scores <- lavaan::predict(value.fit, 
                                 newdata = vdaten )
factor.scores

facscores <- data.frame(vdaten, factor.scores) 

head(facscores, 4)
ess19 <- merge(ess19, facscores, by = "LNR",
               all.x = TRUE,
               all.y = TRUE)
# edit(data.frame(ess19))
save(ess19, file="ess19.Rdata")

# ======== MEASUREMENT MODEL 2 (prejudice) =======================================

rm(list = ls())
load("ess19.Rdata")

ess19$LNR <- seq_len(340215)
pdaten <- subset(ess19, select = c(LNR,  
                                   imsmetn, imdfetn, impcntr, 
                                   anweight.x ))
pdaten <- na.omit(pdaten)

library(lavaan)

# prejudice

prejudice.model <- '

prejudice =~ imsmetn + imdfetn + impcntr 

'
prejudice.fit <- cfa(prejudice.model, data=pdaten, sampling.weights="anweight.x",
                     ordered=c("imsmetn", "imdfetn", "impcntr"),
                     meanstructure = TRUE)

summary(prejudice.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)

factor.scores <- lavaan::predict(prejudice.fit, 
                                 newdata = pdaten )
factor.scores

facscores <- data.frame(pdaten, factor.scores) 

head(facscores, 4)
ess19 <- merge(ess19, facscores, by = "LNR",
               all.x = TRUE,
               all.y = TRUE)
# edit(data.frame(ess19))
save(ess19, file="ess19.Rdata")

# ======== MEASUREMENT MODEL 3 (trust) =======================================

rm(list = ls())
load("ess19.Rdata")

ess19$LNR <- seq_len(340215)
tdaten <- subset(ess19, select = c(LNR,  
                                   ppltrst, pplfair, pplhlp, 
                                   anweight.x.x ))
tdaten <- na.omit(tdaten)

library(lavaan)

# trust in humans

trust.model <- '

trust =~ ppltrst + pplfair + pplhlp 

'
trust.fit <- cfa(trust.model, data=tdaten, sampling.weights="anweight.x.x",
                 meanstructure = TRUE)

summary(trust.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)

factor.scores <- lavaan::predict(trust.fit, 
                                 newdata = tdaten )
factor.scores

facscores <- data.frame(tdaten, factor.scores) 

head(facscores, 4)
ess19 <- merge(ess19, facscores, by = "LNR",
               all.x = TRUE,
               all.y = TRUE)
# edit(data.frame(ess19))
save(ess19, file="ess19.Rdata")

# ====================================================================

rm(list = ls())
load("ess19.Rdata")

addmargins(table(ess19$impsafe.x))
addmargins(table(ess19$ipstrgv.x))
addmargins(table(ess19$ipbhprp.x))
addmargins(table(ess19$ipfrule.x))
addmargins(table(ess19$imptrad.x))
addmargins(table(ess19$imsmetn.x))
addmargins(table(ess19$imdfetn.x))
addmargins(table(ess19$impcntr.x))
addmargins(table(ess19$ppltrst.x))
addmargins(table(ess19$pplfair.x))
addmargins(table(ess19$pplhlp.x))

addmargins(table(ess19$rw1)) # n=299377
addmargins(table(ess19$rw2)) # n=329929
addmargins(table(ess19$rw))  # n=292483 

summary(ess19$authoritarian)
summary(ess19$prejudice)
summary(ess19$trust)

# ##################################################################################
# Testing block 1: Cross-validation using library "boot" and function cv.glm()
# run sequentially # MSE as criterion

rm(list = ls())
load("ess19.Rdata")

# rw

library(boot)

sset1 <- subset(ess19, select = c(authoritarian, rw, anweight.x.x.y )) # n=276,217
sset1 <- na.omit(sset1)

# as linear model

m1 <- glm(rw ~ authoritarian, weights = anweight.x.x.y,
          family=gaussian, data = sset1)

summary(m1)

RSquare <- function(x = m1$y, y = m1$fitted.values)
{rsq <- c(cor(m1$y, m1$fitted.values)^2)
rsq}
RSquare()

Adj.Dispersion <- var(m1$residuals) *(m1$df.residual/m1$df.null)
Adj.Dispersion

# MSE related to N 
MSE1 <- function(x = m1$y, y = m1$fitted.values)
{mean((m1$y - m1$fitted.values)^2)}
MSE1()

# cross-validation of linear model
set.seed(145)
(cv.mse1a <- cv.glm(sset1, m1, K=10)$delta) # using default
#
cost1 <- function(rw, pi=0) {mean((rw-pi)^2)}
set.seed(145)
(cv.mse1b <- cv.glm(sset1, m1, cost1, K=10)$delta) # formulating default explicitly

# as probit model

m2 <- glm(rw ~ authoritarian, weights = anweight.x.x.y,
          family=quasibinomial(link=probit),
          data = sset1)
summary(m2)

McF <- 1 - (m2$deviance/m2$null.deviance) # McFadden Pseudo-R2
McF

MSE2 <- function(x = m2$y, y = m2$fitted.values)
{mean((m2$y - m2$fitted.values)^2)}
MSE2()

# cross-validation of probit model

set.seed(321)
(cv.mse2a <- cv.glm(sset1, m2, K=10)$delta) # using default
#
cost2 <- function(rw, pi=0) {mean((rw-pi)^2)}
set.seed(321)
(cv.mse2b <- cv.glm(sset1, m2, cost2, K=10)$delta) # formulating default explicitly
# 

# ----------------------------------------------------------------------------------

rm(list = ls())
load("ess19.Rdata")
library(boot)

sset1 <- subset(ess19, select = c(authoritarian, prejudice, rw, anweight.x.x.y )) # n=268234
sset1 <- na.omit(sset1)

# as linear model

m1 <- glm(rw ~ authoritarian+prejudice, weights = anweight.x.x.y,
          family=gaussian, data = sset1)

summary(m1)

RSquare <- function(x = m1$y, y = m1$fitted.values)
{rsq <- c(cor(m1$y, m1$fitted.values)^2)
rsq}
RSquare()

Adj.Dispersion <- var(m1$residuals) *(m1$df.residual/m1$df.null)
Adj.Dispersion

# MSE related to N 
MSE1 <- function(x = m1$y, y = m1$fitted.values)
{mean((m1$y - m1$fitted.values)^2)}
MSE1()

# cross-validation of linear model
set.seed(145)
(cv.mse1a <- cv.glm(sset1, m1, K=10)$delta) # using default
#
cost1 <- function(rw, pi=0) {mean((rw-pi)^2)}
set.seed(145)
(cv.mse1b <- cv.glm(sset1, m1, cost1, K=10)$delta) # formulating default explicitly

# as probit model

m2 <- glm(rw ~ authoritarian+prejudice, weights = anweight.x.x.y,
          family=quasibinomial(link=probit),
          data = sset1)
summary(m2)

McF <- 1 - (m2$deviance/m2$null.deviance) # McFadden Pseudo-R2
McF

MSE2 <- function(x = m2$y, y = m2$fitted.values)
{mean((m2$y - m2$fitted.values)^2)}
MSE2()

# cross-validation of probit model

set.seed(321)
(cv.mse2a <- cv.glm(sset1, m2, K=10)$delta) # using default
#
cost2 <- function(rw, pi=0) {mean((rw-pi)^2)}
set.seed(321)
(cv.mse2b <- cv.glm(sset1, m2, cost2, K=10)$delta) # formulating default explicitly
# 
# ----------------------------------------------------------------------------------

rm(list = ls())
load("ess19.Rdata")
library(boot)

sset1 <- subset(ess19, select = c(authoritarian, prejudice, trust, rw, anweight.x.x.y )) # n=268234
sset1 <- na.omit(sset1)

# as linear model

m1 <- glm(rw ~ authoritarian+prejudice+trust, weights = anweight.x.x.y,
          family=gaussian, data = sset1)

summary(m1)

RSquare <- function(x = m1$y, y = m1$fitted.values)
{rsq <- c(cor(m1$y, m1$fitted.values)^2)
rsq}
RSquare()

Adj.Dispersion <- var(m1$residuals) *(m1$df.residual/m1$df.null)
Adj.Dispersion

# MSE related to N 
MSE1 <- function(x = m1$y, y = m1$fitted.values)
{mean((m1$y - m1$fitted.values)^2)}
MSE1()

# cross-validation of linear model
set.seed(145)
(cv.mse1a <- cv.glm(sset1, m1, K=10)$delta) # using default
#
cost1 <- function(rw, pi=0) {mean((rw-pi)^2)}
set.seed(145)
(cv.mse1b <- cv.glm(sset1, m1, cost1, K=10)$delta) # formulating default explicitly

# as probit model

m2 <- glm(rw ~ authoritarian+prejudice+trust, weights = anweight.x.x.y,
          family=quasibinomial(link=probit),
          data = sset1)
summary(m2)

McF <- 1 - (m2$deviance/m2$null.deviance) # McFadden Pseudo-R2
McF

MSE2 <- function(x = m2$y, y = m2$fitted.values)
{mean((m2$y - m2$fitted.values)^2)}
MSE2()

# cross-validation of probit model

set.seed(321)
(cv.mse2a <- cv.glm(sset1, m2, K=10)$delta) # using default
#
cost2 <- function(rw, pi=0) {mean((rw-pi)^2)}
set.seed(321)
(cv.mse2b <- cv.glm(sset1, m2, cost2, K=10)$delta) # formulating default explicitly
# 
# ----------------------------------------------------------------------------------

rm(list = ls())
load("ess19.Rdata")

library(boot)

sset1 <- subset(ess19, select = c(authoritarian, prejudice, anweight.x.x.y ))  
sset1 <- na.omit(sset1)

# as linear model

m1 <- glm(prejudice ~ authoritarian, weights = anweight.x.x.y,
          family=gaussian, data = sset1)

summary(m1)

RSquare <- function(x = m1$y, y = m1$fitted.values)
{rsq <- c(cor(m1$y, m1$fitted.values)^2)
rsq}
RSquare()

Adj.Dispersion <- var(m1$residuals) *(m1$df.residual/m1$df.null)
Adj.Dispersion

# MSE related to N 
MSE1 <- function(x = m1$y, y = m1$fitted.values)
{mean((m1$y - m1$fitted.values)^2)}
MSE1()

# cross-validation of linear model
set.seed(145)
(cv.mse1a <- cv.glm(sset1, m1, K=10)$delta) # using default
#
cost1 <- function(rw, pi=0) {mean((rw-pi)^2)}
set.seed(145)
(cv.mse1b <- cv.glm(sset1, m1, cost1, K=10)$delta) # formulating default explicitly

# --------------------------------------------------------------------------------

rm(list = ls())
load("ess19.Rdata")

library(boot)

sset1 <- subset(ess19, select = c(prejudice, trust, anweight.x.x.y ))  
sset1 <- na.omit(sset1)

# as linear model

m1 <- glm(trust ~ prejudice, weights = anweight.x.x.y,
          family=gaussian, data = sset1)

summary(m1)

RSquare <- function(x = m1$y, y = m1$fitted.values)
{rsq <- c(cor(m1$y, m1$fitted.values)^2)
rsq}
RSquare()

Adj.Dispersion <- var(m1$residuals) *(m1$df.residual/m1$df.null)
Adj.Dispersion

# MSE related to N 
MSE1 <- function(x = m1$y, y = m1$fitted.values)
{mean((m1$y - m1$fitted.values)^2)}
MSE1()

# cross-validation of linear model
set.seed(145)
(cv.mse1a <- cv.glm(sset1, m1, K=10)$delta) # using default
#
cost1 <- function(rw, pi=0) {mean((rw-pi)^2)}
set.seed(145)
(cv.mse1b <- cv.glm(sset1, m1, cost1, K=10)$delta) # formulating default explicitly


# --------------------------------------------------------------------------------

# rw1

rm(list = ls())
load("ess19.Rdata")

library(boot)

sset1 <- subset(ess19, select = c(authoritarian, rw1, anweight.x.x.y )) # 
sset1 <- na.omit(sset1)

# as linear model

m1 <- glm(rw1 ~ authoritarian, weights = anweight.x.x.y,
          family=gaussian, data = sset1)

summary(m1)

RSquare <- function(x = m1$y, y = m1$fitted.values)
{rsq <- c(cor(m1$y, m1$fitted.values)^2)
rsq}
RSquare()

Adj.Dispersion <- var(m1$residuals) *(m1$df.residual/m1$df.null)
Adj.Dispersion

# MSE related to N 
MSE1 <- function(x = m1$y, y = m1$fitted.values)
{mean((m1$y - m1$fitted.values)^2)}
MSE1()

# cross-validation of linear model
set.seed(145)
(cv.mse1a <- cv.glm(sset1, m1, K=10)$delta) # using default
#
cost1 <- function(rw1, pi=0) {mean((rw1-pi)^2)}
set.seed(145)
(cv.mse1b <- cv.glm(sset1, m1, cost1, K=10)$delta) # formulating default explicitly

# as probit model

m2 <- glm(rw1 ~ authoritarian, weights = anweight.x.x.y,
          family=quasibinomial(link=probit),
          data = sset1)
summary(m2)

McF <- 1 - (m2$deviance/m2$null.deviance) # McFadden Pseudo-R2
McF

MSE2 <- function(x = m2$y, y = m2$fitted.values)
{mean((m2$y - m2$fitted.values)^2)}
MSE2()

# cross-validation of probit model

set.seed(321)
(cv.mse2a <- cv.glm(sset1, m2, K=10)$delta) # using default
#
cost2 <- function(rw1, pi=0) {mean((rw1-pi)^2)}
set.seed(321)
(cv.mse2b <- cv.glm(sset1, m2, cost2, K=10)$delta) # formulating default explicitly
# 

# ----------------------------------------------------------------------------------

rm(list = ls())
load("ess19.Rdata")
library(boot)

sset1 <- subset(ess19, select = c(authoritarian, prejudice, rw1, anweight.x.x.y )) 
sset1 <- na.omit(sset1)

# as linear model

m1 <- glm(rw1 ~ authoritarian+prejudice, weights = anweight.x.x.y,
          family=gaussian, data = sset1)

summary(m1)

RSquare <- function(x = m1$y, y = m1$fitted.values)
{rsq <- c(cor(m1$y, m1$fitted.values)^2)
rsq}
RSquare()

Adj.Dispersion <- var(m1$residuals) *(m1$df.residual/m1$df.null)
Adj.Dispersion

# MSE related to N 
MSE1 <- function(x = m1$y, y = m1$fitted.values)
{mean((m1$y - m1$fitted.values)^2)}
MSE1()

# cross-validation of linear model
set.seed(145)
(cv.mse1a <- cv.glm(sset1, m1, K=10)$delta) # using default
#
cost1 <- function(rw1, pi=0) {mean((rw1-pi)^2)}
set.seed(145)
(cv.mse1b <- cv.glm(sset1, m1, cost1, K=10)$delta) # formulating default explicitly

# as probit model

m2 <- glm(rw1 ~ authoritarian+prejudice, weights = anweight.x.x.y,
          family=quasibinomial(link=probit),
          data = sset1)
summary(m2)

McF <- 1 - (m2$deviance/m2$null.deviance) # McFadden Pseudo-R2
McF

MSE2 <- function(x = m2$y, y = m2$fitted.values)
{mean((m2$y - m2$fitted.values)^2)}
MSE2()

# cross-validation of probit model

set.seed(321)
(cv.mse2a <- cv.glm(sset1, m2, K=10)$delta) # using default
#
cost2 <- function(rw1, pi=0) {mean((rw1-pi)^2)}
set.seed(321)
(cv.mse2b <- cv.glm(sset1, m2, cost2, K=10)$delta) # formulating default explicitly
# 
# ----------------------------------------------------------------------------------

rm(list = ls())
load("ess19.Rdata")
library(boot)

sset1 <- subset(ess19, select = c(authoritarian, prejudice, trust, rw1, anweight.x.x.y )) # 
sset1 <- na.omit(sset1)

# as linear model

m1 <- glm(rw1 ~ authoritarian+prejudice+trust, weights = anweight.x.x.y,
          family=gaussian, data = sset1)

summary(m1)

RSquare <- function(x = m1$y, y = m1$fitted.values)
{rsq <- c(cor(m1$y, m1$fitted.values)^2)
rsq}
RSquare()

Adj.Dispersion <- var(m1$residuals) *(m1$df.residual/m1$df.null)
Adj.Dispersion

# MSE related to N 
MSE1 <- function(x = m1$y, y = m1$fitted.values)
{mean((m1$y - m1$fitted.values)^2)}
MSE1()

# cross-validation of linear model
set.seed(145)
(cv.mse1a <- cv.glm(sset1, m1, K=10)$delta) # using default
#
cost1 <- function(rw1, pi=0) {mean((rw1-pi)^2)}
set.seed(145)
(cv.mse1b <- cv.glm(sset1, m1, cost1, K=10)$delta) # formulating default explicitly

# as probit model

m2 <- glm(rw1 ~ authoritarian+prejudice+trust, weights = anweight.x.x.y,
          family=quasibinomial(link=probit),
          data = sset1)
summary(m2)

McF <- 1 - (m2$deviance/m2$null.deviance) # McFadden Pseudo-R2
McF

MSE2 <- function(x = m2$y, y = m2$fitted.values)
{mean((m2$y - m2$fitted.values)^2)}
MSE2()

# cross-validation of probit model

set.seed(321)
(cv.mse2a <- cv.glm(sset1, m2, K=10)$delta) # using default
#
cost2 <- function(rw1, pi=0) {mean((rw1-pi)^2)}
set.seed(321)
(cv.mse2b <- cv.glm(sset1, m2, cost2, K=10)$delta) # formulating default explicitly
# 
# --------------------------------------------------------------------------------

# rw2

rm(list = ls())
load("ess19.Rdata")

library(boot)

sset1 <- subset(ess19, select = c(authoritarian, rw2, anweight.x.x.y )) # 
sset1 <- na.omit(sset1)

# as linear model

m1 <- glm(rw2 ~ authoritarian, weights = anweight.x.x.y,
          family=gaussian, data = sset1)

summary(m1)

RSquare <- function(x = m1$y, y = m1$fitted.values)
{rsq <- c(cor(m1$y, m1$fitted.values)^2)
rsq}
RSquare()

Adj.Dispersion <- var(m1$residuals) *(m1$df.residual/m1$df.null)
Adj.Dispersion

# MSE related to N 
MSE1 <- function(x = m1$y, y = m1$fitted.values)
{mean((m1$y - m1$fitted.values)^2)}
MSE1()

# cross-validation of linear model
set.seed(145)
(cv.mse1a <- cv.glm(sset1, m1, K=10)$delta) # using default
#
cost1 <- function(rw2, pi=0) {mean((rw2-pi)^2)}
set.seed(145)
(cv.mse1b <- cv.glm(sset1, m1, cost1, K=10)$delta) # formulating default explicitly

# as probit model

m2 <- glm(rw2 ~ authoritarian, weights = anweight.x.x.y,
          family=quasibinomial(link=probit),
          data = sset1)
summary(m2)

McF <- 1 - (m2$deviance/m2$null.deviance) # McFadden Pseudo-R2
McF

MSE2 <- function(x = m2$y, y = m2$fitted.values)
{mean((m2$y - m2$fitted.values)^2)}
MSE2()

# cross-validation of probit model

set.seed(321)
(cv.mse2a <- cv.glm(sset1, m2, K=10)$delta) # using default
#
cost2 <- function(rw2, pi=0) {mean((rw2-pi)^2)}
set.seed(321)
(cv.mse2b <- cv.glm(sset1, m2, cost2, K=10)$delta) # formulating default explicitly
# 

# ----------------------------------------------------------------------------------

rm(list = ls())
load("ess19.Rdata")
library(boot)

sset1 <- subset(ess19, select = c(authoritarian, prejudice, rw2, anweight.x.x.y )) 
sset1 <- na.omit(sset1)

# as linear model

m1 <- glm(rw2 ~ authoritarian+prejudice, weights = anweight.x.x.y,
          family=gaussian, data = sset1)

summary(m1)

RSquare <- function(x = m1$y, y = m1$fitted.values)
{rsq <- c(cor(m1$y, m1$fitted.values)^2)
rsq}
RSquare()

Adj.Dispersion <- var(m1$residuals) *(m1$df.residual/m1$df.null)
Adj.Dispersion

# MSE related to N 
MSE1 <- function(x = m1$y, y = m1$fitted.values)
{mean((m1$y - m1$fitted.values)^2)}
MSE1()

# cross-validation of linear model
set.seed(145)
(cv.mse1a <- cv.glm(sset1, m1, K=10)$delta) # using default
#
cost1 <- function(rw2, pi=0) {mean((rw2-pi)^2)}
set.seed(145)
(cv.mse1b <- cv.glm(sset1, m1, cost1, K=10)$delta) # formulating default explicitly

# as probit model

m2 <- glm(rw2 ~ authoritarian+prejudice, weights = anweight.x.x.y,
          family=quasibinomial(link=probit),
          data = sset1)
summary(m2)

McF <- 1 - (m2$deviance/m2$null.deviance) # McFadden Pseudo-R2
McF

MSE2 <- function(x = m2$y, y = m2$fitted.values)
{mean((m2$y - m2$fitted.values)^2)}
MSE2()

# cross-validation of probit model

set.seed(321)
(cv.mse2a <- cv.glm(sset1, m2, K=10)$delta) # using default
#
cost2 <- function(rw2, pi=0) {mean((rw2-pi)^2)}
set.seed(321)
(cv.mse2b <- cv.glm(sset1, m2, cost2, K=10)$delta) # formulating default explicitly
# 
# ----------------------------------------------------------------------------------

rm(list = ls())
load("ess19.Rdata")
library(boot)

sset1 <- subset(ess19, select = c(authoritarian, prejudice, trust, rw2, anweight.x.x.y )) # 
sset1 <- na.omit(sset1)

# as linear model

m1 <- glm(rw2 ~ authoritarian+prejudice+trust, weights = anweight.x.x.y,
          family=gaussian, data = sset1)

summary(m1)

RSquare <- function(x = m1$y, y = m1$fitted.values)
{rsq <- c(cor(m1$y, m1$fitted.values)^2)
rsq}
RSquare()

Adj.Dispersion <- var(m1$residuals) *(m1$df.residual/m1$df.null)
Adj.Dispersion

# MSE related to N 
MSE1 <- function(x = m1$y, y = m1$fitted.values)
{mean((m1$y - m1$fitted.values)^2)}
MSE1()

# cross-validation of linear model
set.seed(145)
(cv.mse1a <- cv.glm(sset1, m1, K=10)$delta) # using default
#
cost1 <- function(rw2, pi=0) {mean((rw2-pi)^2)}
set.seed(145)
(cv.mse1b <- cv.glm(sset1, m1, cost1, K=10)$delta) # formulating default explicitly

# as probit model

m2 <- glm(rw2 ~ authoritarian+prejudice+trust, weights = anweight.x.x.y,
          family=quasibinomial(link=probit),
          data = sset1)
summary(m2)

McF <- 1 - (m2$deviance/m2$null.deviance) # McFadden Pseudo-R2
McF

MSE2 <- function(x = m2$y, y = m2$fitted.values)
{mean((m2$y - m2$fitted.values)^2)}
MSE2()

# cross-validation of probit model

set.seed(321)
(cv.mse2a <- cv.glm(sset1, m2, K=10)$delta) # using default
#
cost2 <- function(rw2, pi=0) {mean((rw2-pi)^2)}
set.seed(321)
(cv.mse2b <- cv.glm(sset1, m2, cost2, K=10)$delta) # formulating default explicitly

# ##################################################################################

# Testing block 2: Validation set design 
# run sequentially # MSE as criterion

# ------------------- VALIDATION SET DESIGN ---------------------------------------
#
#                       
rm(list = ls())
load("ess19.Rdata")

sset1 <- subset(ess19, select = c(authoritarian, rw, anweight.x.x.y ))  
sset1 <- na.omit(sset1) # 276,217

set.seed(123)
train <- sample(276217, 193352) # 276217 * 0.7 = 193,352

# Linear model

m1 <- glm(rw ~ authoritarian, weights = anweight.x.x.y,
          family=gaussian,
          subset = train, data=sset1)
summary(m1)

RSquare <- function(x = m1$y, y = m1$fitted.values)
{rsq <- c(cor(m1$y, m1$fitted.values)^2)
rsq}
RSquare()

Adj.Dispersion <- var(m1$residuals) *(m1$df.residual/m1$df.null)
Adj.Dispersion

# MSE related to N 
MSE1 <- function(x = m1$y, y = m1$fitted.values)
{mean((m1$y - m1$fitted.values)^2)}
MSE1()
mean((sset1$rw-predict(m1, sset1))[-train]^2)

# probit model

m2 <- glm(rw ~ authoritarian, weights = anweight.x.x.y,
          family=quasibinomial(link=probit),
          subset = train, data=sset1)
summary(m2)

McF <- 1 - (m2$deviance/m2$null.deviance) # McFadden Pseudo-R2
McF

MSE2 <- function(x = m2$y, y = m2$fitted.values)
{mean((m2$y - m2$fitted.values)^2)}
MSE2()
mean((sset1$rw-predict(m2, type="response", sset1))[-train]^2) 

# ---------------------------------------------------------------------

rm(list = ls())
load("ess19.Rdata")

sset1 <- subset(ess19, select = c(authoritarian, prejudice, rw, anweight.x.x.y ))  
sset1 <- na.omit(sset1) # 268,234

set.seed(123)
train <- sample(268234, 187764) # 268234 * 0.7 = 187,764

# Linear model

m1 <- glm(rw ~ authoritarian+prejudice, weights = anweight.x.x.y,
          family=gaussian,
          subset = train, data=sset1)
summary(m1)

RSquare <- function(x = m1$y, y = m1$fitted.values)
{rsq <- c(cor(m1$y, m1$fitted.values)^2)
rsq}
RSquare()

Adj.Dispersion <- var(m1$residuals) *(m1$df.residual/m1$df.null)
Adj.Dispersion

# MSE related to N 
MSE1 <- function(x = m1$y, y = m1$fitted.values)
{mean((m1$y - m1$fitted.values)^2)}
MSE1()
mean((sset1$rw-predict(m1, sset1))[-train]^2)

# probit model

m2 <- glm(rw ~ authoritarian+prejudice, weights = anweight.x.x.y,
          family=quasibinomial(link=probit),
          subset = train, data=sset1)
summary(m2)

McF <- 1 - (m2$deviance/m2$null.deviance) # McFadden Pseudo-R2
McF

MSE2 <- function(x = m2$y, y = m2$fitted.values)
{mean((m2$y - m2$fitted.values)^2)}
MSE2()
mean((sset1$rw-predict(m2, type="response", sset1))[-train]^2) 

# ------------------------------------------------------------

rm(list = ls())
load("ess19.Rdata")

sset1 <- subset(ess19, select = c(authoritarian, prejudice, trust, rw, anweight.x.x.y ))  
sset1 <- na.omit(sset1) # 268,234

set.seed(123)
train <- sample(268234, 187764) # 268234 * 0.7 = 187,764

# Linear model

m1 <- glm(rw ~ authoritarian+prejudice+trust, weights = anweight.x.x.y,
          family=gaussian,
          subset = train, data=sset1)
summary(m1)

RSquare <- function(x = m1$y, y = m1$fitted.values)
{rsq <- c(cor(m1$y, m1$fitted.values)^2)
rsq}
RSquare()

Adj.Dispersion <- var(m1$residuals) *(m1$df.residual/m1$df.null)
Adj.Dispersion

# MSE related to N 
MSE1 <- function(x = m1$y, y = m1$fitted.values)
{mean((m1$y - m1$fitted.values)^2)}
MSE1()
mean((sset1$rw-predict(m1, sset1))[-train]^2)

# probit model

m2 <- glm(rw ~ authoritarian+prejudice+trust, weights = anweight.x.x.y,
          family=quasibinomial(link=probit),
          subset = train, data=sset1)
summary(m2)

McF <- 1 - (m2$deviance/m2$null.deviance) # McFadden Pseudo-R2
McF

MSE2 <- function(x = m2$y, y = m2$fitted.values)
{mean((m2$y - m2$fitted.values)^2)}
MSE2()
mean((sset1$rw-predict(m2, type="response", sset1))[-train]^2) 

# ------------------------------------------------------------

rm(list = ls())
load("ess19.Rdata")

sset1 <- subset(ess19, select = c(authoritarian, prejudice, anweight.x.x.y ))  
sset1 <- na.omit(sset1) # 305,492

set.seed(123)
train <- sample(305492, 213844) # 305492 * 0.7 = 213,844

# Linear model

m1 <- glm(prejudice ~ authoritarian, weights = anweight.x.x.y,
          family=gaussian,
          subset = train, data=sset1)
summary(m1)

RSquare <- function(x = m1$y, y = m1$fitted.values)
{rsq <- c(cor(m1$y, m1$fitted.values)^2)
rsq}
RSquare()

Adj.Dispersion <- var(m1$residuals) *(m1$df.residual/m1$df.null)
Adj.Dispersion

# MSE related to N 
MSE1 <- function(x = m1$y, y = m1$fitted.values)
{mean((m1$y - m1$fitted.values)^2)}
MSE1()
mean((sset1$prejudice-predict(m1, sset1))[-train]^2)

# ------------------------------------------------------------

rm(list = ls())
load("ess19.Rdata")

sset1 <- subset(ess19, select = c(trust, prejudice, anweight.x.x.y ))  
sset1 <- na.omit(sset1) # 322,020

set.seed(123)
train <- sample(322020, 225414) # 322,020 * 0.7 = 225,414

# Linear model

m1 <- glm(trust ~ prejudice, weights = anweight.x.x.y,
          family=gaussian,
          subset = train, data=sset1)
summary(m1)

RSquare <- function(x = m1$y, y = m1$fitted.values)
{rsq <- c(cor(m1$y, m1$fitted.values)^2)
rsq}
RSquare()

Adj.Dispersion <- var(m1$residuals) *(m1$df.residual/m1$df.null)
Adj.Dispersion

# MSE related to N 
MSE1 <- function(x = m1$y, y = m1$fitted.values)
{mean((m1$y - m1$fitted.values)^2)}
MSE1()
mean((sset1$trust-predict(m1, sset1))[-train]^2)

# ================================================================

rm(list = ls())
load("ess19.Rdata")

sset1 <- subset(ess19, select = c(authoritarian, rw1, anweight.x.x.y ))  
sset1 <- na.omit(sset1) # 282,343

set.seed(123)
train <- sample(282343, 197640) # 282343 * 0.7 = 197640


# Linear model

m1 <- glm(rw1 ~ authoritarian, weights = anweight.x.x.y,
          family=gaussian,
          subset = train, data=sset1)
summary(m1)

RSquare <- function(x = m1$y, y = m1$fitted.values)
{rsq <- c(cor(m1$y, m1$fitted.values)^2)
rsq}
RSquare()

Adj.Dispersion <- var(m1$residuals) *(m1$df.residual/m1$df.null)
Adj.Dispersion

# MSE related to N 
MSE1 <- function(x = m1$y, y = m1$fitted.values)
{mean((m1$y - m1$fitted.values)^2)}
MSE1()
mean((sset1$rw1-predict(m1, sset1))[-train]^2)

# probit model

m2 <- glm(rw1 ~ authoritarian, weights = anweight.x.x.y,
          family=quasibinomial(link=probit),
          subset = train, data=sset1)
summary(m2)

McF <- 1 - (m2$deviance/m2$null.deviance) # McFadden Pseudo-R2
McF

MSE2 <- function(x = m2$y, y = m2$fitted.values)
{mean((m2$y - m2$fitted.values)^2)}
MSE2()
mean((sset1$rw1-predict(m2, type="response", sset1))[-train]^2) 

# ---------------------------------------------------------------------

rm(list = ls())
load("ess19.Rdata")

sset1 <- subset(ess19, select = c(authoritarian, prejudice, rw1, anweight.x.x.y ))  
sset1 <- na.omit(sset1) # 273,776

set.seed(123)
train <- sample(273776, 191643) # 273776 * 0.7 = 191,643


# Linear model

m1 <- glm(rw1 ~ authoritarian+prejudice, weights = anweight.x.x.y,
          family=gaussian,
          subset = train, data=sset1)
summary(m1)

RSquare <- function(x = m1$y, y = m1$fitted.values)
{rsq <- c(cor(m1$y, m1$fitted.values)^2)
rsq}
RSquare()

Adj.Dispersion <- var(m1$residuals) *(m1$df.residual/m1$df.null)
Adj.Dispersion

# MSE related to N 
MSE1 <- function(x = m1$y, y = m1$fitted.values)
{mean((m1$y - m1$fitted.values)^2)}
MSE1()
mean((sset1$rw1-predict(m1, sset1))[-train]^2)

# probit model

m2 <- glm(rw1 ~ authoritarian+prejudice, weights = anweight.x.x.y,
          family=quasibinomial(link=probit),
          subset = train, data=sset1)
summary(m2)

McF <- 1 - (m2$deviance/m2$null.deviance) # McFadden Pseudo-R2
McF

MSE2 <- function(x = m2$y, y = m2$fitted.values)
{mean((m2$y - m2$fitted.values)^2)}
MSE2()
mean((sset1$rw1-predict(m2, type="response", sset1))[-train]^2) 

# ------------------------------------------------------------

rm(list = ls())
load("ess19.Rdata")

sset1 <- subset(ess19, select = c(authoritarian, prejudice, trust, rw1, anweight.x.x.y ))  
sset1 <- na.omit(sset1) # 273,776

set.seed(123)
train <- sample(273776, 191643) # 273776 * 0.7 = 191,643

# Linear model

m1 <- glm(rw1 ~ authoritarian+prejudice+trust, weights = anweight.x.x.y,
          family=gaussian,
          subset = train, data=sset1)
summary(m1)

RSquare <- function(x = m1$y, y = m1$fitted.values)
{rsq <- c(cor(m1$y, m1$fitted.values)^2)
rsq}
RSquare()

Adj.Dispersion <- var(m1$residuals) *(m1$df.residual/m1$df.null)
Adj.Dispersion

# MSE related to N 
MSE1 <- function(x = m1$y, y = m1$fitted.values)
{mean((m1$y - m1$fitted.values)^2)}
MSE1()
mean((sset1$rw1-predict(m1, sset1))[-train]^2)

# probit model

m2 <- glm(rw1 ~ authoritarian+prejudice+trust, weights = anweight.x.x.y,
          family=quasibinomial(link=probit),
          subset = train, data=sset1)
summary(m2)

McF <- 1 - (m2$deviance/m2$null.deviance) # McFadden Pseudo-R2
McF

MSE2 <- function(x = m2$y, y = m2$fitted.values)
{mean((m2$y - m2$fitted.values)^2)}
MSE2()
mean((sset1$rw1-predict(m2, type="response", sset1))[-train]^2) 


# MODELs for rw2

rm(list = ls())
load("ess19.Rdata")

sset1 <- subset(ess19, select = c(authoritarian, rw2, anweight.x.x.y ))  
sset1 <- na.omit(sset1) # 308,861

set.seed(123)
train <- sample(308861, 216203) # 308,861 * 0.7 =  216,203

# Linear model

m1 <- glm(rw2 ~ authoritarian, weights = anweight.x.x.y,
          family=gaussian,
          subset = train, data=sset1)
summary(m1)

RSquare <- function(x = m1$y, y = m1$fitted.values)
{rsq <- c(cor(m1$y, m1$fitted.values)^2)
rsq}
RSquare()

Adj.Dispersion <- var(m1$residuals) *(m1$df.residual/m1$df.null)
Adj.Dispersion

# MSE related to N 
MSE1 <- function(x = m1$y, y = m1$fitted.values)
{mean((m1$y - m1$fitted.values)^2)}
MSE1()
mean((sset1$rw2-predict(m1, sset1))[-train]^2)

# probit model

m2 <- glm(rw2 ~ authoritarian, weights = anweight.x.x.y,
          family=quasibinomial(link=probit),
          subset = train, data=sset1)
summary(m2)

McF <- 1 - (m2$deviance/m2$null.deviance) # McFadden Pseudo-R2
McF

MSE2 <- function(x = m2$y, y = m2$fitted.values)
{mean((m2$y - m2$fitted.values)^2)}
MSE2()
mean((sset1$rw2-predict(m2, type="response", sset1))[-train]^2) 

# ---------------------------------------------------------------------

rm(list = ls())
load("ess19.Rdata")

sset1 <- subset(ess19, select = c(authoritarian, prejudice, rw2, anweight.x.x.y ))  
sset1 <- na.omit(sset1) # 297,771

set.seed(123)
train <- sample(297771, 208440) # 297771 * 0.7 = 208440

# Linear model

m1 <- glm(rw2 ~ authoritarian+prejudice, weights = anweight.x.x.y,
          family=gaussian,
          subset = train, data=sset1)
summary(m1)

RSquare <- function(x = m1$y, y = m1$fitted.values)
{rsq <- c(cor(m1$y, m1$fitted.values)^2)
rsq}
RSquare()

Adj.Dispersion <- var(m1$residuals) *(m1$df.residual/m1$df.null)
Adj.Dispersion

# MSE related to N 
MSE1 <- function(x = m1$y, y = m1$fitted.values)
{mean((m1$y - m1$fitted.values)^2)}
MSE1()
mean((sset1$rw2-predict(m1, sset1))[-train]^2)

# probit model

m2 <- glm(rw2 ~ authoritarian+prejudice, weights = anweight.x.x.y,
          family=quasibinomial(link=probit),
          subset = train, data=sset1)
summary(m2)

McF <- 1 - (m2$deviance/m2$null.deviance) # McFadden Pseudo-R2
McF

MSE2 <- function(x = m2$y, y = m2$fitted.values)
{mean((m2$y - m2$fitted.values)^2)}
MSE2()
mean((sset1$rw2-predict(m2, type="response", sset1))[-train]^2) 

# ------------------------------------------------------------

rm(list = ls())
load("ess19.Rdata")

sset1 <- subset(ess19, select = c(authoritarian, prejudice, trust, rw2, anweight.x.x.y ))  
sset1 <- na.omit(sset1) # 297,771

set.seed(123)
train <- sample(297771, 208440) # 297771 * 0.7 = 208440

# Linear model

m1 <- glm(rw2 ~ authoritarian+prejudice+trust, weights = anweight.x.x.y,
          family=gaussian,
          subset = train, data=sset1)
summary(m1)

RSquare <- function(x = m1$y, y = m1$fitted.values)
{rsq <- c(cor(m1$y, m1$fitted.values)^2)
rsq}
RSquare()

Adj.Dispersion <- var(m1$residuals) *(m1$df.residual/m1$df.null)
Adj.Dispersion

# MSE related to N 
MSE1 <- function(x = m1$y, y = m1$fitted.values)
{mean((m1$y - m1$fitted.values)^2)}
MSE1()
mean((sset1$rw2-predict(m1, sset1))[-train]^2)

# probit model

m2 <- glm(rw2 ~ authoritarian+prejudice+trust, weights = anweight.x.x.y,
          family=quasibinomial(link=probit),
          subset = train, data=sset1)
summary(m2)

McF <- 1 - (m2$deviance/m2$null.deviance) # McFadden Pseudo-R2
McF

MSE2 <- function(x = m2$y, y = m2$fitted.values)
{mean((m2$y - m2$fitted.values)^2)}
MSE2()
mean((sset1$rw2-predict(m2, type="response", sset1))[-train]^2) 

# #######################################################################
#
# Testing block 3: Validation set design, structural equation models 
# run sequentially # SRMR as criterion
#
# ------------------- SEM in VALIDATION SET DESIGN -----------
#
# -------------------Target  variable rw  --------------------

#
# MODEL 1 FOR rw
#
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, select = c(LNR, cntry, 
                                  impsafe.x, ipstrgv.x, ipbhprp.x,  
                                  ipfrule.x, imptrad.x, 
                                  imsmetn.x, imdfetn.x, impcntr.x, 
                                  ppltrst.x, pplfair.x, pplhlp.x,
                                  rw, anweight.x.x.x ))
#
sset1 <- na.omit(sset1) # results in 268234/340215 # 78.84% of initial sample size
#
set.seed(123)
#
tr <- sample(268234, 187764)
traindata  <- sset1[tr, ] # results in 187,764 cases
testdata  <- sset1[-tr, ] # results in 80,470 cases
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw.fit <- sem(rw.model, data=traindata, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0)
r6  <- c(1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6)
m01
#
#
#
x <- unlist(rw.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/21)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rwt.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rwt.fit <- sem(rwt.model, data=testdata, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
              meanstructure = TRUE)
#
xt <- unlist(rwt.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/21)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rws.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rws.fit <- sem(rws.model, data=sset1, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)

fitmeasures(rws.fit, fit.measures = c("ecvi"))

#
# ========================================================================================
#
# MODEL 2 FOR rw
#
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, select = c(LNR, cntry, 
                                  impsafe.x, ipstrgv.x, ipbhprp.x,  
                                  ipfrule.x, imptrad.x, 
                                  imsmetn.x, imdfetn.x, impcntr.x, 
                                  ppltrst.x, pplfair.x, pplhlp.x,
                                  rw, anweight.x.x.x ))
#
sset1 <- na.omit(sset1) # results in 268234/340215 # 78.84% of initial sample size
#
set.seed(123)
#
tr <- sample(268234, 187764)
traindata  <- sset1[tr, ] # results in 187,764 cases
testdata  <- sset1[-tr, ] # results in 80,470 cases
#
#
library(lavaan)
#
rw.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw.fit <- sem(rw.model, data=traindata, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
#
m01 <- rbind(r1, r2, r3, r4)
m01
#
x <- as.data.frame(rw.fit@SampleStats@cov)
observed <- x * m01
#
y <- as.data.frame(rw.fit@implied$cov)
implied <- y * m01
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/10)
TrainingSRMR
#
# same model for testdata to get its observed covariance matrix
#
rwt.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rwt.fit <- sem(rwt.model, data=testdata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)
#
xt <- as.data.frame(rwt.fit@SampleStats@cov)
observedt <- xt * m01
observedt
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/10)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                     from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
rws.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rws.fit <- sem(rws.model, data=sset1, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)
#
fitmeasures(rws.fit, fit.measures = c("ecvi"))
#
# ================================================================
#
# MODEL 3 FOR rw
#
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, select = c(LNR, cntry, 
                                  impsafe.x, ipstrgv.x, ipbhprp.x,  
                                  ipfrule.x, imptrad.x, 
                                  imsmetn.x, imdfetn.x, impcntr.x, 
                                  ppltrst.x, pplfair.x, pplhlp.x,
                                  rw, anweight.x.x.x ))

sset1 <- na.omit(sset1) # results in 268234/340215 # 78.84% of initial sample size

set.seed(123)

tr <- sample(268234, 187764)
traindata  <- sset1[tr, ] # results in 187,764 cases
testdata  <- sset1[-tr, ] # results in 80,470 cases
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

rw ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw.fit <- sem(rw.model, data=traindata, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9)
m01
#
x <- unlist(rw.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/45)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rwt.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

rw ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rwt.fit <- sem(rwt.model, data=testdata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)
#
xt <- unlist(rwt.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/45)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rws.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

rw ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rws.fit <- sem(rws.model, data=sset1, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)

fitmeasures(rws.fit, fit.measures = c("ecvi"))

# ======================================================================
#
# MODEL 4 FOR rw
#
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, select = c(LNR, cntry, 
                                  impsafe.x, ipstrgv.x, ipbhprp.x,  
                                  ipfrule.x, imptrad.x, 
                                  imsmetn.x, imdfetn.x, impcntr.x, 
                                  ppltrst.x, pplfair.x, pplhlp.x,
                                  rw, anweight.x.x.x ))

sset1 <- na.omit(sset1) # results in 268234/340215 # 78.84% of initial sample size

set.seed(123)

tr <- sample(268234, 187764)
traindata  <- sset1[tr, ] # results in 187,764 cases
testdata  <- sset1[-tr, ] # results in 80,470 cases
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw.fit <- sem(rw.model, data=traindata, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
# 
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
r10 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
r11 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
r12 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12)
m01
#
x <- unlist(rw.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/78)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rwt.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rwt.fit <- sem(rwt.model, data=testdata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)
#
xt <- unlist(rwt.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/78)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rws.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rws.fit <- sem(rws.model, data=sset1, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)

fitmeasures(rws.fit, fit.measures = c("ecvi"))

# ================================================================================
# -------------------Target  variable rw1 ----------------------------------------
#
# MODEL 1 FOR rw1
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, select = c(LNR, cntry, 
                                  impsafe.x, ipstrgv.x, ipbhprp.x,  
                                  ipfrule.x, imptrad.x, 
                                  imsmetn.x, imdfetn.x, impcntr.x, 
                                  ppltrst.x, pplfair.x, pplhlp.x,
                                  rw1, anweight.x.x.x ))
#
sset1 <- na.omit(sset1) # results in 273,776/340215 # 80.47% of initial sample size
# 273776 * 0.7 =191,643

set.seed(123)
#
tr <- sample(273776, 191643) # 273,776 * 0.7 = 191,643
traindata  <- sset1[tr, ]    # results in 191,643 cases
testdata  <- sset1[-tr, ]    # results in  82,133 cases
#
# ---------------------------------------------------------------------------------

# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw1.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw1.fit <- sem(rw1.model, data=traindata, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
              meanstructure = TRUE)
#
summary(rw1.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0)
r6  <- c(1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6)
m01
#
#
#
x <- unlist(rw1.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw1.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/21)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw1t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw1t.fit <- sem(rw1t.model, data=testdata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
               meanstructure = TRUE)
#
xt <- unlist(rw1t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/21)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw1s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw1s.fit <- sem(rw1s.model, data=sset1, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
               meanstructure = TRUE)

fitmeasures(rw1s.fit, fit.measures = c("ecvi"))

#
# =================================================================================
#
# MODEL 2 FOR rw1
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, select = c(LNR, cntry, 
                                  impsafe.x, ipstrgv.x, ipbhprp.x,  
                                  ipfrule.x, imptrad.x, 
                                  imsmetn.x, imdfetn.x, impcntr.x, 
                                  ppltrst.x, pplfair.x, pplhlp.x,
                                  rw1, anweight.x.x.x ))
#
sset1 <- na.omit(sset1) # results in 273,776/340215 # 80.47% of initial sample size
# 273776 * 0.7 =191,643

set.seed(123)
#
tr <- sample(273776, 191643) # 273,776 * 0.7 = 191,643
traindata  <- sset1[tr, ]    # results in 191,643 cases
testdata  <- sset1[-tr, ]    # results in  82,133 cases
#
# ---------------------------------------------------------------------------------
#
#
library(lavaan)
#
rw1.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw1.fit <- sem(rw1.model, data=traindata, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
              meanstructure = TRUE)
#
summary(rw1.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
#
m01 <- rbind(r1, r2, r3, r4)
m01
#
x <- as.data.frame(rw1.fit@SampleStats@cov)
observed <- x * m01
#
y <- as.data.frame(rw1.fit@implied$cov)
implied <- y * m01
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/10)
TrainingSRMR
#
# same model for testdata to get its observed covariance matrix
#
rw1t.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw1t.fit <- sem(rw1t.model, data=testdata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
               meanstructure = TRUE)
#
xt <- as.data.frame(rw1t.fit@SampleStats@cov)
observedt <- xt * m01
observedt
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/10)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                     from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
rw1s.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw1s.fit <- sem(rw1s.model, data=sset1, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
               meanstructure = TRUE)
#
fitmeasures(rw1s.fit, fit.measures = c("ecvi"))
#
# =================================================================================
#
# MODEL 3 FOR rw1
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, select = c(LNR, cntry, 
                                  impsafe.x, ipstrgv.x, ipbhprp.x,  
                                  ipfrule.x, imptrad.x, 
                                  imsmetn.x, imdfetn.x, impcntr.x, 
                                  ppltrst.x, pplfair.x, pplhlp.x,
                                  rw1, anweight.x.x.x ))
#
sset1 <- na.omit(sset1) # results in 273,776/340215 # 80.47% of initial sample size
# 273776 * 0.7 =191,643

set.seed(123)
#
tr <- sample(273776, 191643) # 273,776 * 0.7 = 191,643
traindata  <- sset1[tr, ]    # results in 191,643 cases
testdata  <- sset1[-tr, ]    # results in  82,133 cases
#
# ---------------------------------------------------------------------------------
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw1.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

rw1 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw1.fit <- sem(rw1.model, data=traindata, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
              meanstructure = TRUE)
#
summary(rw1.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9)
m01
#
x <- unlist(rw1.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw1.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/45)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw1t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

rw1 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw1t.fit <- sem(rw1t.model, data=testdata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
               meanstructure = TRUE)
#
xt <- unlist(rw1t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/45)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw1s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

rw1 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw1s.fit <- sem(rw1s.model, data=sset1, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
               meanstructure = TRUE)

fitmeasures(rw1s.fit, fit.measures = c("ecvi"))

# =================================================================================
#
# MODEL 4 FOR rw1
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, select = c(LNR, cntry, 
                                  impsafe.x, ipstrgv.x, ipbhprp.x,  
                                  ipfrule.x, imptrad.x, 
                                  imsmetn.x, imdfetn.x, impcntr.x, 
                                  ppltrst.x, pplfair.x, pplhlp.x,
                                  rw1, anweight.x.x.x ))
#
sset1 <- na.omit(sset1) # results in 273,776/340215 # 80.47% of initial sample size
# 273776 * 0.7 =191,643

set.seed(123)
#
tr <- sample(273776, 191643) # 273,776 * 0.7 = 191,643
traindata  <- sset1[tr, ]    # results in 191,643 cases
testdata  <- sset1[-tr, ]    # results in  82,133 cases
#
# ---------------------------------------------------------------------------------
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw1.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw1 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw1.fit <- sem(rw1.model, data=traindata, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
              meanstructure = TRUE)
#
summary(rw1.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
# 
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
r10 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
r11 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
r12 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12)
m01
#
x <- unlist(rw1.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw1.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/78)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw1t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw1 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw1t.fit <- sem(rw1t.model, data=testdata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
               meanstructure = TRUE)
#
xt <- unlist(rw1t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/78)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw1s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw1 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw1s.fit <- sem(rw1s.model, data=sset1, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
               meanstructure = TRUE)

fitmeasures(rw1s.fit, fit.measures = c("ecvi"))

# ================================================================================
# -------------------Target  variable rw2 ----------------------------------------
#
# MODEL 1 FOR rw2
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, select = c(LNR, cntry, 
                                  impsafe.x, ipstrgv.x, ipbhprp.x,  
                                  ipfrule.x, imptrad.x, 
                                  imsmetn.x, imdfetn.x, impcntr.x, 
                                  ppltrst.x, pplfair.x, pplhlp.x,
                                  rw2, anweight.x.x.x ))
#
sset1 <- na.omit(sset1) # results in 297,771/340,215 # 87.52% of initial sample size
set.seed(123)
#
tr <- sample(297771, 208440) # 297771 * 0.7 = 208440 
traindata  <- sset1[tr, ]    # results in 208,440 cases
testdata  <- sset1[-tr, ]    # results in  89,331 cases
#
# ---------------------------------------------------------------------------------

# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw2.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw2.fit <- sem(rw2.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
               meanstructure = TRUE)
#
summary(rw2.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0)
r6  <- c(1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6)
m01
#
#
#
x <- unlist(rw2.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw2.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/21)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw2t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw2t.fit <- sem(rw2t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)
#
xt <- unlist(rw2t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/21)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw2s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw2s.fit <- sem(rw2s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)

fitmeasures(rw2s.fit, fit.measures = c("ecvi"))

#
# =================================================================================
#
# MODEL 2 FOR rw2
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, select = c(LNR, cntry, 
                                  impsafe.x, ipstrgv.x, ipbhprp.x,  
                                  ipfrule.x, imptrad.x, 
                                  imsmetn.x, imdfetn.x, impcntr.x, 
                                  ppltrst.x, pplfair.x, pplhlp.x,
                                  rw2, anweight.x.x.x ))
#
sset1 <- na.omit(sset1) # results in 297,771/340,215 # 87.52% of initial sample size
set.seed(123)
#
tr <- sample(297771, 208440) # 297771 * 0.7 = 208440 
traindata  <- sset1[tr, ]    # results in 208,440 cases
testdata  <- sset1[-tr, ]    # results in  89,331 cases
#
# ---------------------------------------------------------------------------------
#
#
library(lavaan)
#
rw2.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw2.fit <- sem(rw2.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
               meanstructure = TRUE)
#
summary(rw2.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
#
m01 <- rbind(r1, r2, r3, r4)
m01
#
x <- as.data.frame(rw2.fit@SampleStats@cov)
observed <- x * m01
#
y <- as.data.frame(rw2.fit@implied$cov)
implied <- y * m01
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/10)
TrainingSRMR
#
# same model for testdata to get its observed covariance matrix
#
rw2t.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw2t.fit <- sem(rw2t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)
#
xt <- as.data.frame(rw2t.fit@SampleStats@cov)
observedt <- xt * m01
observedt
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/10)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                     from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
rw2s.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw2s.fit <- sem(rw2s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)
#
fitmeasures(rw2s.fit, fit.measures = c("ecvi"))
#
# =================================================================================
#
# MODEL 3 FOR rw2
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, select = c(LNR, cntry, 
                                  impsafe.x, ipstrgv.x, ipbhprp.x,  
                                  ipfrule.x, imptrad.x, 
                                  imsmetn.x, imdfetn.x, impcntr.x, 
                                  ppltrst.x, pplfair.x, pplhlp.x,
                                  rw2, anweight.x.x.x ))
#
sset1 <- na.omit(sset1) # results in 297,771/340,215 # 87.52% of initial sample size
set.seed(123)
#
tr <- sample(297771, 208440) # 297771 * 0.7 = 208440 
traindata  <- sset1[tr, ]    # results in 208,440 cases
testdata  <- sset1[-tr, ]    # results in  89,331 cases
#
# ---------------------------------------------------------------------------------
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw2.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

rw2 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw2.fit <- sem(rw2.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
               meanstructure = TRUE)
#
summary(rw2.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9)
m01
#
x <- unlist(rw2.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw2.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/45)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw2t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

rw2 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw2t.fit <- sem(rw2t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)
#
xt <- unlist(rw2t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/45)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw2s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

rw2 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw2s.fit <- sem(rw2s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)

fitmeasures(rw2s.fit, fit.measures = c("ecvi"))

# =================================================================================
#
# MODEL 4 FOR rw2
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, select = c(LNR, cntry, 
                                  impsafe.x, ipstrgv.x, ipbhprp.x,  
                                  ipfrule.x, imptrad.x, 
                                  imsmetn.x, imdfetn.x, impcntr.x, 
                                  ppltrst.x, pplfair.x, pplhlp.x,
                                  rw2, anweight.x.x.x ))
#
sset1 <- na.omit(sset1) # results in 297,771/340,215 # 87.52% of initial sample size
set.seed(123)
#
tr <- sample(297771, 208440) # 297771 * 0.7 = 208440 
traindata  <- sset1[tr, ]    # results in 208,440 cases
testdata  <- sset1[-tr, ]    # results in  89,331 cases
#
#
# ---------------------------------------------------------------------------------
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw2.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw2 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw2.fit <- sem(rw2.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
               meanstructure = TRUE)
#
summary(rw2.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
# 
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
r10 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
r11 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
r12 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12)
m01
#
x <- unlist(rw2.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw2.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/78)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw2t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw2 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw2t.fit <- sem(rw2t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)
#
xt <- unlist(rw2t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/78)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw2s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw2 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw2s.fit <- sem(rw2s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)

fitmeasures(rw2s.fit, fit.measures = c("ecvi"))


# ENDE TESTING BLOCK 3
#
# ##################################################################################
#
# Testing block 4: Temporal validation set design, structural equation models 
# run sequentially # SRMR as criterion
#
# ------------------- SEM in LONGITUDINAL VALIDATION SET DESIGN --------------------
#
# -------------------Target  variable rw  ------------------------------------------
# PERIOD A
#
# MODEL 1 FOR rw
#
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, essround <= 4, select = c(LNR, cntry, essround, 
                                  impsafe.x, ipstrgv.x, ipbhprp.x,  
                                  ipfrule.x, imptrad.x, 
                                  imsmetn.x, imdfetn.x, impcntr.x, 
                                  ppltrst.x, pplfair.x, pplhlp.x,
                                  rw, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 112,133 cases overall 
                                           #   (=training + test sample) 
traindata <-subset(sset1, essround <= 3)    # results in 83,071 cases in rounds 1 to 3.
testdata  <-subset(sset1, essround == 4)    # results in 29,062 in round 4 

# addmargins(table(traindata$essround))
# addmargins(table(testdata$essround))

#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw.fit <- sem(rw.model, data=traindata, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0)
r6  <- c(1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6)
m01
#
#
#
x <- unlist(rw.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/21)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rwt.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rwt.fit <- sem(rwt.model, data=testdata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)
#
xt <- unlist(rwt.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/21)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rws.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rws.fit <- sem(rws.model, data=sset1, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)

fitmeasures(rws.fit, fit.measures = c("ecvi"))

#
# ========================================================================================
#
# MODEL 2 FOR rw    
#
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, essround <= 4, select = c(LNR, cntry, essround, 
                                                 impsafe.x, ipstrgv.x, ipbhprp.x,  
                                                 ipfrule.x, imptrad.x, 
                                                 imsmetn.x, imdfetn.x, impcntr.x, 
                                                 ppltrst.x, pplfair.x, pplhlp.x,
                                                 rw, anweight.x.x.x ))

sset1 <- na.omit(sset1)                         # results in 112,133 cases overall 
#                                                 (=training + test sample) 
traindata <-subset(sset1, essround <= 3)        # results in 83,071 cases in rounds 1 to 3.
testdata  <-subset(sset1, essround == 4)    # results in 29,062 in round 4 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))  
#
library(lavaan)
#
rw.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw.fit <- sem(rw.model, data=traindata, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
#
m01 <- rbind(r1, r2, r3, r4)
m01
#
x <- as.data.frame(rw.fit@SampleStats@cov)
observed <- x * m01
#
y <- as.data.frame(rw.fit@implied$cov)
implied <- y * m01
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/10)
TrainingSRMR
#
# same model for testdata to get its observed covariance matrix
#
rwt.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rwt.fit <- sem(rwt.model, data=testdata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)
#
xt <- as.data.frame(rwt.fit@SampleStats@cov)
observedt <- xt * m01
observedt
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/10)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                     from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
rws.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rws.fit <- sem(rws.model, data=sset1, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)
#
fitmeasures(rws.fit, fit.measures = c("ecvi"))
#
# =================================================================================
#
# MODEL 3 FOR rw
#
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, essround <= 4, select = c(LNR, cntry, essround, 
                                                 impsafe.x, ipstrgv.x, ipbhprp.x,  
                                                 ipfrule.x, imptrad.x, 
                                                 imsmetn.x, imdfetn.x, impcntr.x, 
                                                 ppltrst.x, pplfair.x, pplhlp.x,
                                                 rw, anweight.x.x.x ))

sset1 <- na.omit(sset1)                         # results in 112,133 cases overall 
#                                                 (=training + test sample) 
traindata <-subset(sset1, essround <= 3)        # results in 83,071 cases in rounds 1 to 3.
testdata  <-subset(sset1, essround == 4)    # results in 29,062 in round 4 

# addmargins(table(traindata$essround))
# addmargins(table(testdata$essround)) 
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

rw ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw.fit <- sem(rw.model, data=traindata, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9)
m01
#
x <- unlist(rw.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/45)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rwt.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

rw ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rwt.fit <- sem(rwt.model, data=testdata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)
#
xt <- unlist(rwt.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/45)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rws.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

rw ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rws.fit <- sem(rws.model, data=sset1, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)

fitmeasures(rws.fit, fit.measures = c("ecvi"))

# ========================================================================================
#
# MODEL 4 FOR rw
#
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, essround <= 4, select = c(LNR, cntry, essround, 
                                                 impsafe.x, ipstrgv.x, ipbhprp.x,  
                                                 ipfrule.x, imptrad.x, 
                                                 imsmetn.x, imdfetn.x, impcntr.x, 
                                                 ppltrst.x, pplfair.x, pplhlp.x,
                                                 rw, anweight.x.x.x ))

sset1 <- na.omit(sset1)                         # results in 112,133 cases overall 
#                                                 (=training + test sample) 
traindata <-subset(sset1, essround <= 3)        # results in 83,071 cases in rounds 1 to 3.
testdata  <-subset(sset1, essround == 4)    # results in 29,062 in round 4 

# addmargins(table(traindata$essround))
# addmargins(table(testdata$essround)) 
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw.fit <- sem(rw.model, data=traindata, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
# 
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
r10 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
r11 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
r12 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12)
m01
#
x <- unlist(rw.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/78)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rwt.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rwt.fit <- sem(rwt.model, data=testdata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)
#
xt <- unlist(rwt.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/78)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rws.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rws.fit <- sem(rws.model, data=sset1, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)

fitmeasures(rws.fit, fit.measures = c("ecvi"))

# ================================================================================
# -------------------Target  variable rw1 ----------------------------------------
#
# MODEL 1 FOR rw1
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, essround <= 4, select = c(LNR, cntry, essround, 
                                                 impsafe.x, ipstrgv.x, ipbhprp.x,  
                                                 ipfrule.x, imptrad.x, 
                                                 imsmetn.x, imdfetn.x, impcntr.x, 
                                                 ppltrst.x, pplfair.x, pplhlp.x,
                                                 rw1, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 114,667 cases overall 
#   (=training + test sample) 
traindata <-subset(sset1, essround <= 3)    # results in 85,035 cases in rounds 1 to 3.
testdata  <-subset(sset1, essround == 4)    # results in 29,632 in round 4 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# ---------------------------------------------------------------------------------

# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw1.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw1.fit <- sem(rw1.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
               meanstructure = TRUE)
#
summary(rw1.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0)
r6  <- c(1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6)
m01
#
#
#
x <- unlist(rw1.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw1.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/21)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw1t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw1t.fit <- sem(rw1t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)
#
xt <- unlist(rw1t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/21)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw1s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw1s.fit <- sem(rw1s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)

fitmeasures(rw1s.fit, fit.measures = c("ecvi"))

#
# =================================================================================
#
# MODEL 2 FOR rw1
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, essround <= 4, select = c(LNR, cntry, essround, 
                                                 impsafe.x, ipstrgv.x, ipbhprp.x,  
                                                 ipfrule.x, imptrad.x, 
                                                 imsmetn.x, imdfetn.x, impcntr.x, 
                                                 ppltrst.x, pplfair.x, pplhlp.x,
                                                 rw1, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 114,667 cases overall 
#   (=training + test sample) 
traindata <-subset(sset1, essround <= 3)    # results in 85,035 cases in rounds 1 to 3.
testdata  <-subset(sset1, essround == 4)    # results in 29,632 in round 4 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# ---------------------------------------------------------------------------------
#
#
library(lavaan)
#
rw1.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw1.fit <- sem(rw1.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
               meanstructure = TRUE)
#
summary(rw1.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
#
m01 <- rbind(r1, r2, r3, r4)
m01
#
x <- as.data.frame(rw1.fit@SampleStats@cov)
observed <- x * m01
#
y <- as.data.frame(rw1.fit@implied$cov)
implied <- y * m01
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/10)
TrainingSRMR
#
# same model for testdata to get its observed covariance matrix
#
rw1t.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw1t.fit <- sem(rw1t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)
#
xt <- as.data.frame(rw1t.fit@SampleStats@cov)
observedt <- xt * m01
observedt
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/10)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                     from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
rw1s.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw1s.fit <- sem(rw1s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)
#
fitmeasures(rw1s.fit, fit.measures = c("ecvi"))
#
# =================================================================================
#
# MODEL 3 FOR rw1
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, essround <= 4, select = c(LNR, cntry, essround, 
                                                 impsafe.x, ipstrgv.x, ipbhprp.x,  
                                                 ipfrule.x, imptrad.x, 
                                                 imsmetn.x, imdfetn.x, impcntr.x, 
                                                 ppltrst.x, pplfair.x, pplhlp.x,
                                                 rw1, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 114,667 cases overall 
#   (=training + test sample) 
traindata <-subset(sset1, essround <= 3)    # results in 85,035 cases in rounds 1 to 3.
testdata  <-subset(sset1, essround == 4)    # results in 29,632 in round 4 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
# ---------------------------------------------------------------------------------
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw1.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

rw1 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw1.fit <- sem(rw1.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
               meanstructure = TRUE)
#
summary(rw1.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9)
m01
#
x <- unlist(rw1.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw1.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/45)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw1t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

rw1 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw1t.fit <- sem(rw1t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)
#
xt <- unlist(rw1t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/45)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw1s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

rw1 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw1s.fit <- sem(rw1s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)

fitmeasures(rw1s.fit, fit.measures = c("ecvi"))

# =================================================================================
#
# MODEL 4 FOR rw1
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, essround <= 4, select = c(LNR, cntry, essround, 
                                                 impsafe.x, ipstrgv.x, ipbhprp.x,  
                                                 ipfrule.x, imptrad.x, 
                                                 imsmetn.x, imdfetn.x, impcntr.x, 
                                                 ppltrst.x, pplfair.x, pplhlp.x,
                                                 rw1, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 114,667 cases overall 
#   (=training + test sample) 
traindata <-subset(sset1, essround <= 3)    # results in 85,035 cases in rounds 1 to 3.
testdata  <-subset(sset1, essround == 4)    # results in 29,632 in round 4 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
# ---------------------------------------------------------------------------------
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw1.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw1 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw1.fit <- sem(rw1.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
               meanstructure = TRUE)
#
summary(rw1.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
# 
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
r10 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
r11 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
r12 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12)
m01
#
x <- unlist(rw1.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw1.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/78)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw1t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw1 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw1t.fit <- sem(rw1t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)
#
xt <- unlist(rw1t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/78)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw1s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw1 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw1s.fit <- sem(rw1s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)

fitmeasures(rw1s.fit, fit.measures = c("ecvi"))

# ================================================================================
# -------------------Target  variable rw2 ----------------------------------------
#
# MODEL 1 FOR rw2
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, essround <= 4, select = c(LNR, cntry, essround, 
                                                 impsafe.x, ipstrgv.x, ipbhprp.x,  
                                                 ipfrule.x, imptrad.x, 
                                                 imsmetn.x, imdfetn.x, impcntr.x, 
                                                 ppltrst.x, pplfair.x, pplhlp.x,
                                                 rw2, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 124,726 cases overall 
#   (=training + test sample) 
traindata <-subset(sset1, essround <= 3)    # results in 92,474 cases in rounds 1 to 3.
testdata  <-subset(sset1, essround == 4)    # results in 32,252 in round 4 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
# ---------------------------------------------------------------------------------

# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw2.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw2.fit <- sem(rw2.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
               meanstructure = TRUE)
#
summary(rw2.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0)
r6  <- c(1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6)
m01
#
#
#
x <- unlist(rw2.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw2.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/21)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw2t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw2t.fit <- sem(rw2t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)
#
xt <- unlist(rw2t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/21)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw2s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw2s.fit <- sem(rw2s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)

fitmeasures(rw2s.fit, fit.measures = c("ecvi"))

#
# =================================================================================
# MODEL 2 FOR rw2
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, essround <= 4, select = c(LNR, cntry, essround, 
                                                 impsafe.x, ipstrgv.x, ipbhprp.x,  
                                                 ipfrule.x, imptrad.x, 
                                                 imsmetn.x, imdfetn.x, impcntr.x, 
                                                 ppltrst.x, pplfair.x, pplhlp.x,
                                                 rw2, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 124,726 cases overall 
#   (=training + test sample) 
traindata <-subset(sset1, essround <= 3)    # results in 92,474 cases in rounds 1 to 3.
testdata  <-subset(sset1, essround == 4)    # results in 32,252 in round 4 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# ---------------------------------------------------------------------------------
#
#
library(lavaan)
#
rw2.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw2.fit <- sem(rw2.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
               meanstructure = TRUE)
#
summary(rw2.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
#
m01 <- rbind(r1, r2, r3, r4)
m01
#
x <- as.data.frame(rw2.fit@SampleStats@cov)
observed <- x * m01
#
y <- as.data.frame(rw2.fit@implied$cov)
implied <- y * m01
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/10)
TrainingSRMR
#
# same model for testdata to get its observed covariance matrix
#
rw2t.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw2t.fit <- sem(rw2t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)
#
xt <- as.data.frame(rw2t.fit@SampleStats@cov)
observedt <- xt * m01
observedt
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/10)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                     from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
rw2s.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw2s.fit <- sem(rw2s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)
#
fitmeasures(rw2s.fit, fit.measures = c("ecvi"))
#
# =================================================================================
#
# MODEL 3 FOR rw2
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, essround <= 4, select = c(LNR, cntry, essround, 
                                                 impsafe.x, ipstrgv.x, ipbhprp.x,  
                                                 ipfrule.x, imptrad.x, 
                                                 imsmetn.x, imdfetn.x, impcntr.x, 
                                                 ppltrst.x, pplfair.x, pplhlp.x,
                                                 rw2, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 124,726 cases overall 
#   (=training + test sample) 
traindata <-subset(sset1, essround <= 3)    # results in 92,474 cases in rounds 1 to 3.
testdata  <-subset(sset1, essround == 4)    # results in 32,252 in round 4 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# ---------------------------------------------------------------------------------
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw2.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

rw2 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw2.fit <- sem(rw2.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
               meanstructure = TRUE)
#
summary(rw2.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9)
m01
#
x <- unlist(rw2.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw2.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/45)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw2t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

rw2 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw2t.fit <- sem(rw2t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)
#
xt <- unlist(rw2t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/45)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw2s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

rw2 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw2s.fit <- sem(rw2s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)

fitmeasures(rw2s.fit, fit.measures = c("ecvi"))

# ========================================================================================
#
# MODEL 4 FOR rw2
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, essround <= 4, select = c(LNR, cntry, essround, 
                                                 impsafe.x, ipstrgv.x, ipbhprp.x,  
                                                 ipfrule.x, imptrad.x, 
                                                 imsmetn.x, imdfetn.x, impcntr.x, 
                                                 ppltrst.x, pplfair.x, pplhlp.x,
                                                 rw2, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 124,726 cases overall 
#   (=training + test sample) 
traindata <-subset(sset1, essround <= 3)    # results in 92,474 cases in rounds 1 to 3.
testdata  <-subset(sset1, essround == 4)    # results in 32,252 in round 4 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# ---------------------------------------------------------------------------------
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw2.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw2 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw2.fit <- sem(rw2.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
               meanstructure = TRUE)
#
summary(rw2.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
# 
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
r10 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
r11 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
r12 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12)
m01
#
x <- unlist(rw2.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw2.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/78)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw2t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw2 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw2t.fit <- sem(rw2t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)
#
xt <- unlist(rw2t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/78)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw2s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw2 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw2s.fit <- sem(rw2s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)

fitmeasures(rw2s.fit, fit.measures = c("ecvi"))


# ENDE TESTING BLOCK 4
# ================================================================================
#
# Testing block 5: Temporal validation set design, structural equation models 
# run sequentially # SRMR as criterion
#
# ------------------- SEM in LONGITUDINAL VALIDATION SET DESIGN ---------------
#
# -------------------Target  variable rw  -------------------------------------
# PERIOD B 
#
# MODEL 1 FOR rw
#
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 4 | essround == 5 | essround == 6 ),  
                        select = c(LNR, cntry, essround, 
                                                 impsafe.x, ipstrgv.x, ipbhprp.x,  
                                                 ipfrule.x, imptrad.x, 
                                                 imsmetn.x, imdfetn.x, impcntr.x, 
                                                 ppltrst.x, pplfair.x, pplhlp.x,
                                                 rw, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 92,278 cases overall (=training + test sample)
#
traindata <-subset(sset1, (essround == 4 | essround == 5 ))    # results in 59,183 cases in rounds 4 and 5.
testdata  <-subset(sset1,  essround == 6)    # results in 33,095 cases in round 4 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw.fit <- sem(rw.model, data=traindata, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0)
r6  <- c(1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6)
m01
#
#
#
x <- unlist(rw.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/21)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rwt.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rwt.fit <- sem(rwt.model, data=testdata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)
#
xt <- unlist(rwt.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/21)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rws.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rws.fit <- sem(rws.model, data=sset1, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)

fitmeasures(rws.fit, fit.measures = c("ecvi"))

#
# ========================================================================================
#
# MODEL 2 FOR rw    
#
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 4 | essround == 5 | essround == 6 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 92,278 cases overall (=training + test sample)
#
traindata <-subset(sset1, (essround == 4 | essround == 5 ))    # results in 59,183 cases in rounds 4 and 5.
testdata  <-subset(sset1,  essround == 6)    # results in 33,095 cases in round 4 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround)) 
#
library(lavaan)
#
rw.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw.fit <- sem(rw.model, data=traindata, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
#
m01 <- rbind(r1, r2, r3, r4)
m01
#
x <- as.data.frame(rw.fit@SampleStats@cov)
observed <- x * m01
#
y <- as.data.frame(rw.fit@implied$cov)
implied <- y * m01
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/10)
TrainingSRMR
#
# same model for testdata to get its observed covariance matrix
#
rwt.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rwt.fit <- sem(rwt.model, data=testdata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)
#
xt <- as.data.frame(rwt.fit@SampleStats@cov)
observedt <- xt * m01
observedt
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/10)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                     from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
rws.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rws.fit <- sem(rws.model, data=sset1, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)
#
fitmeasures(rws.fit, fit.measures = c("ecvi"))
#
# =================================================================================
#
# MODEL 3 FOR rw
#
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 4 | essround == 5 | essround == 6 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 92,278 cases overall (=training + test sample)
#
traindata <-subset(sset1, (essround == 4 | essround == 5 ))    # results in 59,183 cases in rounds 4 and 5.
testdata  <-subset(sset1,  essround == 6)    # results in 33,095 cases in round 4 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

rw ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw.fit <- sem(rw.model, data=traindata, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9)
m01
#
x <- unlist(rw.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/45)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rwt.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

rw ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rwt.fit <- sem(rwt.model, data=testdata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)
#
xt <- unlist(rwt.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/45)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rws.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

rw ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rws.fit <- sem(rws.model, data=sset1, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)

fitmeasures(rws.fit, fit.measures = c("ecvi"))

# ========================================================================================
#
# MODEL 4 FOR rw
#
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 4 | essround == 5 | essround == 6 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 92,278 cases overall (=training + test sample)
#
traindata <-subset(sset1, (essround == 4 | essround == 5 ))    # results in 59,183 cases in rounds 4 and 5.
testdata  <-subset(sset1,  essround == 6)    # results in 33,095 cases in round 4 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw.fit <- sem(rw.model, data=traindata, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
# 
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
r10 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
r11 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
r12 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12)
m01
#
x <- unlist(rw.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/78)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rwt.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rwt.fit <- sem(rwt.model, data=testdata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)
#
xt <- unlist(rwt.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/78)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rws.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rws.fit <- sem(rws.model, data=sset1, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)

fitmeasures(rws.fit, fit.measures = c("ecvi"))

# ================================================================================ 
# -------------------Target  variable rw1 ----------------------------------------
#
# MODEL 1 FOR rw1
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 4 | essround == 5 | essround == 6 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw1, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 94,015 cases overall (=training + test sample)
#
traindata <-subset(sset1, (essround == 4 | essround == 5 ))    # results in 60,307 cases in rounds 4 and 5.
testdata  <-subset(sset1,  essround == 6)    # results in 33,708 cases in round 4 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# ---------------------------------------------------------------------------------

# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw1.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw1.fit <- sem(rw1.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
               meanstructure = TRUE)
#
summary(rw1.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0)
r6  <- c(1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6)
m01
#
#
#
x <- unlist(rw1.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw1.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/21)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw1t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw1t.fit <- sem(rw1t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)
#
xt <- unlist(rw1t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/21)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw1s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw1s.fit <- sem(rw1s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)

fitmeasures(rw1s.fit, fit.measures = c("ecvi"))

#
# =================================================================================
#
# MODEL 2 FOR rw1
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 4 | essround == 5 | essround == 6 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw1, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 94,015 cases overall (=training + test sample)
#
traindata <-subset(sset1, (essround == 4 | essround == 5 ))    # results in 60,307 cases in rounds 4 and 5.
testdata  <-subset(sset1,  essround == 6)    # results in 33,708 cases in round 4 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# ---------------------------------------------------------------------------------
#
#
library(lavaan)
#
rw1.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw1.fit <- sem(rw1.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
               meanstructure = TRUE)
#
summary(rw1.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
#
m01 <- rbind(r1, r2, r3, r4)
m01
#
x <- as.data.frame(rw1.fit@SampleStats@cov)
observed <- x * m01
#
y <- as.data.frame(rw1.fit@implied$cov)
implied <- y * m01
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/10)
TrainingSRMR
#
# same model for testdata to get its observed covariance matrix
#
rw1t.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw1t.fit <- sem(rw1t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)
#
xt <- as.data.frame(rw1t.fit@SampleStats@cov)
observedt <- xt * m01
observedt
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/10)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                     from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
rw1s.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw1s.fit <- sem(rw1s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)
#
fitmeasures(rw1s.fit, fit.measures = c("ecvi"))
#
# =================================================================================
#
# MODEL 3 FOR rw1
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 4 | essround == 5 | essround == 6 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw1, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 94,015 cases overall (=training + test sample)
#
traindata <-subset(sset1, (essround == 4 | essround == 5 ))    # results in 60,307 cases in rounds 4 and 5.
testdata  <-subset(sset1,  essround == 6)    # results in 33,708 cases in round 4 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
# ---------------------------------------------------------------------------------
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw1.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

rw1 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw1.fit <- sem(rw1.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
               meanstructure = TRUE)
#
summary(rw1.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9)
m01
#
x <- unlist(rw1.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw1.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/45)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw1t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

rw1 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw1t.fit <- sem(rw1t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)
#
xt <- unlist(rw1t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/45)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw1s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

rw1 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw1s.fit <- sem(rw1s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)

fitmeasures(rw1s.fit, fit.measures = c("ecvi"))

# ========================================================================================
#
# MODEL 4 FOR rw1
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 4 | essround == 5 | essround == 6 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw1, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 94,015 cases overall (=training + test sample)
#
traindata <-subset(sset1, (essround == 4 | essround == 5 ))    # results in 60,307 cases in rounds 4 and 5.
testdata  <-subset(sset1,  essround == 6)    # results in 33,708 cases in round 4 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
# ---------------------------------------------------------------------------------
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw1.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw1 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw1.fit <- sem(rw1.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
               meanstructure = TRUE)
#
summary(rw1.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
# 
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
r10 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
r11 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
r12 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12)
m01
#
x <- unlist(rw1.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw1.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/78)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw1t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw1 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw1t.fit <- sem(rw1t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)
#
xt <- unlist(rw1t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/78)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw1s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw1 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw1s.fit <- sem(rw1s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)

fitmeasures(rw1s.fit, fit.measures = c("ecvi"))

# ================================================================================
# -------------------Target  variable rw2 ----------------------------------------
#
# MODEL 1 FOR rw2
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 4 | essround == 5 | essround == 6 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw2, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 102,694 cases overall (=training + test sample)
#
traindata <-subset(sset1, (essround == 4 | essround == 5 ))    # results in 65,886 cases in rounds 4 and 5.
testdata  <-subset(sset1,  essround == 6)    # results in 36,808 cases in round 4 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
# ---------------------------------------------------------------------------------

# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw2.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw2.fit <- sem(rw2.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
               meanstructure = TRUE)
#
summary(rw2.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0)
r6  <- c(1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6)
m01
#
#
#
x <- unlist(rw2.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw2.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/21)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw2t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw2t.fit <- sem(rw2t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)
#
xt <- unlist(rw2t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/21)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw2s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw2s.fit <- sem(rw2s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)

fitmeasures(rw2s.fit, fit.measures = c("ecvi"))

#
# ========================================================================================
#
# MODEL 2 FOR rw2
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 4 | essround == 5 | essround == 6 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw2, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 102,694 cases overall (=training + test sample)
#
traindata <-subset(sset1, (essround == 4 | essround == 5 ))    # results in 65,886 cases in rounds 4 and 5.
testdata  <-subset(sset1,  essround == 6)    # results in 36,808 cases in round 4 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# ---------------------------------------------------------------------------------
#
#
library(lavaan)
#
rw2.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw2.fit <- sem(rw2.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
               meanstructure = TRUE)
#
summary(rw2.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
#
m01 <- rbind(r1, r2, r3, r4)
m01
#
x <- as.data.frame(rw2.fit@SampleStats@cov)
observed <- x * m01
#
y <- as.data.frame(rw2.fit@implied$cov)
implied <- y * m01
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/10)
TrainingSRMR
#
# same model for testdata to get its observed covariance matrix
#
rw2t.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw2t.fit <- sem(rw2t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)
#
xt <- as.data.frame(rw2t.fit@SampleStats@cov)
observedt <- xt * m01
observedt
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/10)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                     from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
rw2s.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw2s.fit <- sem(rw2s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)
#
fitmeasures(rw2s.fit, fit.measures = c("ecvi"))
#
# =================================================================================
#
# MODEL 3 FOR rw2
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 4 | essround == 5 | essround == 6 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw2, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 102,694 cases overall (=training + test sample)
#
traindata <-subset(sset1, (essround == 4 | essround == 5 ))    # results in 65,886 cases in rounds 4 and 5.
testdata  <-subset(sset1,  essround == 6)    # results in 36,808 cases in round 4 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# ---------------------------------------------------------------------------------
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw2.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

rw2 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw2.fit <- sem(rw2.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
               meanstructure = TRUE)
#
summary(rw2.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9)
m01
#
x <- unlist(rw2.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw2.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/45)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw2t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

rw2 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw2t.fit <- sem(rw2t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)
#
xt <- unlist(rw2t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/45)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw2s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

rw2 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw2s.fit <- sem(rw2s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)

fitmeasures(rw2s.fit, fit.measures = c("ecvi"))

# ========================================================================================
#
# MODEL 4 FOR rw2
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 4 | essround == 5 | essround == 6 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw2, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 102,694 cases overall (=training + test sample)
#
traindata <-subset(sset1, (essround == 4 | essround == 5 ))    # results in 65,886 cases in rounds 4 and 5.
testdata  <-subset(sset1,  essround == 6)    # results in 36,808 cases in round 4  

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# ---------------------------------------------------------------------------------
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw2.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw2 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw2.fit <- sem(rw2.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
               meanstructure = TRUE)
#
summary(rw2.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
# 
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
r10 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
r11 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
r12 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12)
m01
#
x <- unlist(rw2.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw2.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/78)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw2t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw2 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw2t.fit <- sem(rw2t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)
#
xt <- unlist(rw2t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/78)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw2s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw2 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw2s.fit <- sem(rw2s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)

fitmeasures(rw2s.fit, fit.measures = c("ecvi"))


# ENDE TESTING BLOCK 5
# ================================================================================
#
# Testing block 6: Temporal validation set design, structural equation models 
# run sequentially # SRMR as criterion
#
# ------------------- SEM in LONGITUDINAL VALIDATION SET DESIGN ------------------
#
# -------------------Target  variable rw  ----------------------------------------
# PERIOD C 
#
# MODEL 1 FOR rw
#
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 6 | essround == 7 | essround == 8 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 94,939 cases overall (=training + test sample)
#
traindata <-subset(sset1, (essround == 6 | essround == 7 ))    # results in 63,660 cases in rounds 6 and 7.
testdata  <-subset(sset1,  essround == 8)    # results in 31,279 cases in round 8 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw.fit <- sem(rw.model, data=traindata, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0)
r6  <- c(1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6)
m01
#
#
#
x <- unlist(rw.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/21)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rwt.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rwt.fit <- sem(rwt.model, data=testdata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)
#
xt <- unlist(rwt.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/21)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rws.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rws.fit <- sem(rws.model, data=sset1, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)

fitmeasures(rws.fit, fit.measures = c("ecvi"))

#
# ========================================================================================
#
# MODEL 2 FOR rw    
#
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 6 | essround == 7 | essround == 8 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 94,939 cases overall (=training + test sample)
#
traindata <-subset(sset1, (essround == 6 | essround == 7 ))    # results in 63,660 cases in rounds 6 and 7.
testdata  <-subset(sset1,  essround == 8)    # results in 31,279 cases in round 8 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround)) 
#
library(lavaan)
#
rw.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw.fit <- sem(rw.model, data=traindata, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
#
m01 <- rbind(r1, r2, r3, r4)
m01
#
x <- as.data.frame(rw.fit@SampleStats@cov)
observed <- x * m01
#
y <- as.data.frame(rw.fit@implied$cov)
implied <- y * m01
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/10)
TrainingSRMR
#
# same model for testdata to get its observed covariance matrix
#
rwt.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rwt.fit <- sem(rwt.model, data=testdata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)
#
xt <- as.data.frame(rwt.fit@SampleStats@cov)
observedt <- xt * m01
observedt
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/10)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                     from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
rws.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rws.fit <- sem(rws.model, data=sset1, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)
#
fitmeasures(rws.fit, fit.measures = c("ecvi"))
#
# =================================================================================
#
# MODEL 3 FOR rw
#
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 6 | essround == 7 | essround == 8 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 94,939 cases overall (=training + test sample)
#
traindata <-subset(sset1, (essround == 6 | essround == 7 ))    # results in 63,660 cases in rounds 6 and 7.
testdata  <-subset(sset1,  essround == 8)    # results in 31,279 cases in round 8 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

rw ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw.fit <- sem(rw.model, data=traindata, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9)
m01
#
x <- unlist(rw.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/45)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rwt.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

rw ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rwt.fit <- sem(rwt.model, data=testdata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)
#
xt <- unlist(rwt.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/45)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rws.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

rw ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rws.fit <- sem(rws.model, data=sset1, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)

fitmeasures(rws.fit, fit.measures = c("ecvi"))

# ========================================================================================
#
# MODEL 4 FOR rw
#
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 6 | essround == 7 | essround == 8 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 94,939 cases overall (=training + test sample)
#
traindata <-subset(sset1, (essround == 6 | essround == 7 ))    # results in 63,660 cases in rounds 6 and 7.
testdata  <-subset(sset1,  essround == 8)    # results in 31,279 cases in round 8 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw.fit <- sem(rw.model, data=traindata, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
# 
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
r10 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
r11 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
r12 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12)
m01
#
x <- unlist(rw.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/78)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rwt.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rwt.fit <- sem(rwt.model, data=testdata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)
#
xt <- unlist(rwt.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/78)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rws.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rws.fit <- sem(rws.model, data=sset1, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)

fitmeasures(rws.fit, fit.measures = c("ecvi"))

# ================================================================================ 
# -------------------Target  variable rw1 ----------------------------------------
#
# MODEL 1 FOR rw1
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 6 | essround == 7 | essround == 8 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw1, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 96,787 cases overall (=training + test sample)
#
traindata <-subset(sset1, (essround == 6 | essround == 7 ))    # results in 64,849 cases in rounds 6 and 7.
testdata  <-subset(sset1,  essround == 8)    # results in 31,938 cases in round 8 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# ---------------------------------------------------------------------------------

# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw1.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw1.fit <- sem(rw1.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
               meanstructure = TRUE)
#
summary(rw1.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0)
r6  <- c(1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6)
m01
#
#
#
x <- unlist(rw1.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw1.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/21)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw1t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw1t.fit <- sem(rw1t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)
#
xt <- unlist(rw1t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/21)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw1s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw1s.fit <- sem(rw1s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)

fitmeasures(rw1s.fit, fit.measures = c("ecvi"))

#
# ========================================================================================
#
# MODEL 2 FOR rw1
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 6 | essround == 7 | essround == 8 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw1, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 96,787 cases overall (=training + test sample)
#
traindata <-subset(sset1, (essround == 6 | essround == 7 ))    # results in 64,849 cases in rounds 6 and 7.
testdata  <-subset(sset1,  essround == 8)    # results in 31,938 cases in round 8 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# ---------------------------------------------------------------------------------
#
#
library(lavaan)
#
rw1.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw1.fit <- sem(rw1.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
               meanstructure = TRUE)
#
summary(rw1.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
#
m01 <- rbind(r1, r2, r3, r4)
m01
#
x <- as.data.frame(rw1.fit@SampleStats@cov)
observed <- x * m01
#
y <- as.data.frame(rw1.fit@implied$cov)
implied <- y * m01
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/10)
TrainingSRMR
#
# same model for testdata to get its observed covariance matrix
#
rw1t.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw1t.fit <- sem(rw1t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)
#
xt <- as.data.frame(rw1t.fit@SampleStats@cov)
observedt <- xt * m01
observedt
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/10)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                     from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
rw1s.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw1s.fit <- sem(rw1s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)
#
fitmeasures(rw1s.fit, fit.measures = c("ecvi"))
#
# =================================================================================
#
# MODEL 3 FOR rw1
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 6 | essround == 7 | essround == 8 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw1, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 96,787 cases overall (=training + test sample)
#
traindata <-subset(sset1, (essround == 6 | essround == 7 ))    # results in 64,849 cases in rounds 6 and 7.
testdata  <-subset(sset1,  essround == 8)    # results in 31,938 cases in round 8 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
# ---------------------------------------------------------------------------------
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw1.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

rw1 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw1.fit <- sem(rw1.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
               meanstructure = TRUE)
#
summary(rw1.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9)
m01
#
x <- unlist(rw1.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw1.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/45)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw1t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

rw1 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw1t.fit <- sem(rw1t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)
#
xt <- unlist(rw1t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/45)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw1s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

rw1 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw1s.fit <- sem(rw1s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)

fitmeasures(rw1s.fit, fit.measures = c("ecvi"))

# ========================================================================================
#
# MODEL 4 FOR rw1
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 6 | essround == 7 | essround == 8 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw1, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 96,787 cases overall (=training + test sample)
#
traindata <-subset(sset1, (essround == 6 | essround == 7 ))    # results in 64,849 cases in rounds 6 and 7.
testdata  <-subset(sset1,  essround == 8)    # results in 31,938 cases in round 8 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
# ---------------------------------------------------------------------------------
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw1.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw1 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw1.fit <- sem(rw1.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
               meanstructure = TRUE)
#
summary(rw1.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
# 
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
r10 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
r11 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
r12 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12)
m01
#
x <- unlist(rw1.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw1.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/78)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw1t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw1 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw1t.fit <- sem(rw1t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)
#
xt <- unlist(rw1t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/78)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw1s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw1 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw1s.fit <- sem(rw1s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)

fitmeasures(rw1s.fit, fit.measures = c("ecvi"))

# ================================================================================
# -------------------Target  variable rw2 ----------------------------------------
#
# MODEL 1 FOR rw2
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 6 | essround == 7 | essround == 8 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw2, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 105,184 cases overall (=training + test sample)
#
traindata <-subset(sset1, (essround == 6 | essround == 7 ))    # results in 70,393 cases in rounds 6 and 7.
testdata  <-subset(sset1,  essround == 8)    # results in 34,791 cases in round 8 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
# ---------------------------------------------------------------------------------

# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw2.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw2.fit <- sem(rw2.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
               meanstructure = TRUE)
#
summary(rw2.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0)
r6  <- c(1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6)
m01
#
#
#
x <- unlist(rw2.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw2.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/21)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw2t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw2t.fit <- sem(rw2t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)
#
xt <- unlist(rw2t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/21)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw2s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw2s.fit <- sem(rw2s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)

fitmeasures(rw2s.fit, fit.measures = c("ecvi"))

#
# ========================================================================================
#
# MODEL 2 FOR rw2
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 6 | essround == 7 | essround == 8 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw2, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 105,184 cases overall (=training + test sample)
#
traindata <-subset(sset1, (essround == 6 | essround == 7 ))    # results in 70,393 cases in rounds 6 and 7.
testdata  <-subset(sset1,  essround == 8)    # results in 34,791 cases in round 8 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# ---------------------------------------------------------------------------------
#
#
library(lavaan)
#
rw2.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw2.fit <- sem(rw2.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
               meanstructure = TRUE)
#
summary(rw2.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
#
m01 <- rbind(r1, r2, r3, r4)
m01
#
x <- as.data.frame(rw2.fit@SampleStats@cov)
observed <- x * m01
#
y <- as.data.frame(rw2.fit@implied$cov)
implied <- y * m01
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/10)
TrainingSRMR
#
# same model for testdata to get its observed covariance matrix
#
rw2t.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw2t.fit <- sem(rw2t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)
#
xt <- as.data.frame(rw2t.fit@SampleStats@cov)
observedt <- xt * m01
observedt
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/10)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                     from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
rw2s.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw2s.fit <- sem(rw2s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)
#
fitmeasures(rw2s.fit, fit.measures = c("ecvi"))
#
# =================================================================================
#
# MODEL 3 FOR rw2
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 6 | essround == 7 | essround == 8 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw2, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 105,184 cases overall (=training + test sample)
#
traindata <-subset(sset1, (essround == 6 | essround == 7 ))    # results in 70,393 cases in rounds 6 and 7.
testdata  <-subset(sset1,  essround == 8)    # results in 34,791 cases in round 8 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# ---------------------------------------------------------------------------------
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw2.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

rw2 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw2.fit <- sem(rw2.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
               meanstructure = TRUE)
#
summary(rw2.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9)
m01
#
x <- unlist(rw2.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw2.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/45)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw2t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

rw2 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw2t.fit <- sem(rw2t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)
#
xt <- unlist(rw2t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/45)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw2s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

rw2 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw2s.fit <- sem(rw2s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)

fitmeasures(rw2s.fit, fit.measures = c("ecvi"))

# ========================================================================================
#
# MODEL 4 FOR rw2
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 6 | essround == 7 | essround == 8 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw2, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 105,184 cases overall (=training + test sample)
#
traindata <-subset(sset1, (essround == 6 | essround == 7 ))    # results in 70,393 cases in rounds 6 and 7.
testdata  <-subset(sset1,  essround == 8)    # results in 34,791 cases in round 8 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# ---------------------------------------------------------------------------------
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw2.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw2 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw2.fit <- sem(rw2.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
               meanstructure = TRUE)
#
summary(rw2.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
# 
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
r10 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
r11 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
r12 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12)
m01
#
x <- unlist(rw2.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw2.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/78)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw2t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw2 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw2t.fit <- sem(rw2t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)
#
xt <- unlist(rw2t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/78)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw2s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw2 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw2s.fit <- sem(rw2s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)

fitmeasures(rw2s.fit, fit.measures = c("ecvi"))


# ENDE TESTING BLOCK 6

# ================================================================================
#
# Testing block 7: Temporal validation set design, structural equation models 
# run sequentially # SRMR as criterion
#
# ------------------- SEM in LONGITUDINAL VALIDATION SET DESIGN ------------------
#
# -------------------Target  variable rw  ----------------------------------------  
# PERIOD D 
#
# MODEL 1 FOR rw
#
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 8 | essround == 9 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 62,320 cases overall (=training + test sample)
#
traindata <-subset(sset1, essround == 8 )    # results in 31,279 cases in round 8
testdata  <-subset(sset1, essround == 9 )    # results in 31,041 cases in round 9 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw.fit <- sem(rw.model, data=traindata, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0)
r6  <- c(1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6)
m01
#
#
#
x <- unlist(rw.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/21)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rwt.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rwt.fit <- sem(rwt.model, data=testdata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)
#
xt <- unlist(rwt.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/21)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rws.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rws.fit <- sem(rws.model, data=sset1, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)

fitmeasures(rws.fit, fit.measures = c("ecvi"))

#
# ========================================================================================
#
# MODEL 2 FOR rw    
#
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 8 | essround == 9 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 62,320 cases overall (=training + test sample)
#
traindata <-subset(sset1, essround == 8 )    # results in 31,279 cases in round 8
testdata  <-subset(sset1, essround == 9 )    # results in 31,041 cases in round 9 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround)) 
#
library(lavaan)
#
rw.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw.fit <- sem(rw.model, data=traindata, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
#
m01 <- rbind(r1, r2, r3, r4)
m01
#
x <- as.data.frame(rw.fit@SampleStats@cov)
observed <- x * m01
#
y <- as.data.frame(rw.fit@implied$cov)
implied <- y * m01
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/10)
TrainingSRMR
#
# same model for testdata to get its observed covariance matrix
#
rwt.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rwt.fit <- sem(rwt.model, data=testdata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)
#
xt <- as.data.frame(rwt.fit@SampleStats@cov)
observedt <- xt * m01
observedt
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/10)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                     from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
rws.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rws.fit <- sem(rws.model, data=sset1, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)
#
fitmeasures(rws.fit, fit.measures = c("ecvi"))
#
# =================================================================================
#
# MODEL 3 FOR rw
#
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 8 | essround == 9 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 62,320 cases overall (=training + test sample)
#
traindata <-subset(sset1, essround == 8 )    # results in 31,279 cases in round 8
testdata  <-subset(sset1, essround == 9 )    # results in 31,041 cases in round 9 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

rw ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw.fit <- sem(rw.model, data=traindata, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9)
m01
#
x <- unlist(rw.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/45)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rwt.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

rw ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rwt.fit <- sem(rwt.model, data=testdata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)
#
xt <- unlist(rwt.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/45)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rws.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

rw ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rws.fit <- sem(rws.model, data=sset1, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)

fitmeasures(rws.fit, fit.measures = c("ecvi"))

# ========================================================================================
#
# MODEL 4 FOR rw
#
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 8 | essround == 9 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 62,320 cases overall (=training + test sample)
#
traindata <-subset(sset1, essround == 8 )    # results in 31,279 cases in round 8
testdata  <-subset(sset1, essround == 9 )    # results in 31,041 cases in round 9 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw.fit <- sem(rw.model, data=traindata, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
# 
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
r10 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
r11 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
r12 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12)
m01
#
x <- unlist(rw.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/78)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rwt.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rwt.fit <- sem(rwt.model, data=testdata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)
#
xt <- unlist(rwt.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/78)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rws.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rws.fit <- sem(rws.model, data=sset1, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
               meanstructure = TRUE)

fitmeasures(rws.fit, fit.measures = c("ecvi"))

# ================================================================================ 
# -------------------Target  variable rw1 ----------------------------------------
#
# MODEL 1 FOR rw1
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 8 | essround == 9 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw1, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 63,585 cases overall (=training + test sample)
#
traindata <-subset(sset1, essround == 8 )    # results in 31,938 cases in round 8
testdata  <-subset(sset1, essround == 9 )    # results in 31,647 cases in round 9 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# ---------------------------------------------------------------------------------

# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw1.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw1.fit <- sem(rw1.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
               meanstructure = TRUE)
#
summary(rw1.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0)
r6  <- c(1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6)
m01
#
#
#
x <- unlist(rw1.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw1.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/21)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw1t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw1t.fit <- sem(rw1t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)
#
xt <- unlist(rw1t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/21)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw1s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw1s.fit <- sem(rw1s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)

fitmeasures(rw1s.fit, fit.measures = c("ecvi"))

#
# ========================================================================================
#
# MODEL 2 FOR rw1
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 8 | essround == 9 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw1, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 63,585 cases overall (=training + test sample)
#
traindata <-subset(sset1, essround == 8 )    # results in 31,938 cases in round 8
testdata  <-subset(sset1, essround == 9 )    # results in 31,647 cases in round 9 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# ---------------------------------------------------------------------------------
#
#
library(lavaan)
#
rw1.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw1.fit <- sem(rw1.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
               meanstructure = TRUE)
#
summary(rw1.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
#
m01 <- rbind(r1, r2, r3, r4)
m01
#
x <- as.data.frame(rw1.fit@SampleStats@cov)
observed <- x * m01
#
y <- as.data.frame(rw1.fit@implied$cov)
implied <- y * m01
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/10)
TrainingSRMR
#
# same model for testdata to get its observed covariance matrix
#
rw1t.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw1t.fit <- sem(rw1t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)
#
xt <- as.data.frame(rw1t.fit@SampleStats@cov)
observedt <- xt * m01
observedt
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/10)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                     from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
rw1s.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw1s.fit <- sem(rw1s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)
#
fitmeasures(rw1s.fit, fit.measures = c("ecvi"))
#
# =================================================================================
#
# MODEL 3 FOR rw1
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 8 | essround == 9 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw1, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 63,585 cases overall (=training + test sample)
#
traindata <-subset(sset1, essround == 8 )    # results in 31,938 cases in round 8
testdata  <-subset(sset1, essround == 9 )    # results in 31,647 cases in round 9 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
# ---------------------------------------------------------------------------------
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw1.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

rw1 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw1.fit <- sem(rw1.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
               meanstructure = TRUE)
#
summary(rw1.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9)
m01
#
x <- unlist(rw1.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw1.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/45)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw1t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

rw1 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw1t.fit <- sem(rw1t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)
#
xt <- unlist(rw1t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/45)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw1s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

rw1 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw1s.fit <- sem(rw1s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)

fitmeasures(rw1s.fit, fit.measures = c("ecvi"))

# ========================================================================================
#
# MODEL 4 FOR rw1
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 8 | essround == 9 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw1, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 63,585 cases overall (=training + test sample)
#
traindata <-subset(sset1, essround == 8 )    # results in 31,938 cases in round 8
testdata  <-subset(sset1, essround == 9 )    # results in 31,647 cases in round 9 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
# ---------------------------------------------------------------------------------
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw1.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw1 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw1.fit <- sem(rw1.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
               meanstructure = TRUE)
#
summary(rw1.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
# 
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
r10 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
r11 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
r12 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12)
m01
#
x <- unlist(rw1.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw1.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/78)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw1t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw1 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw1t.fit <- sem(rw1t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)
#
xt <- unlist(rw1t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/78)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw1s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw1 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw1s.fit <- sem(rw1s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
                meanstructure = TRUE)

fitmeasures(rw1s.fit, fit.measures = c("ecvi"))

# ================================================================================
# -------------------Target  variable rw2 ----------------------------------------
#
# MODEL 1 FOR rw2
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 8 | essround == 9 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw2, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 69,018 cases overall (=training + test sample)
#
traindata <-subset(sset1, essround == 8 )    # results in 34,791 cases in round 8
testdata  <-subset(sset1, essround == 9 )    # results in 34,227 cases in round 9 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
# ---------------------------------------------------------------------------------

# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw2.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw2.fit <- sem(rw2.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
               meanstructure = TRUE)
#
summary(rw2.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0)
r6  <- c(1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6)
m01
#
#
#
x <- unlist(rw2.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw2.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/21)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw2t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw2t.fit <- sem(rw2t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)
#
xt <- unlist(rw2t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/21)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw2s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw2s.fit <- sem(rw2s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)

fitmeasures(rw2s.fit, fit.measures = c("ecvi"))

#
# ========================================================================================
#
# MODEL 2 FOR rw2
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 8 | essround == 9 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw2, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 69,018 cases overall (=training + test sample)
#
traindata <-subset(sset1, essround == 8 )    # results in 34,791 cases in round 8
testdata  <-subset(sset1, essround == 9 )    # results in 34,227 cases in round 9 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# ---------------------------------------------------------------------------------
#
#
library(lavaan)
#
rw2.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw2.fit <- sem(rw2.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
               meanstructure = TRUE)
#
summary(rw2.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0)
#
m01 <- rbind(r1, r2, r3, r4)
m01
#
x <- as.data.frame(rw2.fit@SampleStats@cov)
observed <- x * m01
#
y <- as.data.frame(rw2.fit@implied$cov)
implied <- y * m01
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/10)
TrainingSRMR
#
# same model for testdata to get its observed covariance matrix
#
rw2t.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw2t.fit <- sem(rw2t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)
#
xt <- as.data.frame(rw2t.fit@SampleStats@cov)
observedt <- xt * m01
observedt
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/10)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                     from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
rw2s.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  

'
rw2s.fit <- sem(rw2s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)
#
fitmeasures(rw2s.fit, fit.measures = c("ecvi"))
#
# =================================================================================
#
# MODEL 3 FOR rw2
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 8 | essround == 9 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw2, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 69,018 cases overall (=training + test sample)
#
traindata <-subset(sset1, essround == 8 )    # results in 34,791 cases in round 8
testdata  <-subset(sset1, essround == 9 )    # results in 34,227 cases in round 9 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# ---------------------------------------------------------------------------------
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw2.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

rw2 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw2.fit <- sem(rw2.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
               meanstructure = TRUE)
#
summary(rw2.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
#
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
#
m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9)
m01
#
x <- unlist(rw2.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw2.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/45)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw2t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

rw2 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw2t.fit <- sem(rw2t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)
#
xt <- unlist(rw2t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/45)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw2s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

rw2 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw2s.fit <- sem(rw2s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)

fitmeasures(rw2s.fit, fit.measures = c("ecvi"))

# ========================================================================================
#
# MODEL 4 FOR rw2
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19, (essround == 8 | essround == 9 ),  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw2, anweight.x.x.x ))

sset1 <- na.omit(sset1)                    # results in 69,018 cases overall (=training + test sample)
#
traindata <-subset(sset1, essround == 8 )    # results in 34,791 cases in round 8
testdata  <-subset(sset1, essround == 9 )    # results in 34,227 cases in round 9 

addmargins(table(traindata$essround))
addmargins(table(testdata$essround))
#
# ---------------------------------------------------------------------------------
#
# scale direction reversed for value items
#
library(car)
#
traindata$impsafer <- car::recode(traindata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipstrgvr <- car::recode(traindata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipbhprpr <- car::recode(traindata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$ipfruler <- car::recode(traindata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
traindata$imptradr <- car::recode(traindata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw2.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw2 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw2.fit <- sem(rw2.model, data=traindata, sampling.weights="anweight.x.x.x",
               ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
               meanstructure = TRUE)
#
summary(rw2.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
# 
r1  <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r2  <- c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r3  <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
r4  <- c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
r5  <- c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
r6  <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
r7  <- c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
r8  <- c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
r9  <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
r10 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
r11 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
r12 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

m01 <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12)
m01
#
x <- unlist(rw2.fit@SampleStats@cov)
obs <- x * m01
observed <- cov2cor(obs)
#
y <- unlist(rw2.fit@implied$cov)
impl <- y * m01
implied <- cov2cor(impl)
implied
#
TrainingSRMR <- sqrt(sum((observed - implied)^2)/78)
TrainingSRMR

#
# same model for testdata to get its observed covariance matrix
#
#
library(car)
#
testdata$impsafer <- car::recode(testdata$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipstrgvr <- car::recode(testdata$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipbhprpr <- car::recode(testdata$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$ipfruler <- car::recode(testdata$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
testdata$imptradr <- car::recode(testdata$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
rw2t.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw2 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw2t.fit <- sem(rw2t.model, data=testdata, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)
#
xt <- unlist(rw2t.fit@SampleStats@cov)
obst <- xt * m01
observedt <- cov2cor(obst)
observedt
#
#
TestSRMR <- sqrt(sum((observedt - implied)^2)/78)
TestSRMR # relates implied covariance matrix from traindata to observed covariance 
#                                                    from testdata
#
#
# same model for sset1 (=traindata + testdata combined) to compute ECVI
#
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')

rw2s.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw2 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw2s.fit <- sem(rw2s.model, data=sset1, sampling.weights="anweight.x.x.x",
                ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
                meanstructure = TRUE)

fitmeasures(rw2s.fit, fit.measures = c("ecvi"))


# ENDE TESTING BLOCK 7
# ================================================================================
#
# Block 8: structural equation models 
# run sequentially (without possibility of out-of-sample testing (last available round))
#
#
# -------------------Target  variable rw  ----------------------------------------  
# PERIOD E 
#
# MODEL 1 FOR rw
#
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19,  essround == 9 ,  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw, anweight.x.x.x ))

sset1 <- na.omit(sset1)     # results in  31,041 cases 
#
addmargins(table(sset1$essround))
#
# scale direction reversed for value items
#
library(car)
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw.fit <- sem(rw.model, data=sset1, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
fitmeasures(rw.fit, fit.measures = c("ecvi"))
#
# ========================================================================================
#
# MODEL 2 FOR rw    
#
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19,  essround == 9 ,  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw, anweight.x.x.x ))

sset1 <- na.omit(sset1)     # results in  31,041 cases 
#
addmargins(table(sset1$essround))
#
library(lavaan)
#
rw.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw.fit <- sem(rw.model, data=sset1, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
fitmeasures(rw.fit, fit.measures = c("ecvi"))
#
# =================================================================================
#
# MODEL 3 FOR rw
#
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19,  essround == 9 ,  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw, anweight.x.x.x ))

sset1 <- na.omit(sset1)     # results in  31,041 cases 
#
addmargins(table(sset1$essround))
#
# scale direction reversed for value items
#
library(car)
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

rw ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw.fit <- sem(rw.model, data=sset1, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
fitmeasures(rw.fit, fit.measures = c("ecvi"))

# ========================================================================================
#
# MODEL 4 FOR rw
#
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19,  essround == 9 ,  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw, anweight.x.x.x ))

sset1 <- na.omit(sset1)     # results in  31,041 cases 
#
addmargins(table(sset1$essround))
#
# scale direction reversed for value items
#
library(car)
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw ~ authoritarian
# rw ~ prejudice

# rw ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw.fit <- sem(rw.model, data=sset1, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
fitmeasures(rw.fit, fit.measures = c("ecvi"))

# ================================================================================ 
# -------------------Target  variable rw1 ----------------------------------------
#
# MODEL 1 FOR rw1
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19,  essround == 9 ,  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw1, anweight.x.x.x ))

sset1 <- na.omit(sset1)     # results in  31,647 cases 
#
addmargins(table(sset1$essround))
#
# scale direction reversed for value items
#
library(car)
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw.fit <- sem(rw.model, data=sset1, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
fitmeasures(rw.fit, fit.measures = c("ecvi"))

#
# ========================================================================================
#
# MODEL 2 FOR rw1
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19,  essround == 9 ,  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw1, anweight.x.x.x ))

sset1 <- na.omit(sset1)     # results in  31,647 cases 
#
addmargins(table(sset1$essround))
#
#
library(lavaan)
#
rw.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw.fit <- sem(rw.model, data=sset1, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
fitmeasures(rw.fit, fit.measures = c("ecvi"))
#
# =================================================================================
#
# MODEL 3 FOR rw1
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19,  essround == 9 ,  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw1, anweight.x.x.x ))

sset1 <- na.omit(sset1)     # results in  31,647 cases 
#
addmargins(table(sset1$essround))
# ---------------------------------------------------------------------------------
#
# scale direction reversed for value items
#
library(car)
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

rw1 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw1 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw.fit <- sem(rw.model, data=sset1, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
fitmeasures(rw.fit, fit.measures = c("ecvi"))

# ========================================================================================
#
# MODEL 4 FOR rw1
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19,  essround == 9 ,  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw1, anweight.x.x.x ))

sset1 <- na.omit(sset1)     # results in  31,647 cases 
#
addmargins(table(sset1$essround))
# ---------------------------------------------------------------------------------
#
# scale direction reversed for value items
#
library(car)
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw1 ~ authoritarian
# rw1 ~ prejudice

# rw1 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw1 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw.fit <- sem(rw.model, data=sset1, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw1"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
fitmeasures(rw.fit, fit.measures = c("ecvi"))

# ================================================================================
# -------------------Target  variable rw2 ----------------------------------------
#
# MODEL 1 FOR rw2
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19,  essround == 9 ,  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw2, anweight.x.x.x ))

sset1 <- na.omit(sset1)     # results in  34,227 cases 
#
addmargins(table(sset1$essround))
# ---------------------------------------------------------------------------------

# scale direction reversed for value items
#
library(car)
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

# prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw.fit <- sem(rw.model, data=sset1, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
fitmeasures(rw.fit, fit.measures = c("ecvi"))

#
# ========================================================================================
#
# MODEL 2 FOR rw2
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19,  essround == 9 ,  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw2, anweight.x.x.x ))

sset1 <- na.omit(sset1)     # results in  34,227 cases 
#
addmargins(table(sset1$essround))
#
# ---------------------------------------------------------------------------------
#
#
library(lavaan)
#
rw.model <- '

# authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice  
#
'
rw.fit <- sem(rw.model, data=sset1, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
fitmeasures(rw.fit, fit.measures = c("ecvi"))
#
# =================================================================================
#
# MODEL 3 FOR rw2
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19,  essround == 9 ,  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw2, anweight.x.x.x ))

sset1 <- na.omit(sset1)     # results in  34,227 cases 
#
addmargins(table(sset1$essround))
#
# ---------------------------------------------------------------------------------
#
# scale direction reversed for value items
#
library(car)
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

# trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

rw2 ~ authoritarian + prejudice
prejudice ~ authoritarian

# rw2 ~ authoritarian + prejudice + trust
# prejudice ~ authoritarian
# trust ~ prejudice

'
rw.fit <- sem(rw.model, data=sset1, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
fitmeasures(rw.fit, fit.measures = c("ecvi"))
# ========================================================================================
#
# MODEL 4 FOR rw2
#
# ---------------------------------------------------------------------------------
rm(list = ls())
load("ess19.Rdata")
ess19$LNR <- seq_len(340215)
sset1 <- subset(ess19,  essround == 9 ,  
                select = c(LNR, cntry, essround, 
                           impsafe.x, ipstrgv.x, ipbhprp.x,  
                           ipfrule.x, imptrad.x, 
                           imsmetn.x, imdfetn.x, impcntr.x, 
                           ppltrst.x, pplfair.x, pplhlp.x,
                           rw2, anweight.x.x.x ))

sset1 <- na.omit(sset1)     # results in  34,227 cases 
#
addmargins(table(sset1$essround))
#
# ---------------------------------------------------------------------------------
#
# scale direction reversed for value items
#
library(car)
#
sset1$impsafer <- car::recode(sset1$impsafe, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipstrgvr <- car::recode(sset1$ipstrgv, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipbhprpr <- car::recode(sset1$ipbhprp, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$ipfruler <- car::recode(sset1$ipfrule, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
sset1$imptradr <- car::recode(sset1$imptrad, recodes = '1=6;2=5;3=4;4=3;5=2;6=1')
#
library(lavaan)
#
rw.model <- '

authoritarian =~ impsafer + ipstrgvr + ipbhprpr + ipfruler + imptradr  

prejudice =~ imsmetn.x + imdfetn.x + impcntr.x

trust =~ ppltrst.x + pplfair.x + pplhlp.x

# rw2 ~ authoritarian
# rw2 ~ prejudice

# rw2 ~ authoritarian + prejudice
# prejudice ~ authoritarian

rw2 ~ authoritarian + prejudice + trust
prejudice ~ authoritarian
trust ~ prejudice  

'
rw.fit <- sem(rw.model, data=sset1, sampling.weights="anweight.x.x.x",
              ordered=c("imsmetn.x", "imdfetn.x", "impcntr.x", "rw2"),
              meanstructure = TRUE)
#
summary(rw.fit, header = TRUE, fit.measures = TRUE, 
        estimates = TRUE, standardized = TRUE, rsquare=TRUE,
        modindices = FALSE)
fitmeasures(rw.fit, fit.measures = c("ecvi"))


# END BLOCK 8 for ROUND 9 
# ================================================================================

