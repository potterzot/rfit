ae <- function(hr, pa, adj = 0) { (1609.34/ (pa + adj)) / hr}
aet <- function(hr1, pa1, hr2, pa2, adj = 0) {
  ae1 <- ae(hr1, pa1, adj = -adj)
  ae2 <- ae(hr2, pa2, adj = adj)
  (ae1 - ae2)/ae1
}

ae1 <- ae(140, 8*60+38, -20)
ae2 <- ae(142, 7*60+53, 20)
aet(.76*180, (8*60)+6, .76*180, (8*60)+23, 0)

(1609.34/(8*60+38 - 20)) / 140 
(1609.34/(7*60+53 + 20)) / 142

pace1 <- ( (7*60 + 36) + (7*60 + 53) + (8*60 + 06) + (8*60+15) ) / 4
pace2 <- ( (8*60 + 03) + (7*60 + 47) + (7*60 + 34) + (8*60+21) ) / 4
aet(141, pace1, 142, pace2, 5)
aet(140, 8*60+9, 140, 7*60+51, 20)

aet(.76*180, (8*60)+6, .76*180, (8*60)+23, 0)
aet(.78*180, 8*60, .79*180, 7*60+48, 30)


(1609.34/(8*60+38 - 20)) / 140 
(1609.34/(8*60+38 - 20)) / 145 




#' Calculate body density from 3-point skinfold caliper measures.
#' 
body_density <- function(mm, age) {
  1.10938 - (0.0008267 * mm) + (0.0000016 * mm^2) - (0.0002574 * age)
}

#' Calculate body fat percentage from body density measure.
body_fat <- function(mm, age) {
  bd <- body_density(mm, age)
  4.95/bd - 4.5
}

body_fat(65, 41)
body_fat(58, 41)
body_fat(57, 41)
body_fat(54, 41)
body_fat(35, 41)

18.6*177/100
