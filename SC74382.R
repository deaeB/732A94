## KB 13.5p D
# liuid <- donni508


#### q1 start ####
## KB 3p
# a) 
## KB 3p
# depends: will be loaded and attached
# import: will be loaded without attaching
# suggest: recommended but not obligatory
# 
# b)
## KB 0p
# use
# devtools::install_github()
# to access the package from github
# then load it when in need

#### q1 end ####


#### q2 start ####

## KB 4.5p
## KB a 2p
## KB b 1p you create a unique id, but then check if medicine is present and add, the  for (i in 1:amount_in)  loop seems completely wrong 
## KB c 1.5p amount not reduced by dosage_out, 0 amount medicine not removed
## KB d 0p 

cabinet <- setRefClass("cabinet",
                       fields = list(
                         max_number = "numeric",
                         id = "numeric",
                         name = "character",
                         symptons = "list",
                         max_dosage = "numeric",
                         last_usage = "numeric",
                         # last usage time
                         freq_usage = "numeric",
                         # fter how much time the medication can be used again
                         number = "numeric"
                         # how much of it is stored
                       ),
                       methods = list(
                         add_medication_to_cabinet = function(name_in, symp_in, maxDosage_in, freq_in, amount_in){
                           # five parameters: the text name of the
                           # substance, a text vector of symptoms it can be applied for, maximal dosage, how often it can
                           # be used, and the amount of it
                           
                           input_check <- is.character(name_in) && is.character(symp_in) && is.numeric(maxDosage_in) && is.numeric(freq_in) && is.numeric(amount_in)
                           stopifnot(input_check)
                           
                           for (i in 1:amount_in) {
                             if (length(id) >= max_number) {
                               base::print("full")
                               amount_in <- i - 1
                               break
                             }
                             
                             name <<- append(name, name_in)
                             if (length(id) > 0) {
                               id <<- append(id, max(id) + 1)
                             } else {
                               id <<- append(id, 1)
                             }
                             symptons[[length(id)]] <<- symp_in
                             max_dosage <<- append(max_dosage, maxDosage_in)
                             freq_usage <<- append(freq_usage, freq_in)
                             last_usage <<- append(last_usage, 0)
                             
                           }
                           
                           for (i in 1:length(id)) {
                             if (name[i] == name_in) {
                               if (!is.na(my_cabinet$number[i])) {
                                number[i] <<- number[i] + amount_in
                               } else{
                                number[i] <<- amount_in
                               }
                             }
                           }
                           # didn't seen "and how much of it is stored" at the beginning so the code may be extremely twisted
                           
                         },
                         dispense_medication = function(symptom, date_out, dosage_out){
                           
                           input_check <- is.character(symptom) && is.numeric(date_out) && is.numeric(dosage_out)
                           stopifnot(input_check)
                           
                           dosage_out_input <- dosage_out
                           
                           count <- 0
                           for (i in 1:length(id)) {
                             if (any(symptons[[i]] == symptom)) {
                               name_out <- name[i]
                               count <- count + 1 ##KB what is is this ? 
                               if (max_dosage[i] < dosage_out) {
                                 base::print("overdose")
                                 stop()
                               }
                              # if the requested dosage exceeds the maximal one
                               if (last_usage[i] + freq_usage[i] > date_out) {
                                 base::print("too often")
                                 stop()
                               }
                               # if not enough time has passed between requests
                             }
                           }
                             if (count < dosage_out) {
                               base::print("more than we have")
                               stop()
                             }
                             # there are not enough doses in the cabinet
                           # loop for check
                           
                           for (i in length(id):1) {
                             if (any(symptons[[i]] == symptom)){
                               if (dosage_out > 0) {
                                 id <<- id[-i]
                                 name <<- name[-i]
                                 symptons <<- symptons[-i]
                                 max_dosage <<- max_dosage[-i]
                                 freq_usage <<- freq_usage[-i]
                                 last_usage <<- last_usage[-i]
                                 number <<- number[-i]
                                 dosage_out <- dosage_out - 1 ##KB why by 1 ?
                               } else{
                                last_usage[i] <<- date_out
                               }
                             }
                           }
                           
                           for (i in 1:length(id)) {
                             if (name[i] == name_out) {
                                 number[i] <<- number[i] - dosage_out_input
                               
                             }
                           }
                           
                         },
                         print = function(){
                           print.data.frame(data.frame(id, name, max_dosage, last_usage, freq_usage, number))
                           base::print("use print_sympton(id) to check corresponding symtons ")
                         },
                         print_sympton = function(x){
                           stopifnot( is.numeric(x))
                           
                           base::print(my_cabinet$symptons[[which(id == x)]])
                         }
                         
                       )
                       
                       )
# 
# # add_medication_to_cabinet = function(name_in, symp_in, maxDosage_in, freq_in, amount_in)
# # dispense_medication = function(symptom, date_out, dosage_out)
# 
 my_cabinet <- cabinet$new(max_number=100)
 my_cabinet$add_medication_to_cabinet("onion_sugar_syrup",c("cough","sneezing"),2,1,5)
 my_cabinet$add_medication_to_cabinet("potato_syrup",c("a","b"),10,10,5)
 my_cabinet$dispense_medication("cough", 1, 1)
 my_cabinet$print()
# 
 my_cabinet$dispense_medication("b", 1, 1)
# # error # if not enough time has passed between requests
# 
 my_cabinet$dispense_medication("b", 1, 15)
# # error # if the requested dosage exceeds the maximal one
# 
 my_cabinet$dispense_medication("b", 11, 7)
# # error # # there are not enough doses in the cabinet
# 
 my_cabinet$add_medication_to_cabinet("illiotiacirum_syrup",c("c","d"),2,1,105)
# # cabinet full
#

#### q2 end ####


#### q3 start ####
## KB 6p
# a)

find_singleton<-function(v){
  if (!is.integer(v)){stop("Only integers can provided, e.g. c(1L,2L,2L)")}
  
  for (i in 1:length(v)) {
    flag_single <- FALSE
    tv <- v[-i]
    for (j in 1:length(tv)) {
      flag_single <- flag_single || v[i] == tv[j]
    }
    if (!flag_single) {
      print(v[i])
      return(v[i])
      break
    }
  }
}

# b) T(n) = n * (n -1) = O(n^2)
# c) T(n) = O(n)

#d)
f_findsingleton<-function(v){
  if (!is.integer(v)){stop("Only integers can provided, e.g. c(1L,2L,2L)")}
  x<-0L
  for (i in v){x<-bitwXor(x,i)}
  return(x)
}

testthat::test_that("compares your implementation with the f findsingleton()",{
  v1 <- c(1L:4L, 1L:3L)
  v2 <- c(1L:4L, 1L:5L)
  testthat::expect_equal(find_singleton(v1), f_findsingleton(v1))
  testthat::expect_equal(find_singleton(v2), f_findsingleton(v2))
}
                    )




#### q3 end ####
